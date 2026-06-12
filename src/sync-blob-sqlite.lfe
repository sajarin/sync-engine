;;;; sync-blob-sqlite -- SQLite-backed content-addressed blob store.
;;;;
;;;; Implements the sync-blob behaviour. Shares the SQLite connection
;;;; opened by sync-store-sqlite:init/1 (same db file / persistent_term
;;;; key) so an embedding app runs one database. Call (ensure-schema)
;;;; once after sync-store-sqlite:init to create the blob tables.
;;;;
;;;; Tables:
;;;;   blobs(scope, hash, content_type, size_bytes, data, PK(scope,hash))
;;;;     content-addressed bytes; hash = lowercase hex sha256(data).
;;;;   blob_refs(scope, entity_type, entity_id, hash, ord, meta,
;;;;             PK(scope,entity_type,entity_id,hash))
;;;;     the manifest: which hashes (and in what order, with what per-ref
;;;;     metadata) make up an entity. meta is opaque JSON text.

(defmodule sync-blob-sqlite
  (behaviour sync-blob)
  (export
   (ensure-schema 0)
   (put-blob 4)
   (get-blob 2)
   (have-blobs 2)
   (put-manifest 4)
   (get-manifest 3)
   (blob-stats 1)
   (gc-unreferenced 1)
   (sha256-hex 1)))

;;; --- connection (shared with sync-store-sqlite) ---------------------

(defun conn () (persistent_term:get #(sync-store-sqlite conn)))

(defun ensure-schema ()
  "Create the blob tables. Idempotent; call once after the store is open."
  (esqlite3:exec
   (conn)
   (lists:append
    (list
     "CREATE TABLE IF NOT EXISTS blobs ("
     "  scope TEXT NOT NULL, hash TEXT NOT NULL,"
     "  content_type TEXT, size_bytes INTEGER NOT NULL,"
     "  data BLOB NOT NULL,"
     "  PRIMARY KEY (scope, hash));"
     "CREATE TABLE IF NOT EXISTS blob_refs ("
     "  scope TEXT NOT NULL, entity_type TEXT NOT NULL,"
     "  entity_id TEXT NOT NULL, hash TEXT NOT NULL,"
     "  ord INTEGER NOT NULL DEFAULT 0, meta TEXT,"
     "  PRIMARY KEY (scope, entity_type, entity_id, hash));"
     "CREATE INDEX IF NOT EXISTS idx_blob_refs_entity"
     "  ON blob_refs (scope, entity_type, entity_id);"
     "CREATE INDEX IF NOT EXISTS idx_blob_refs_hash"
     "  ON blob_refs (scope, hash);")))
  'ok)

;;; --- content addressing ---------------------------------------------

(defun sha256-hex (bytes)
  "Lowercase hex sha256 -- the canonical blob hash (matches WebCrypto)."
  (string:lowercase (binary:encode_hex (crypto:hash 'sha256 bytes))))

;;; --- sync-blob callbacks --------------------------------------------

(defun put-blob (scope hash content-type bytes)
  "Store BYTES under HASH after verifying HASH = sha256(BYTES). Idempotent:
   a re-put of identical content is a no-op (content can't differ for a
   given hash, so we leave the existing row)."
  (case (=:= (sha256-hex bytes) (normalize-hash hash))
    ('false (tuple 'error 'hash-mismatch))
    ('true
     (esqlite3:q
      (conn)
      #"INSERT OR IGNORE INTO blobs (scope,hash,content_type,size_bytes,data) VALUES (?1,?2,?3,?4,?5)"
      (list scope (normalize-hash hash)
            (or-null content-type) (byte_size bytes) bytes))
     'ok)))

(defun get-blob (scope hash)
  (case (esqlite3:q
         (conn)
         #"SELECT content_type, data FROM blobs WHERE scope=?1 AND hash=?2"
         (list scope (normalize-hash hash)))
    ((cons (list ctype data) _) (tuple 'ok ctype data))
    (_ 'not-found)))

(defun have-blobs (scope hashes)
  "Subset of HASHES already stored for SCOPE. One query, not N."
  (case hashes
    ('() '())
    (_
     (let* ((norm  (lists:map (lambda (h) (normalize-hash h)) hashes))
            (place (placeholders (length norm) 2))
            (sql   (iolist_to_binary
                    (list #"SELECT hash FROM blobs WHERE scope=?1 AND hash IN ("
                          place #")")))
            (rows  (esqlite3:q (conn) sql (cons scope norm))))
       (lists:map (lambda (r) (case r ((list h) h))) rows)))))

(defun put-manifest (scope entity-type entity-id refs)
  "Replace-all the manifest for (entity-type, entity-id). REFS is a list of
   maps #m(\"hash\" .. \"ord\" .. \"meta\" ..)."
  (let ((c (conn)))
    (esqlite3:q
     c
     #"DELETE FROM blob_refs WHERE scope=?1 AND entity_type=?2 AND entity_id=?3"
     (list scope entity-type entity-id))
    (lists:foreach
     (lambda (ref)
       (esqlite3:q
        c
        #"INSERT OR REPLACE INTO blob_refs (scope,entity_type,entity_id,hash,ord,meta) VALUES (?1,?2,?3,?4,?5,?6)"
        (list scope entity-type entity-id
              (normalize-hash (maps:get #"hash" ref))
              (maps:get #"ord" ref 0)
              (encode-meta (maps:get #"meta" ref 'undefined)))))
     refs)
    'ok))

(defun get-manifest (scope entity-type entity-id)
  "The manifest rows for an entity, joined to blob presence/size/type.
   `present` is 1 when the bytes are in the store, 0 when only the ref
   exists (a dangling manifest entry)."
  (lists:map
   (lambda (r)
     (case r
       ((list hash ord meta ctype size present)
        (map #"hash"        hash
             #"ord"         ord
             #"meta"        (decode-meta meta)
             #"contentType" (nv ctype)
             #"size"        (nv size)
             #"present"     (=:= present 1)))))
   (esqlite3:q
    (conn)
    #"SELECT r.hash, r.ord, r.meta, b.content_type, b.size_bytes,
             (b.hash IS NOT NULL) AS present
        FROM blob_refs r
        LEFT JOIN blobs b ON b.scope=r.scope AND b.hash=r.hash
       WHERE r.scope=?1 AND r.entity_type=?2 AND r.entity_id=?3
       ORDER BY r.ord ASC, r.hash ASC"
    (list scope entity-type entity-id))))

(defun blob-stats (scope)
  (case (esqlite3:q
         (conn)
         #"SELECT COUNT(*), COALESCE(SUM(size_bytes),0) FROM blobs WHERE scope=?1"
         (list scope))
    ((list (list n total)) (map #"count" n #"totalBytes" total))
    (_ (map #"count" 0 #"totalBytes" 0))))

(defun gc-unreferenced (scope)
  "Delete blobs no manifest references. Returns the count removed."
  (let* ((before (count-blobs scope)))
    (esqlite3:q
     (conn)
     #"DELETE FROM blobs WHERE scope=?1 AND hash NOT IN (SELECT hash FROM blob_refs WHERE scope=?1)"
     (list scope))
    (- before (count-blobs scope))))

;;; --- helpers --------------------------------------------------------

(defun count-blobs (scope)
  (case (esqlite3:q (conn) #"SELECT COUNT(*) FROM blobs WHERE scope=?1" (list scope))
    ((list (list n)) n)
    (_ 0)))

(defun normalize-hash (h)
  "Hashes are compared lowercase; tolerate a client sending uppercase hex."
  (if (is_binary h) (string:lowercase h) h))

(defun or-null (x) (if (=:= x 'undefined) 'null x))
(defun nv (x) (if (=:= x 'undefined) 'null x))

(defun encode-meta
  ((m) (when (=:= m 'undefined)) 'null)
  ((m) (when (=:= m 'null))      'null)
  ((m) (erlang:iolist_to_binary (json:encode m))))

(defun decode-meta
  ((m) (when (=:= m 'undefined)) 'null)
  ((m) (when (=:= m 'null))      'null)
  ((m) (try (json:decode m) (catch ((tuple _ _ _) m)))))

(defun placeholders (n start)
  "Comma-joined SQL placeholders ?START..?(START+N-1) for an IN clause."
  (let ((nums (lists:seq start (+ start (- n 1)))))
    (join-comma
     (lists:map (lambda (i) (iolist_to_binary (list #"?" (integer_to_binary i)))) nums))))

(defun join-comma
  (('()) #"")
  (((cons h t))
   (lists:foldl
    (lambda (x acc) (iolist_to_binary (list acc #"," x)))
    h t)))
