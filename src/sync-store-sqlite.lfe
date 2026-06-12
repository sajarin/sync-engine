;;;; sync-store-sqlite -- persistent sync-store backed by SQLite (esqlite).
;;;;
;;;; Implements the sync-store behaviour. The connection is opened once
;;;; by (init Path) and kept in persistent_term. Path may be a file
;;;; (e.g. a LiteFS-mounted db) or ":memory:".
;;;;
;;;; Concurrency note: SQLite serialises internally, but for strict
;;;; ordering all access to one scope should be funnelled through that
;;;; scope's gen_server (see sync-scope, planned).

(defmodule sync-store-sqlite
  (behaviour sync-store)
  (export
   (init 1)
   (close 0)
   (append-changes 2)
   (read-since 3)
   (current-version 1)
   (seen-ids 2)
   (get-cursor 2)
   (put-cursor 3)
   (list-cursors 1)
   (count-changes 1)))

(include-lib "sync_engine/include/sync-records.lfe")

;;; --- connection ------------------------------------------------------

(defun pt-key () #(sync-store-sqlite conn))

(defun conn () (persistent_term:get (pt-key)))

(defun schema ()
  (lists:append
   (list
    "CREATE TABLE IF NOT EXISTS changes ("
    "  scope TEXT NOT NULL, id TEXT NOT NULL,"
    "  entity_type TEXT, entity_id TEXT, op TEXT,"
    "  payload TEXT, version INTEGER NOT NULL,"
    "  device_id TEXT, timestamp TEXT,"
    "  PRIMARY KEY (scope, id));"
    "CREATE INDEX IF NOT EXISTS idx_changes_scope_version"
    "  ON changes (scope, version);"
    "CREATE TABLE IF NOT EXISTS cursors ("
    "  scope TEXT NOT NULL, device_id TEXT NOT NULL,"
    "  cursor INTEGER NOT NULL,"
    "  PRIMARY KEY (scope, device_id));")))

(defun init (path)
  "Open the database at PATH, create the schema, stash the connection."
  (case (esqlite3:open path)
    ((tuple 'ok c)
     (esqlite3:exec c (schema))
     (persistent_term:put (pt-key) c)
     'ok)
    ((tuple 'error reason)
     (error (tuple 'sqlite-open-failed reason)))))

(defun close ()
  (case (persistent_term:get (pt-key) 'undefined)
    ('undefined 'ok)
    (c (esqlite3:close c)
       (persistent_term:erase (pt-key))
       'ok)))

;;; --- payload (de)serialisation --------------------------------------
;;; Opaque payloads are stored as JSON text so the db stays inspectable.

(defun encode-payload
  ((p) (when (=:= p 'undefined)) 'undefined)
  ((p) (erlang:iolist_to_binary (json:encode p))))

(defun decode-payload
  ((p) (when (=:= p 'undefined)) 'undefined)
  ((p) (when (=:= p 'null))      'undefined)
  ((p) (json:decode p)))

(defun row->change
  (((list id scope etype eid op payload version did ts))
   (make-change
    id          id
    scope       scope
    entity-type etype
    entity-id   eid
    op          op
    payload     (decode-payload payload)
    version     version
    device-id   did
    timestamp   ts)))

;;; --- sync-store callbacks -------------------------------------------

(defun append-changes (scope changes)
  (let ((c (conn)))
    (lists:foreach
     (lambda (ch)
       (esqlite3:q
        c
        #"INSERT OR IGNORE INTO changes (scope,id,entity_type,entity_id,op,payload,version,device_id,timestamp) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9)"
        (list scope
              (change-id ch)
              (change-entity-type ch)
              (change-entity-id ch)
              (change-op ch)
              (encode-payload (change-payload ch))
              (change-version ch)
              (change-device-id ch)
              (change-timestamp ch))))
     changes)
    'ok))

(defun read-since (scope cursor limit)
  ;; Fetch limit+1 rows so has-more is known without a second query.
  (let* ((rows (esqlite3:q
                (conn)
                #"SELECT id,scope,entity_type,entity_id,op,payload,version,device_id,timestamp FROM changes WHERE scope=?1 AND version>?2 ORDER BY version ASC LIMIT ?3"
                (list scope cursor (+ limit 1))))
         (all  (lists:map (lambda (r) (row->change r)) rows))
         (has-more (> (length all) limit))
         (page (lists:sublist all limit))
         (next (case page
                 ('() cursor)
                 (_   (change-version (lists:last page))))))
    (tuple page next has-more)))

(defun current-version (scope)
  (case (esqlite3:q
         (conn)
         #"SELECT COALESCE(MAX(version),0) FROM changes WHERE scope=?1"
         (list scope))
    ((list (list v)) v)
    (_ 0)))

(defun seen-ids (scope ids)
  (let ((c (conn)))
    (lists:filter
     (lambda (id)
       (=/= '()
            (esqlite3:q
             c
             #"SELECT 1 FROM changes WHERE scope=?1 AND id=?2 LIMIT 1"
             (list scope id))))
     ids)))

(defun get-cursor (scope device-id)
  (case (esqlite3:q
         (conn)
         #"SELECT cursor FROM cursors WHERE scope=?1 AND device_id=?2"
         (list scope device-id))
    ((list (list v)) v)
    (_ 0)))

(defun put-cursor (scope device-id cursor)
  (esqlite3:q
   (conn)
   #"INSERT INTO cursors (scope,device_id,cursor) VALUES (?1,?2,?3) ON CONFLICT(scope,device_id) DO UPDATE SET cursor=excluded.cursor"
   (list scope device-id cursor))
  'ok)

;;; --- observability --------------------------------------------------

(defun list-cursors (scope)
  "All device cursors for SCOPE -> [#(DeviceId Cursor)]."
  (lists:map
   (lambda (r)
     (case r ((list did cur) (tuple did cur))))
   (esqlite3:q
    (conn)
    #"SELECT device_id, cursor FROM cursors WHERE scope=?1 ORDER BY device_id"
    (list scope))))

(defun count-changes (scope)
  "Number of stored changes for SCOPE."
  (case (esqlite3:q
         (conn)
         #"SELECT COUNT(*) FROM changes WHERE scope=?1"
         (list scope))
    ((list (list n)) n)
    (_ 0)))
