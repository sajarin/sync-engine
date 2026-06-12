;;;; sync-http-blob -- Cowboy handler for the content-addressed blob API.
;;;;
;;;; Routes (all scope-isolated; scope from the auth token):
;;;;   PUT  /sync/blobs/:hash    raw body = bytes, Content-Type header kept.
;;;;                             Stores iff sha256(body) == :hash.
;;;;   GET  /sync/blobs/:hash    -> raw bytes (or 404).
;;;;   POST /sync/blobs/check    body #m("hashes" [..]) -> #m("have" [..]).
;;;;                             One round-trip to learn which blobs the
;;;;                             server already has, so a client uploads
;;;;                             only the missing ones (idempotent + resumable).
;;;;
;;;; State map: #m('blob BlobModule 'auth AuthModule).

(defmodule sync-http-blob
  (export (init 2)))

(defun init (req0 state)
  (let ((method (cowboy_req:method req0))
        (hash   (cowboy_req:binding 'hash req0 'undefined)))
    (case (tuple method hash)
      ((tuple #"GET" 'undefined) (bad req0 state 404 #"not found"))
      ((tuple #"GET"  h) (handle-get req0 state h))
      ((tuple #"PUT" 'undefined) (bad req0 state 400 #"missing hash"))
      ((tuple #"PUT"  h) (handle-put req0 state h))
      ((tuple #"POST" _) (handle-check req0 state))
      (_ (bad req0 state 405 #"method not allowed")))))

;;; --- GET /sync/blobs/:hash ------------------------------------------

(defun handle-get (req0 state hash)
  (case (scope req0 state 'pull)
    ((tuple 'error status) (bad req0 state status #"unauthorized"))
    ((tuple 'ok sc)
     (case (call (blob-mod state) 'get-blob sc hash)
       ('not-found (bad req0 state 404 #"not found"))
       ((tuple 'ok ctype bytes)
        (tuple 'ok
               (cowboy_req:reply
                200
                (map #"content-type" (content-type-bin ctype)
                     #"cache-control" #"private, max-age=31536000, immutable")
                bytes req0)
               state))))))

;;; --- PUT /sync/blobs/:hash ------------------------------------------

(defun handle-put (req0 state hash)
  (case (scope req0 state 'push)
    ((tuple 'error status) (bad req0 state status #"unauthorized"))
    ((tuple 'ok sc)
     (let* (((tuple bytes req1) (sync-http-util:read-body req0))
            (ctype (cowboy_req:header #"content-type" req1 'undefined)))
       (case (call (blob-mod state) 'put-blob sc hash ctype bytes)
         ('ok
          (tuple 'ok
                 (sync-http-util:reply
                  req1 200 (json:encode (map #"ok" 'true #"hash" hash)))
                 state))
         ((tuple 'error 'hash-mismatch)
          (bad req1 state 422 #"hash does not match body"))
         ((tuple 'error _)
          (bad req1 state 500 #"store error")))))))

;;; --- POST /sync/blobs/check -----------------------------------------

(defun handle-check (req0 state)
  (case (scope req0 state 'pull)
    ((tuple 'error status) (bad req0 state status #"unauthorized"))
    ((tuple 'ok sc)
     (let (((tuple body req1) (sync-http-util:read-body req0)))
       (case (sync-http-util:decode body)
         ((tuple 'error _) (bad req1 state 400 #"invalid json"))
         ((tuple 'ok m)
          (let* ((hashes (maps:get #"hashes" m '()))
                 (have   (call (blob-mod state) 'have-blobs sc hashes)))
            (tuple 'ok
                   (sync-http-util:reply
                    req1 200 (json:encode (map #"have" have)))
                   state))))))))

;;; --- helpers --------------------------------------------------------

(defun blob-mod (state) (maps:get 'blob state))

(defun scope (req state op)
  (sync-http-util:resolve-scope
   req (maps:get 'auth state 'undefined) op 'undefined))

(defun content-type-bin (ctype)
  (case ctype
    ('null #"application/octet-stream")
    ('undefined #"application/octet-stream")
    (c (if (is_binary c) c #"application/octet-stream"))))

(defun bad (req state status msg)
  (tuple 'ok (sync-http-util:reply req status (sync-http-util:err msg)) state))
