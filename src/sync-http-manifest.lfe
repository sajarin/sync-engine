;;;; sync-http-manifest -- Cowboy handler for per-entity blob manifests.
;;;;
;;;; A manifest lists which blob hashes (and in what order, with what
;;;; per-ref metadata) make up an entity, so a client can pull "the shape
;;;; of book X" and fetch only the blobs it's missing.
;;;;
;;;;   GET /sync/manifest/:entityType/:entityId
;;;;       -> #m("refs" [#m("hash" "ord" "meta" "contentType" "size" "present")])
;;;;   PUT /sync/manifest/:entityType/:entityId
;;;;       body #m("refs" [#m("hash" "ord" "meta")]) -> replace-all.
;;;;
;;;; Most manifests are built server-side by a materializer from the change
;;;; stream; PUT is offered for apps that prefer to assemble them client-side.
;;;;
;;;; State map: #m('blob BlobModule 'auth AuthModule).

(defmodule sync-http-manifest
  (export (init 2)))

(defun init (req0 state)
  (let ((etype (cowboy_req:binding 'entityType req0 'undefined))
        (eid   (cowboy_req:binding 'entityId req0 'undefined)))
    (case (and (is-set etype) (is-set eid))
      ('false (bad req0 state 400 #"missing entity type or id"))
      ('true
       (case (cowboy_req:method req0)
         (#"GET" (handle-get req0 state etype eid))
         (#"PUT" (handle-put req0 state etype eid))
         (_ (bad req0 state 405 #"method not allowed")))))))

(defun handle-get (req0 state etype eid)
  (case (scope req0 state 'pull)
    ((tuple 'error status) (bad req0 state status #"unauthorized"))
    ((tuple 'ok sc)
     (let ((refs (call (blob-mod state) 'get-manifest sc etype eid)))
       (tuple 'ok
              (sync-http-util:reply
               req0 200 (json:encode (map #"refs" refs)))
              state)))))

(defun handle-put (req0 state etype eid)
  (case (scope req0 state 'push)
    ((tuple 'error status) (bad req0 state status #"unauthorized"))
    ((tuple 'ok sc)
     (let (((tuple body req1) (sync-http-util:read-body req0)))
       (case (sync-http-util:decode body)
         ((tuple 'error _) (bad req1 state 400 #"invalid json"))
         ((tuple 'ok m)
          (let ((refs (maps:get #"refs" m '())))
            (call (blob-mod state) 'put-manifest sc etype eid refs)
            (tuple 'ok
                   (sync-http-util:reply
                    req1 200 (json:encode (map #"ok" 'true)))
                   state))))))))

;;; --- helpers --------------------------------------------------------

(defun blob-mod (state) (maps:get 'blob state))

(defun scope (req state op)
  (sync-http-util:resolve-scope
   req (maps:get 'auth state 'undefined) op 'undefined))

(defun is-set (x) (andalso (=/= x 'undefined) (=/= x 'null)))

(defun bad (req state status msg)
  (tuple 'ok (sync-http-util:reply req status (sync-http-util:err msg)) state))
