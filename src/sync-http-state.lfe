;;;; sync-http-state -- Cowboy handler for GET /sync/state.
;;;;
;;;; Per-scope observability: where every device's cursor sits, how many
;;;; changes exist, and blob-store totals. One endpoint turns "why is
;;;; device B missing data?" from console-flood archaeology into a diff.
;;;;
;;;;   GET /sync/state
;;;;     -> #m("scope" .. "version" ..            ; server head version
;;;;           "changeCount" ..
;;;;           "cursors" [#m("deviceId" .. "cursor" .. "behind" ..)]
;;;;           "blobs" #m("count" .. "totalBytes" ..))
;;;;
;;;; State map: #m('store StoreModule 'blob BlobModule|undefined 'auth Auth).

(defmodule sync-http-state
  (export (init 2)))

(defun init (req0 state)
  (case (cowboy_req:method req0)
    (#"GET" (handle req0 state))
    (_ (tuple 'ok
              (sync-http-util:reply
               req0 405 (sync-http-util:err #"method not allowed"))
              state))))

(defun handle (req0 state)
  (case (sync-http-util:resolve-scope
         req0 (maps:get 'auth state 'undefined) 'pull
         (sync-http-util:qs-scope req0))
    ((tuple 'error status)
     (tuple 'ok
            (sync-http-util:reply req0 status (sync-http-util:err #"unauthorized"))
            state))
    ((tuple 'ok scope)
     (let* ((store   (maps:get 'store state))
            (version (call store 'current-version scope))
            (count   (call store 'count-changes scope))
            (cursors (call store 'list-cursors scope))
            (blobs   (blob-stats state scope)))
       (tuple 'ok
              (sync-http-util:reply
               req0 200
               (json:encode
                (map #"scope"       scope
                     #"version"     version
                     #"changeCount" count
                     #"cursors"     (cursors->json cursors version)
                     #"blobs"       blobs)))
              state)))))

(defun cursors->json (cursors version)
  "Each device's cursor plus how far it lags the server head."
  (lists:map
   (lambda (c)
     (case c
       ((tuple did cur)
        (map #"deviceId" did
             #"cursor"   cur
             #"behind"   (max 0 (- version cur))))))
   cursors))

(defun blob-stats (state scope)
  (case (maps:get 'blob state 'undefined)
    ('undefined (map #"count" 0 #"totalBytes" 0))
    (mod (call mod 'blob-stats scope))))
