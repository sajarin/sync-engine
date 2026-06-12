;;;; sync-http -- Cowboy HTTP transport for the sync engine.
;;;;
;;;; A thin adapter: parse JSON, call the engine, serialise the result.
;;;; The engine core stays transport-free and embeddable.
;;;;
;;;;   (sync-store-ets:init)
;;;;   (sync-http:start 'sync-store-ets)                 ; :3001, no auth
;;;;   (sync-http:start 'sync-store-ets 8080)            ; custom port
;;;;   (sync-http:start 'sync-store-ets 'sync-auth-jwt 8080)  ; with auth
;;;;   (sync-http:start 'sync-store-sqlite 'sync-auth-jwt 'sync-blob-sqlite 8080) ; + blobs
;;;;   (sync-http:stop)
;;;;
;;;; AUTH is a module implementing the sync-auth behaviour, or
;;;; 'undefined for dev mode (scope taken from the request).
;;;; BLOB is a module implementing the sync-blob behaviour, or
;;;; 'undefined to omit the blob/manifest/state routes.

(defmodule sync-http
  (export
   (start 1)
   (start 2)
   (start 3)
   (start 4)
   (stop 0)
   (ensure-infra 0)
   (set-auth-cookie 1)
   (routes 2)
   (blob-routes 1)))

(defun default-port () 3001)

(defun start (store)
  (start store 'undefined 'undefined (default-port)))

(defun start (store port)
  (start store 'undefined 'undefined port))

(defun start (store auth port)
  (start store auth 'undefined port))

(defun start (store auth blob port)
  (application:ensure_all_started 'cowboy)
  (ensure-infra)
  (let ((dispatch
         (cowboy_router:compile
          (list (tuple '_ (routes (map 'store store 'auth auth 'blob blob)
                                  blob))))))
    (cowboy:start_clear
     'sync-http-listener
     (map 'socket_opts (list (tuple 'port port)))
     (map 'env (map 'dispatch dispatch)))))

(defun routes (state blob)
  "The sync-engine route list, for embedding apps that compile their own
   dispatch. STATE is #m('store .. 'auth .. 'blob ..). Blob/manifest/state
   routes are included only when BLOB is a module (not 'undefined)."
  (++ (list
       (tuple #"/health" 'sync-http-health (map))
       (tuple #"/sync/pull"      'sync-http-pull      state)
       (tuple #"/sync/push"      'sync-http-push      state)
       (tuple #"/sync/subscribe" 'sync-http-subscribe state)
       (tuple #"/sync/state"     'sync-http-state     state))
      (case blob
        ('undefined '())
        (_ (blob-routes state)))))

(defun blob-routes (state)
  "Blob + manifest routes. `check` MUST precede `:hash` or Cowboy binds
   `check` to :hash."
  (list
   (tuple #"/sync/blobs/check" 'sync-http-blob state)
   (tuple #"/sync/blobs/:hash" 'sync-http-blob state)
   (tuple #"/sync/manifest/:entityType/:entityId" 'sync-http-manifest state)))

(defun ensure-infra ()
  "Start the pg scope and the scope supervisor (idempotent)."
  (ensure-started (pg:start_link (sync-scope:pg-scope)))
  (ensure-started (sync-scope-sup:start_link))
  'ok)

(defun ensure-started
  (((tuple 'ok _)) 'ok)
  (((tuple 'error (tuple 'already_started _))) 'ok)
  ((_) 'ok))

(defun set-auth-cookie (name)
  "Override the cookie name the SSE/cookie auth path looks for."
  (persistent_term:put #(sync-engine auth-cookie) name)
  'ok)

(defun stop ()
  (cowboy:stop_listener 'sync-http-listener))
