;;;; sync-http -- Cowboy HTTP transport for the sync engine.
;;;;
;;;; A thin adapter: parse JSON, call the engine, serialise the result.
;;;; The engine core stays transport-free and embeddable.
;;;;
;;;;   (sync-store-ets:init)
;;;;   (sync-http:start 'sync-store-ets)                 ; :3001, no auth
;;;;   (sync-http:start 'sync-store-ets 8080)            ; custom port
;;;;   (sync-http:start 'sync-store-ets 'sync-auth-jwt 8080)  ; with auth
;;;;   (sync-http:stop)
;;;;
;;;; AUTH is a module implementing the sync-auth behaviour, or
;;;; 'undefined for dev mode (scope taken from the request).

(defmodule sync-http
  (export
   (start 1)
   (start 2)
   (start 3)
   (stop 0)
   (ensure-infra 0)
   (set-auth-cookie 1)))

(defun default-port () 3001)

(defun start (store)
  (start store 'undefined (default-port)))

(defun start (store port)
  (start store 'undefined port))

(defun start (store auth port)
  (application:ensure_all_started 'cowboy)
  (ensure-infra)
  (let ((dispatch
         (cowboy_router:compile
          (list
           (tuple '_
                  (list
                   (tuple #"/health" 'sync-http-health (map))
                   (tuple #"/sync/pull" 'sync-http-pull
                          (map 'store store 'auth auth))
                   (tuple #"/sync/push" 'sync-http-push
                          (map 'store store 'auth auth))
                   (tuple #"/sync/subscribe" 'sync-http-subscribe
                          (map 'store store 'auth auth))))))))
    (cowboy:start_clear
     'sync-http-listener
     (map 'socket_opts (list (tuple 'port port)))
     (map 'env (map 'dispatch dispatch)))))

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
