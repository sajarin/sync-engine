;;;; sync-http-push -- Cowboy handler for POST /sync/push.
;;;;
;;;; Request : #m("scope" .. "changes" [..])
;;;; Response: #m("acked" [..] "applied" [..] "version" ..)
;;;;
;;;; Pushes route through sync-scope (the per-scope gen_server) so
;;;; version assignment is serialised and the new version fans out to
;;;; subscribers. Scope is token-derived when auth is configured.

(defmodule sync-http-push
  (export (init 2)))

(defun init (req0 state)
  (case (cowboy_req:method req0)
    (#"POST" (handle req0 state))
    (_ (tuple 'ok
              (sync-http-util:reply
               req0 405 (sync-http-util:err #"method not allowed"))
              state))))

(defun handle (req0 state)
  (let (((tuple body req1) (sync-http-util:read-body req0)))
    (case (sync-http-util:decode body)
      ((tuple 'error _)
       (tuple 'ok
              (sync-http-util:reply
               req1 400 (sync-http-util:err #"invalid json"))
              state))
      ((tuple 'ok m)
       (case (sync-http-util:resolve-scope
              req1 (maps:get 'auth state 'undefined)
              'push (maps:get #"scope" m 'undefined))
         ((tuple 'error status)
          (tuple 'ok
                 (sync-http-util:reply
                  req1 status (sync-http-util:err #"unauthorized"))
                 state))
         ((tuple 'ok scope)
          (let* ((store   (maps:get 'store state))
                 (changes (lists:map
                           (lambda (cm) (sync-json:map->change cm))
                           (maps:get #"changes" m '())))
                 (result  (sync-scope:push scope store changes)))
            (tuple 'ok
                   (sync-http-util:reply
                    req1 200 (sync-json:push-result->json result))
                   state))))))))
