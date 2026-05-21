;;;; sync-http-pull -- Cowboy handler for POST /sync/pull.
;;;;
;;;; Request : #m("scope" .. "cursor" .. "limit" ..)
;;;; Response: #m("changes" [..] "next" .. "hasMore" ..)
;;;;
;;;; With auth configured the scope is taken from the token, not the
;;;; body; the body "scope" is only used in dev mode (auth = undefined).

(defmodule sync-http-pull
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
              'pull (maps:get #"scope" m 'undefined))
         ((tuple 'error status)
          (tuple 'ok
                 (sync-http-util:reply
                  req1 status (sync-http-util:err #"unauthorized"))
                 state))
         ((tuple 'ok scope)
          (let* ((store  (maps:get 'store state))
                 (cursor (maps:get #"cursor" m 0))
                 ;; Clamp limit to >= 1. A limit of 0 returns an empty
                 ;; page with has-more set and an unchanged cursor, which
                 ;; loops a paginating client forever.
                 (limit  (max 1 (maps:get #"limit" m 500)))
                 (result (sync-engine:pull store scope cursor limit)))
            (tuple 'ok
                   (sync-http-util:reply
                    req1 200 (sync-json:pull-result->json result))
                   state))))))))
