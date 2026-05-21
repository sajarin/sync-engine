;;;; sync-http-subscribe -- Cowboy SSE handler for GET /sync/subscribe.
;;;;
;;;; A loop handler. On connect it joins the scope's pg group and
;;;; streams Server-Sent Events; when sync-scope fans out a push the
;;;; handler forwards a `sync-updated` event. A keepalive comment every
;;;; 30s holds the connection open through idle proxies.
;;;;
;;;; The client uses this only as a hint -- it then does a normal pull.

(defmodule sync-http-subscribe
  (export (init 2) (info 3)))

(defun keepalive-ms () 30000)

(defun init (req0 state)
  (case (cowboy_req:method req0)
    (#"GET" (start-stream req0 state))
    (_ (tuple 'ok
              (sync-http-util:reply
               req0 405 (sync-http-util:err #"method not allowed"))
              state))))

(defun start-stream (req0 state)
  (case (sync-http-util:resolve-scope
         req0 (maps:get 'auth state 'undefined)
         'subscribe (sync-http-util:qs-scope req0))
    ((tuple 'error status)
     (tuple 'ok
            (sync-http-util:reply
             req0 status (sync-http-util:err #"unauthorized"))
            state))
    ((tuple 'ok scope)
     (let ((req1 (cowboy_req:stream_reply
                  200
                  (map #"content-type"      #"text/event-stream"
                       #"cache-control"     #"no-cache"
                       #"x-accel-buffering" #"no")
                  req0)))
       (pg:join (sync-scope:pg-scope) scope (self))
       (cowboy_req:stream_events
        (map 'event #"ready" 'data #"{}") 'nofin req1)
       (erlang:send_after (keepalive-ms) (self) 'keepalive)
       (tuple 'cowboy_loop req1 state 'hibernate)))))

(defun info
  (((tuple 'sync-updated version) req state)
   (cowboy_req:stream_events
    (map 'event #"sync-updated"
         'data  (json:encode (map #"version" version)))
    'nofin req)
   (tuple 'ok req state 'hibernate))
  (('keepalive req state)
   (cowboy_req:stream_events (map 'comment #"keepalive") 'nofin req)
   (erlang:send_after (keepalive-ms) (self) 'keepalive)
   (tuple 'ok req state 'hibernate))
  ((_msg req state)
   (tuple 'ok req state 'hibernate)))
