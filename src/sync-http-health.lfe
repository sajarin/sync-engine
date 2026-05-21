;;;; sync-http-health -- Cowboy handler for GET /health.

(defmodule sync-http-health
  (export (init 2)))

(defun init (req0 state)
  (tuple 'ok
         (sync-http-util:reply req0 200 (json:encode (map #"status" #"ok")))
         state))
