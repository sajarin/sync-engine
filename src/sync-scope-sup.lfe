;;;; sync-scope-sup -- dynamic supervisor for per-scope gen_servers.
;;;;
;;;; simple_one_for_one: scope processes are spawned on demand by
;;;; sync-scope:ensure/2 and are 'temporary' -- a crash takes down only
;;;; that one scope, and it is recreated on the next request.

(defmodule sync-scope-sup
  (behaviour supervisor)
  (export
   (start_link 0)
   (start-child 2)
   (init 1)))

(defun start_link ()
  (supervisor:start_link
   (tuple 'local 'sync-scope-sup)
   'sync-scope-sup
   '()))

(defun start-child (scope store)
  (supervisor:start_child 'sync-scope-sup (list scope store)))

(defun init (_args)
  (let ((flags (map 'strategy  'simple_one_for_one
                    'intensity 10
                    'period    10))
        (child (map 'id      'sync-scope
                    'start   (tuple 'sync-scope 'start_link '())
                    'restart 'temporary
                    'shutdown 5000
                    'type    'worker)))
    (tuple 'ok (tuple flags (list child)))))
