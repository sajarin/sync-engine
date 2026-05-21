;;;; sync-scope -- per-scope gen_server.
;;;;
;;;; One process per sync scope. All pushes for a scope funnel through
;;;; its mailbox, so version assignment is race-free (single writer).
;;;; After a push it fans the new version out to that scope's
;;;; subscribers via pg.
;;;;
;;;; The process registers globally as {sync-scope, ScopeId} -- no atoms
;;;; are minted from untrusted scope ids.

(defmodule sync-scope
  (behaviour gen_server)
  (export
   (start_link 2)
   (push 3)
   (ensure 2)
   (pg-scope 0))
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)))

;; No record include: sync-scope moves opaque change lists through to
;; sync-engine without inspecting them.

(defun pg-scope () 'sync-engine-pg)

(defun reg-name (scope) (tuple 'sync-scope scope))

(defun start_link (scope store)
  (gen_server:start_link
   (tuple 'global (reg-name scope))
   'sync-scope
   (list scope store)
   '()))

(defun ensure (scope store)
  "Return the gen_server pid for SCOPE, starting it on demand."
  (case (global:whereis_name (reg-name scope))
    ('undefined
     (case (sync-scope-sup:start-child scope store)
       ((tuple 'ok pid) pid)
       ((tuple 'error (tuple 'already_started pid)) pid)))
    (pid pid)))

(defun push (scope store changes)
  "Serialised push for SCOPE -> {ok, #m(acked applied version)}."
  (gen_server:call (ensure scope store) (tuple 'push changes)))

;;; --- gen_server callbacks -------------------------------------------

(defun init
  (((list scope store))
   (tuple 'ok (map 'scope scope 'store store))))

(defun handle_call
  (((tuple 'push changes) _from state)
   (let* ((scope  (maps:get 'scope state))
          (store  (maps:get 'store state))
          (result (sync-engine:push store scope changes)))
     (fan-out scope result)
     (tuple 'reply result state)))
  ((_msg _from state)
   (tuple 'reply (tuple 'error 'unknown-call) state)))

(defun handle_cast ((_msg state) (tuple 'noreply state)))
(defun handle_info ((_msg state) (tuple 'noreply state)))

;;; --- fan-out --------------------------------------------------------

(defun fan-out (scope result)
  (case result
    ((tuple 'ok res)
     (let ((version (maps:get 'version res)))
       (lists:foreach
        (lambda (pid) (! pid (tuple 'sync-updated version)))
        (pg:get_members (pg-scope) scope))))
    (_ 'ok)))
