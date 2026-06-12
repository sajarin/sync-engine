;;;; sync-materializer -- the change-stream reducer behaviour.
;;;;
;;;; The engine's `changes` table is the single source of truth, but many
;;;; apps also want current-state domain tables (a `books` table, a
;;;; `notes` table) to query directly or to gate REST endpoints on. Rather
;;;; than dual-writing those tables through a separate path (which drifts
;;;; from the change log), an app registers a materializer: a reducer the
;;;; engine invokes for every freshly-applied change, in the same push that
;;;; appended it. Domain tables become a pure projection of the log.
;;;;
;;;; The default is `sync-materializer-noop`. An app sets its own with
;;;; sync-materializer:set/1 and the push path picks it up.
;;;;
;;;; Contract: materialize(Scope, Change) -> 'ok | #(error Reason).
;;;; A crashing or error-returning materializer MUST NOT fail the push --
;;;; the change is already durably in the log; the projection can be rebuilt
;;;; (see replay/3). The engine logs and moves on.

(defmodule sync-materializer
  (export
   (behaviour_info 1)
   (set 1)
   (get 0)
   (apply-changes 2)
   (replay 3)))

(include-lib "sync_engine/include/sync-records.lfe")

(defun behaviour_info
  (('callbacks)
   ;; materialize(Scope, Change) -> 'ok | #(error Reason)
   (list (tuple 'materialize 2)))
  ((_) 'undefined))

(defun pt-key () #(sync-engine materializer))

(defun set (mod)
  "Register the app's materializer module (or 'undefined to disable)."
  (persistent_term:put (pt-key) mod)
  'ok)

(defun get ()
  (persistent_term:get (pt-key) 'undefined))

(defun apply-changes (scope changes)
  "Run the configured materializer over freshly-applied CHANGES. Never
   throws: a materializer fault is isolated per-change and logged, because
   the log is already authoritative."
  (case (get)
    ('undefined 'ok)
    (mod
     (lists:foreach
      (lambda (ch) (safe-materialize mod scope ch))
      changes)
     'ok)))

(defun safe-materialize (mod scope ch)
  (try
    (case (call mod 'materialize scope ch)
      ('ok 'ok)
      ((tuple 'error reason)
       (logger:warning "sync-materializer ~p error ~p on change ~p"
                       (list mod reason (change-id ch)))
       'ok)
      (_ 'ok))
    (catch
      ((tuple class reason stack)
       (logger:error "sync-materializer ~p crash ~p:~p on change ~p~n~p"
                     (list mod class reason (change-id ch) stack))
       'ok))))

(defun replay (mod store scope)
  "Rebuild a projection: feed every stored change for SCOPE through MOD.
   For recovery / schema changes. Pages through the store in 500s."
  (set mod)
  (replay-loop mod store scope 0))

(defun replay-loop (mod store scope cursor)
  (case (call store 'read-since scope cursor 500)
    ((tuple '() _ _) 'ok)
    ((tuple page next has-more)
     (lists:foreach (lambda (ch) (safe-materialize mod scope ch)) page)
     (case has-more
       ('true  (replay-loop mod store scope next))
       ('false 'ok)))))
