;;;; sync-store -- the storage behaviour.
;;;;
;;;; A sync-store hides where changes and cursors actually live. The
;;;; engine talks only to this contract, so a project can back it with
;;;; ETS, SQLite, Postgres, ... without the engine knowing.

(defmodule sync-store
  (export (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   (list
    ;; append-changes(Scope, VersionedChanges) -> 'ok
    (tuple 'append-changes 2)
    ;; read-since(Scope, Cursor, Limit) -> #(changes next has-more)
    (tuple 'read-since 3)
    ;; current-version(Scope) -> integer  (0 when scope empty)
    (tuple 'current-version 1)
    ;; seen-ids(Scope, Ids) -> [Id]  (subset already stored)
    (tuple 'seen-ids 2)
    ;; get-cursor(Scope, DeviceId) -> integer
    (tuple 'get-cursor 2)
    ;; put-cursor(Scope, DeviceId, Cursor) -> 'ok
    (tuple 'put-cursor 3)
    ;; list-cursors(Scope) -> [#(DeviceId Cursor)]   (observability)
    (tuple 'list-cursors 1)
    ;; count-changes(Scope) -> integer               (observability)
    (tuple 'count-changes 1)))
  ((_) 'undefined))
