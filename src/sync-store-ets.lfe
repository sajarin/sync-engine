;;;; sync-store-ets -- reference sync-store backed by ETS.
;;;;
;;;; In-memory, process-independent (named public tables). Good for
;;;; tests and tiny single-node deploys; swap for sync-store-sqlite in
;;;; production. Call (init) once before use.

(defmodule sync-store-ets
  (behaviour sync-store)
  (export
   (init 0)
   (append-changes 2)
   (read-since 3)
   (current-version 1)
   (seen-ids 2)
   (get-cursor 2)
   (put-cursor 3)))

(include-file "sync-records.lfe")

(defun changes-table () 'sync-store-ets-changes)
(defun cursors-table () 'sync-store-ets-cursors)

(defun init ()
  "Create the ETS tables if they do not yet exist."
  (ensure-table (changes-table) '(bag named_table public))
  (ensure-table (cursors-table) '(set named_table public))
  'ok)

(defun ensure-table (name opts)
  (case (ets:info name)
    ('undefined (ets:new name opts) 'ok)
    (_ 'ok)))

(defun scope-changes (scope)
  "All change records stored under SCOPE."
  (lists:map
   (lambda (pair) (element 2 pair))
   (ets:lookup (changes-table) scope)))

;;; --- sync-store callbacks -------------------------------------------

(defun append-changes (scope changes)
  (lists:foreach
   (lambda (c) (ets:insert (changes-table) (tuple scope c)))
   changes)
  'ok)

(defun read-since (scope cursor limit)
  (sync-core:pull (scope-changes scope) cursor limit))

(defun current-version (scope)
  (sync-core:max-version (scope-changes scope)))

(defun seen-ids (scope ids)
  (let ((stored (lists:map (lambda (c) (change-id c))
                           (scope-changes scope))))
    (lists:filter (lambda (id) (lists:member id stored)) ids)))

(defun get-cursor (scope device-id)
  (case (ets:lookup (cursors-table) (tuple scope device-id))
    ('() 0)
    ((cons (tuple _ v) _) v)))

(defun put-cursor (scope device-id cursor)
  (ets:insert (cursors-table) (tuple (tuple scope device-id) cursor))
  'ok)
