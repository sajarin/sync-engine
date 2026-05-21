;;;; sync-resolver-lww -- last-write-wins conflict resolver.
;;;;
;;;; Higher version wins; ties break on timestamp. The default policy:
;;;; simple, total, and good enough for settings-style data. Entities
;;;; that need real merge should plug a different sync-resolver.

(defmodule sync-resolver-lww
  (behaviour sync-resolver)
  (export (resolve 3)))

(include-file "sync-records.lfe")

(defun resolve (incoming existing _ctx)
  (case (winner incoming existing)
    ('incoming (tuple 'accept incoming))
    ('existing (tuple 'reject 'stale))))

(defun winner (a b)
  (let ((va (change-version a))
        (vb (change-version b)))
    (cond
     ((> va vb) 'incoming)
     ((< va vb) 'existing)
     ('true (if (>= (ts a) (ts b)) 'incoming 'existing)))))

(defun ts (c)
  (case (change-timestamp c)
    ('undefined #"")
    (t t)))
