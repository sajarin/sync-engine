;;;; sync-resolver -- the conflict-resolution behaviour.
;;;;
;;;; Decides what happens when a pushed change collides with the change
;;;; already stored for the same entity. Swap the module to get
;;;; field-level merge, CRDT semantics, etc.

(defmodule sync-resolver
  (export (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   ;; resolve(Incoming, Existing, Ctx) -> Resolution
   ;;   Resolution :: {accept, Change}     -- take this change
   ;;               | {reject, Reason}     -- drop the incoming change
   ;;               | {merged, Change}     -- store a merged change
   (list (tuple 'resolve 3)))
  ((_) 'undefined))
