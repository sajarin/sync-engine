;;;; sync-materializer-noop -- the default materializer: does nothing.
;;;;
;;;; The engine ships with no projection. An app that wants current-state
;;;; domain tables implements the sync-materializer behaviour and registers
;;;; it with sync-materializer:set/1.

(defmodule sync-materializer-noop
  (behaviour sync-materializer)
  (export (materialize 2)))

(defun materialize (_scope _change) 'ok)
