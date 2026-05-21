;;;; sync-core -- the pure functional heart of the sync engine.
;;;;
;;;; Every function here is a pure transform over plain change lists:
;;;; no I/O, no process state, no storage. This is the part that is
;;;; trivially testable and that transports/stores compose around.

(defmodule sync-core
  (export
   (assign-versions 2)
   (dedup 2)
   (filter-since 2)
   (sort-by-version 1)
   (max-version 1)
   (pull 3)))

(include-file "sync-records.lfe")

;;; --- versioning ------------------------------------------------------

(defun assign-versions (changes base)
  "Stamp CHANGES with sequential versions starting at BASE + 1.
   Returns #(versioned-changes top-version)."
  (assign-versions changes base '()))

(defun assign-versions
  (('() base acc)
   (tuple (lists:reverse acc) base))
  (((cons c rest) base acc)
   (let ((v (+ base 1)))
     (assign-versions rest v (cons (set-change-version c v) acc)))))

(defun max-version
  "Highest version among CHANGES, or 0 when there are none."
  (('()) 0)
  ((changes)
   (lists:max (lists:map (lambda (c) (change-version c)) changes))))

;;; --- selection -------------------------------------------------------

(defun dedup (changes seen-ids)
  "Drop changes whose id is already known (idempotent push)."
  (lists:filter
   (lambda (c) (not (lists:member (change-id c) seen-ids)))
   changes))

(defun filter-since (changes cursor)
  "Keep only changes newer than CURSOR."
  (lists:filter
   (lambda (c) (> (change-version c) cursor))
   changes))

(defun sort-by-version (changes)
  "Order changes by ascending version."
  (lists:sort
   (lambda (a b) (=< (change-version a) (change-version b)))
   changes))

;;; --- pull ------------------------------------------------------------

(defun pull (changes cursor limit)
  "Pure pull: from CHANGES return at most LIMIT entries newer than
   CURSOR, ordered by version. Returns #(page next-cursor has-more?)."
  (let* ((fresh (sort-by-version (filter-since changes cursor)))
         (page (lists:sublist fresh limit))
         (has-more (> (length fresh) (length page)))
         (next (case page
                 ('() cursor)
                 (_   (max-version page)))))
    (tuple page next has-more)))
