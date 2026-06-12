;;;; sync-rate-limit -- reusable ETS fixed-window rate limiter.
;;;;
;;;; Shipped with the engine because a sync server controls its own request
;;;; fan-out and should rate-limit by the sync identity (scope = user,
;;;; device) rather than by IP -- two devices behind one NAT must not share
;;;; a budget. Apps opt in by calling check-tier/2 in their handlers; the
;;;; engine's own routes do not force it.
;;;;
;;;; A gen_server owns the table and prunes expired windows every 60s.
;;;;
;;;; Tiers are #(Max WindowMs Keyer). The key is namespaced by the tier
;;;; signature so distinct tiers never share a counter -- the bug that made
;;;; a burst of cheap reads trip a separate write tier's limit.

(defmodule sync-rate-limit
  (behaviour gen_server)
  (export
   (start_link 0)
   (init 1)
   (handle_call 3) (handle_cast 2) (handle_info 2)
   (check 3)
   (check-tier 2)))

(defun table () 'sync-rate-limit)
(defun prune-interval-ms () 60000)

;;; --- gen_server -----------------------------------------------------

(defun start_link ()
  (gen_server:start_link
   (tuple 'local 'sync-rate-limit-gs) 'sync-rate-limit '() '()))

(defun init (_args)
  (ensure-table)
  (erlang:send_after (prune-interval-ms) (self) 'prune)
  (tuple 'ok 'no-state))

(defun ensure-table ()
  (case (ets:info (table))
    ('undefined
     (ets:new (table)
              (list 'named_table 'public 'set
                    (tuple 'write_concurrency 'true)
                    (tuple 'read_concurrency 'true)))
     'ok)
    (_ 'ok)))

(defun handle_call (_msg _from state) (tuple 'reply 'ok state))
(defun handle_cast (_msg state) (tuple 'noreply state))

(defun handle_info
  (('prune state)
   (prune-expired)
   (erlang:send_after (prune-interval-ms) (self) 'prune)
   (tuple 'noreply state))
  ((_msg state) (tuple 'noreply state)))

;;; --- atomic fixed-window check --------------------------------------
;;; Row: {Key, Count, ResetAtMs}. update_counter with a default tuple opens
;;; the window lock-free on the first request.
;;; Returns #(ok Remaining ResetAt) | #(rate_limited RetryAfterS ResetAt).

(defun check (key max window-ms)
  (ensure-table)
  (let* ((now (now-ms))
         (default-reset (+ now window-ms))
         (new-count
          (ets:update_counter
           (table) key (tuple 2 1) (tuple key 0 default-reset)))
         (reset-at (current-reset-at key)))
    (case (< reset-at now)
      ('true
       ;; window expired between read and write: roll forward
       (ets:insert (table) (tuple key 1 default-reset))
       (tuple 'ok (- max 1) default-reset))
      ('false
       (case (> new-count max)
         ('true
          (tuple 'rate_limited
                 (max-int 1 (div-ceil (- reset-at now) 1000))
                 reset-at))
         ('false
          (tuple 'ok (- max new-count) reset-at)))))))

;;; --- tier check -----------------------------------------------------
;;;
;;; TIER = #(Max WindowMs Keyer).  Keyer in:
;;;   'scope        -> per user (scope)
;;;   'device       -> per device id
;;;   'scope-device -> per (scope, device)   [default; the right granularity]
;;; CTX = #m('scope .. 'device-id .. ) (binaries; missing -> "anon").

(defun check-tier (tier ctx)
  (let* (((tuple max window-ms keyer) tier)
         (base (base-key keyer ctx))
         ;; Namespace by tier signature so each tier has its own counter.
         (key  (iolist_to_binary
                (list (integer_to_binary max) #"/"
                      (integer_to_binary window-ms) #":" base))))
    (check key max window-ms)))

(defun base-key (keyer ctx)
  (let ((scope (bin (maps:get 'scope ctx #"anon")))
        (dev   (bin (maps:get 'device-id ctx #"anon"))))
    (case keyer
      ('scope        (iolist_to_binary (list #"s:" scope)))
      ('device       (iolist_to_binary (list #"d:" dev)))
      ('scope-device (iolist_to_binary (list #"sd:" scope #"/" dev)))
      (_             (iolist_to_binary (list #"sd:" scope #"/" dev))))))

;;; --- helpers --------------------------------------------------------

(defun bin
  ((x) (when (is_binary x)) x)
  ((x) (when (is_atom x))   (atom_to_binary x 'utf8))
  ((x) (iolist_to_binary (io_lib:format "~p" (list x)))))

(defun current-reset-at (key)
  (case (ets:lookup_element (table) key 3 0)
    (0 (+ (now-ms) 60000))
    (v v)))

(defun div-ceil (n d) (div (+ n (- d 1)) d))
(defun max-int (a b) (if (> a b) a b))

(defun prune-expired ()
  (let ((cutoff (now-ms)))
    (ets:select_delete
     (table)
     (list (tuple (tuple '$1 '$2 '$3)
                  (list (tuple '< '$3 cutoff))
                  (list 'true))))))

(defun now-ms () (erlang:system_time 'millisecond))
