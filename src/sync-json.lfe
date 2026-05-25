;;;; sync-json -- change <-> JSON map conversion.
;;;;
;;;; Reusable across transports. Uses the OTP 27+ built-in `json`
;;;; module, so there is no JSON dependency. JSON keys are camelCase
;;;; binaries; `op` and `entity-type` stay binaries (never atomised --
;;;; atomising untrusted input risks atom-table exhaustion).

(defmodule sync-json
  (export
   (change->map 1)
   (map->change 1)
   (pull-result->json 1)
   (push-result->json 1)))

(include-lib "sync_engine/include/sync-records.lfe")

;;; --- helpers ---------------------------------------------------------

(defun nv (x)
  "JSON-safe: unset record fields ('undefined) become null."
  (if (=:= x 'undefined) 'null x))

(defun to-bin
  "Coerce an atom or binary to a binary."
  ((x) (when (is_binary x)) x)
  ((x) (when (is_atom x))   (atom_to_binary x 'utf8))
  ((x) x))

(defun mget (m k)   (maps:get k m 'undefined))
(defun mget (m k d) (maps:get k m d))

;;; --- change <-> map --------------------------------------------------

(defun change->map (c)
  (map #"id"         (nv (change-id c))
       #"scope"      (nv (change-scope c))
       #"entityType" (to-bin (change-entity-type c))
       #"entityId"   (nv (change-entity-id c))
       #"op"         (to-bin (change-op c))
       #"payload"    (nv (change-payload c))
       #"version"    (change-version c)
       #"deviceId"   (nv (change-device-id c))
       #"timestamp"  (nv (change-timestamp c))))

(defun map->change (m)
  (make-change
   id          (mget m #"id")
   scope       (mget m #"scope")
   entity-type (mget m #"entityType" #"undefined")
   entity-id   (mget m #"entityId")
   op          (mget m #"op" #"update")
   payload     (mget m #"payload")
   version     (mget m #"version" 0)
   device-id   (mget m #"deviceId")
   timestamp   (mget m #"timestamp")))

;;; --- response encoders ----------------------------------------------

(defun pull-result->json
  "#(changes next has-more) -> JSON iodata."
  (((tuple changes next has-more))
   (json:encode
    (map #"changes" (lists:map (lambda (c) (change->map c)) changes)
         #"next"    next
         #"hasMore" has-more))))

(defun push-result->json
  "#(ok #m(acked applied version)) -> JSON iodata."
  (((tuple 'ok m))
   (json:encode
    (map #"acked"   (maps:get 'acked m)
         #"applied" (lists:map (lambda (c) (change->map c))
                               (maps:get 'applied m))
         #"version" (maps:get 'version m)))))
