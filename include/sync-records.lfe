;;;; sync-engine -- shared record definitions.
;;;; Included by every module that touches a change.

(defrecord change
  ;; client-generated unique id -- used for idempotent dedup on push
  id
  ;; isolation unit: which sync scope this belongs to (e.g. a user id)
  scope
  ;; opaque-to-the-engine entity classifier (atom or binary)
  (entity-type 'undefined)
  ;; id of the entity within its type
  entity-id
  ;; 'create | 'update | 'delete  (deletes are tombstones, never absence)
  (op 'update)
  ;; opaque payload -- the engine never inspects this
  payload
  ;; monotonic per-scope version, assigned by the server on push
  (version 0)
  ;; originating device id
  device-id
  ;; client-supplied timestamp (binary/iso8601 or epoch)
  timestamp)
