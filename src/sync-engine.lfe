;;;; sync-engine -- orchestration: composes sync-core with a sync-store.
;;;;
;;;; This is the seam a transport layer (HTTP/SSE/WebSocket) calls into.
;;;; STORE is any module implementing the sync-store behaviour, so the
;;;; engine has no idea where data lives or what the payloads mean.

(defmodule sync-engine
  (export
   (pull 4)
   (push 3)))

(include-lib "sync_engine/include/sync-records.lfe")

(defun pull (store scope cursor limit)
  "Return changes for SCOPE newer than CURSOR via STORE.
   -> #(changes next-cursor has-more?)."
  (call store 'read-since scope cursor limit))

(defun push (store scope pending)
  "Persist PENDING changes for SCOPE: drop already-seen ids, assign
   versions, append. -> #(ok #m(acked .. applied .. version ..)).
   acked lists every pending id (already-stored ids are acked too, so
   the client can safely drop them)."
  (let* ((ids     (lists:map (lambda (c) (change-id c)) pending))
         (already (call store 'seen-ids scope ids))
         (fresh   (sync-core:dedup pending already))
         (base    (call store 'current-version scope)))
    (case (sync-core:assign-versions fresh base)
      ((tuple versioned top)
       (call store 'append-changes scope versioned)
       (tuple 'ok (map 'acked   ids
                       'applied versioned
                       'version top))))))
