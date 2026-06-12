;;;; sync-blob -- the content-addressed blob storage behaviour.
;;;;
;;;; The sync-engine's `changes` table is for small, ordered, opaque
;;;; deltas. Heavy assets (files, images, rendered content) don't belong
;;;; there. A sync-blob store holds them content-addressed: the key IS the
;;;; sha256 of the bytes, so the same content uploaded twice (or from two
;;;; devices) stores once and uploads are idempotent + resumable.
;;;;
;;;; Blobs are scope-isolated (a scope = a user, same as changes), and a
;;;; per-entity *manifest* records which hashes make up an entity ("these
;;;; 140 image hashes belong to book X"). A client pulls the manifest,
;;;; diffs it against what it has, and fetches only the missing blobs.
;;;;
;;;; The engine stays dumb about what a blob means -- it only moves bytes
;;;; and lists. The embedding app's materializer (sync-materializer)
;;;; decides what manifests to build from the change stream.

(defmodule sync-blob
  (export (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   (list
    ;; put-blob(Scope, Hash, ContentType, Bytes)
    ;;   -> 'ok | #(error 'hash-mismatch) | #(error Reason)
    ;; Stores Bytes under Hash. The impl MUST verify Hash = sha256(Bytes)
    ;; and reject a mismatch (a client lying about content addressing).
    (tuple 'put-blob 4)
    ;; get-blob(Scope, Hash) -> #(ok ContentType Bytes) | 'not-found
    (tuple 'get-blob 2)
    ;; have-blobs(Scope, [Hash]) -> [Hash]   (the subset already stored)
    (tuple 'have-blobs 2)
    ;; put-manifest(Scope, EntityType, EntityId, [#m(hash ord meta)])
    ;;   -> 'ok   (replace-all semantics for that entity)
    (tuple 'put-manifest 4)
    ;; get-manifest(Scope, EntityType, EntityId)
    ;;   -> [#m(hash ord meta contentType size present)]
    (tuple 'get-manifest 3)
    ;; blob-stats(Scope) -> #m(count totalBytes)
    (tuple 'blob-stats 1)
    ;; gc-unreferenced(Scope) -> integer   (blobs deleted that no manifest cites)
    (tuple 'gc-unreferenced 1)))
  ((_) 'undefined))
