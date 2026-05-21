;;;; sync-auth -- the authentication behaviour.
;;;;
;;;; Hides how a request is authenticated and which scope a caller may
;;;; touch. The engine/transport derives the scope from the principal,
;;;; never from the request body -- a caller cannot sync someone else's
;;;; scope.

(defmodule sync-auth
  (export (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   (list
    ;; authenticate(Token) -> {ok, Principal} | {error, Reason}
    (tuple 'authenticate 1)
    ;; scope(Principal) -> Scope   (the scope this principal owns)
    (tuple 'scope 1)
    ;; authorize(Principal, Scope, Op) -> ok | {error, forbidden}
    ;; Op :: 'pull | 'push | 'subscribe
    (tuple 'authorize 3)))
  ((_) 'undefined))
