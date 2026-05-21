;;;; sync-auth-jwt -- reference sync-auth: HS256 JSON Web Tokens.
;;;;
;;;; Cookie/Authorization-header transports hand the raw token to
;;;; authenticate/1. The `sub` claim is the scope. The shared secret is
;;;; set once via (init Secret) and kept in persistent_term.
;;;;
;;;; Security notes:
;;;;  - only HS256 is accepted; `alg: none` and any other alg rejected.
;;;;  - the signature is verified before any claim is trusted.
;;;;  - signature comparison is constant-time.

(defmodule sync-auth-jwt
  (behaviour sync-auth)
  (export
   (init 1)
   (sign 1)
   (sign 2)
   (authenticate 1)
   (scope 1)
   (authorize 3)))

(defun pt-key () #(sync-auth-jwt secret))

(defun init (secret)
  "Set the HMAC secret (a binary)."
  (persistent_term:put (pt-key) secret)
  'ok)

(defun secret () (persistent_term:get (pt-key)))

;;; --- base64url + helpers --------------------------------------------

(defun b64-opts () (map 'mode 'urlsafe 'padding 'false))

(defun b64url-encode (bin) (base64:encode bin (b64-opts)))
(defun b64url-decode (bin) (base64:decode bin (b64-opts)))

(defun json-bin (term)
  (erlang:iolist_to_binary (json:encode term)))

(defun join-dot (parts)
  (erlang:iolist_to_binary (lists:join #"." parts)))

(defun hmac (data)
  (crypto:mac 'hmac 'sha256 (secret) data))

(defun constant-eq (a b)
  (case (=:= (byte_size a) (byte_size b))
    ('false 'false)
    ('true  (crypto:hash_equals a b))))

(defun decode-segment (seg)
  (try
    (tuple 'ok (json:decode (b64url-decode seg)))
    (catch
      ((tuple _ _ _) (tuple 'error 'bad-segment)))))

;;; --- signing (for token issuers / tests) ----------------------------

(defun sign (claims) (sign claims 3600))

(defun sign (claims ttl)
  "Mint an HS256 token. CLAIMS is a map; iat/exp are added from TTL."
  (let* ((now  (erlang:system_time 'second))
         (full (maps:merge claims (map #"iat" now #"exp" (+ now ttl))))
         (h64  (b64url-encode (json-bin (map #"alg" #"HS256" #"typ" #"JWT"))))
         (p64  (b64url-encode (json-bin full)))
         (input (join-dot (list h64 p64)))
         (sig  (b64url-encode (hmac input))))
    (join-dot (list input sig))))

;;; --- sync-auth callbacks --------------------------------------------

(defun authenticate (token)
  (case (binary:split token #"." (list 'global))
    ((list h64 p64 sig) (verify h64 p64 sig))
    (_ (tuple 'error 'malformed))))

(defun verify (h64 p64 sig)
  (let ((expected (b64url-encode (hmac (join-dot (list h64 p64))))))
    (case (constant-eq expected sig)
      ('false (tuple 'error 'bad-signature))
      ('true
       (case (decode-segment h64)
         ((tuple 'error _) (tuple 'error 'bad-header))
         ((tuple 'ok header)
          (case (=:= #"HS256" (maps:get #"alg" header #""))
            ('false (tuple 'error 'bad-alg))
            ('true
             (case (decode-segment p64)
               ((tuple 'error _) (tuple 'error 'bad-payload))
               ((tuple 'ok claims)
                (case (expired? claims)
                  ('true  (tuple 'error 'expired))
                  ('false (tuple 'ok claims)))))))))))))

(defun expired? (claims)
  (case (maps:get #"exp" claims 'undefined)
    ('undefined 'false)
    (exp (=< exp (erlang:system_time 'second)))))

(defun scope (principal)
  (maps:get #"sub" principal 'undefined))

(defun authorize (_principal _scope _op)
  ;; Authentication already pins the scope via scope/1, so any
  ;; authenticated principal is authorized for its own scope.
  'ok)
