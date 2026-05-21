;;;; sync-http-util -- shared helpers for the Cowboy HTTP transport.

(defmodule sync-http-util
  (export
   (read-body 1)
   (reply 3)
   (decode 1)
   (err 1)
   (extract-token 1)
   (qs-scope 1)
   (resolve-scope 4)))

;;; --- body / response ------------------------------------------------

(defun read-body (req)
  "Read a full request body into a binary. Returns #(body req)."
  (read-body req '()))

(defun read-body (req acc)
  (case (cowboy_req:read_body req)
    ((tuple 'ok data req1)
     (tuple (erlang:iolist_to_binary (lists:reverse (cons data acc))) req1))
    ((tuple 'more data req1)
     (read-body req1 (cons data acc)))))

(defun reply (req status body)
  (cowboy_req:reply
   status
   (map #"content-type" #"application/json")
   body
   req))

(defun decode (body)
  (try
    (tuple 'ok (json:decode body))
    (catch
      ((tuple _ _ _) (tuple 'error 'bad-json)))))

(defun err (msg)
  (json:encode (map #"error" msg)))

;;; --- auth -----------------------------------------------------------

(defun extract-token (req)
  "Bearer token from the Authorization header, else the sync_token
   cookie (EventSource cannot set headers, only cookies)."
  (case (cowboy_req:header #"authorization" req 'undefined)
    ('undefined (cookie-token req))
    (h (case (binary:split h #" ")
         ((list #"Bearer" t) t)
         (_ (cookie-token req))))))

(defun auth-cookie-name ()
  "The cookie carrying the token. Default 'sync_token'; an embedding app
   can override it via sync-http:set-auth-cookie/1."
  (persistent_term:get #(sync-engine auth-cookie) #"sync_token"))

(defun cookie-token (req)
  (case (lists:keyfind (auth-cookie-name) 1 (cowboy_req:parse_cookies req))
    ((tuple _ t) t)
    (_ 'undefined)))

(defun qs-scope (req)
  "Scope from the ?scope= query param (dev mode, no auth configured)."
  (case (lists:keyfind #"scope" 1 (cowboy_req:parse_qs req))
    ((tuple _ s) s)
    (_ 'undefined)))

(defun resolve-scope (req auth op dev-scope)
  "Resolve the scope a request may touch. Returns #(ok Scope) | #(error Status).
   auth = 'undefined -> dev mode: trust DEV-SCOPE (request body / query).
   auth = Module     -> authenticate the token; scope comes from the
                        principal, so a caller cannot touch another scope."
  (case auth
    ('undefined (tuple 'ok dev-scope))
    (mod
     (case (extract-token req)
       ('undefined (tuple 'error 401))
       (token
        (case (call mod 'authenticate token)
          ((tuple 'ok principal)
           (let ((scope (call mod 'scope principal)))
             (case (call mod 'authorize principal scope op)
               ('ok (tuple 'ok scope))
               (_   (tuple 'error 403)))))
          ((tuple 'error _) (tuple 'error 401))))))))
