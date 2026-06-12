;;;; sync-core-tests -- minimal self-contained test runner.
;;;;
;;;; Step-1 harness with no eunit dependency. Run with:
;;;;   lfe -pa ebin -eval "(sync-core-tests:run)" -s erlang halt
;;;; A proper eunit suite arrives once rebar3_lfe is wired (rebar3 lfe test).

(defmodule sync-core-tests
  (export (run 0) (run-http 0) (run-sqlite 0) (run-realtime 0) (run-blob 0)
          ;; materialize/2: this module doubles as a test materializer; it
          ;; records each change it sees into the `mat-test` ETS table.
          (materialize 2)))

(include-file "sync-records.lfe")

(defun check (name expected actual)
  (case (=:= expected actual)
    ('true  (io:format "  ok   ~s~n" (list name)))
    ('false (io:format "  FAIL ~s : expected ~p, got ~p~n"
                        (list name expected actual))
            (error 'test-failed))))

(defun run ()
  (io:format "sync-core~n")
  (test-assign-versions)
  (test-dedup)
  (test-pull)
  (test-pull-limit)
  (io:format "sync-store-ets~n")
  (test-store)
  (io:format "sync-engine~n")
  (test-engine-roundtrip)
  (io:format "sync-json~n")
  (test-json-roundtrip)
  (test-json-encode)
  (io:format "sync-resolver~n")
  (test-resolver)
  (io:format "sync-auth~n")
  (test-auth)
  (io:format "all tests passed~n")
  'ok)

(defun test-assign-versions ()
  (let ((cs (list (make-change id #"a" scope #"u" entity-id #"e1")
                  (make-change id #"b" scope #"u" entity-id #"e2"))))
    (case (sync-core:assign-versions cs 5)
      ((tuple versioned top)
       (check "assign top version" 7 top)
       (check "assign first"  6 (change-version (lists:nth 1 versioned)))
       (check "assign second" 7 (change-version (lists:nth 2 versioned)))))))

(defun test-dedup ()
  (let ((kept (sync-core:dedup
               (list (make-change id #"a" scope #"u")
                     (make-change id #"b" scope #"u"))
               (list #"a"))))
    (check "dedup count" 1 (length kept))
    (check "dedup kept"  #"b" (change-id (lists:nth 1 kept)))))

(defun test-pull ()
  (let ((cs (list (make-change id #"c3" scope #"u" version 3)
                  (make-change id #"c1" scope #"u" version 1)
                  (make-change id #"c2" scope #"u" version 2))))
    (case (sync-core:pull cs 1 10)
      ((tuple page next has-more)
       (check "pull count"       2 (length page))
       (check "pull ordered"     2 (change-version (lists:nth 1 page)))
       (check "pull next cursor" 3 next)
       (check "pull has-more"    'false has-more)))))

(defun test-pull-limit ()
  (let ((cs (list (make-change id #"c1" scope #"u" version 1)
                  (make-change id #"c2" scope #"u" version 2)
                  (make-change id #"c3" scope #"u" version 3))))
    (case (sync-core:pull cs 0 2)
      ((tuple page next has-more)
       (check "limit count"       2 (length page))
       (check "limit next cursor" 2 next)
       (check "limit has-more"    'true has-more)))))

(defun test-store ()
  (sync-store-ets:init)
  (sync-store-ets:append-changes
   #"s1"
   (list (make-change id #"x1" scope #"s1" version 1)
         (make-change id #"x2" scope #"s1" version 2)))
  (check "store current-version" 2 (sync-store-ets:current-version #"s1"))
  (check "store seen-ids"
         (list #"x1")
         (sync-store-ets:seen-ids #"s1" (list #"x1" #"nope")))
  (sync-store-ets:put-cursor #"s1" #"dev1" 2)
  (check "store cursor roundtrip" 2 (sync-store-ets:get-cursor #"s1" #"dev1"))
  (check "store cursor default"   0 (sync-store-ets:get-cursor #"s1" #"dev9")))

(defun test-engine-roundtrip ()
  (sync-store-ets:init)
  (let ((r (sync-engine:push
            'sync-store-ets #"s2"
            (list (make-change id #"e1" scope #"s2" entity-id #"n1" op 'create)
                  (make-change id #"e2" scope #"s2" entity-id #"n2" op 'create)))))
    (case r
      ((tuple 'ok result)
       (check "engine push version" 2 (maps:get 'version result))
       (check "engine push acked"   2 (length (maps:get 'acked result))))))
  ;; idempotent re-push of e1 must not mint a new version
  (let ((r2 (sync-engine:push
             'sync-store-ets #"s2"
             (list (make-change id #"e1" scope #"s2" entity-id #"n1" op 'create)))))
    (case r2
      ((tuple 'ok result)
       (check "engine idempotent version" 2 (maps:get 'version result)))))
  (case (sync-engine:pull 'sync-store-ets #"s2" 0 10)
    ((tuple page next has-more)
     (check "engine pull count"    2 (length page))
     (check "engine pull next"     2 next)
     (check "engine pull has-more" 'false has-more))))

(defun test-json-roundtrip ()
  (let* ((c  (make-change id #"j1" scope #"u" entity-type #"note"
                          entity-id #"n1" op 'create payload (map #"t" #"hi")
                          version 4 device-id #"d1" timestamp #"2026"))
         (c2 (sync-json:map->change (sync-json:change->map c))))
    (check "json id"      #"j1" (change-id c2))
    (check "json version" 4 (change-version c2))
    (check "json op"      #"create" (change-op c2))
    (check "json type"    #"note" (change-entity-type c2))
    (check "json payload" (map #"t" #"hi") (change-payload c2))))

(defun test-json-encode ()
  (let ((j (sync-json:pull-result->json (tuple '() 0 'false))))
    (check "json encode pull"
           'true
           (is_binary (erlang:iolist_to_binary j)))))

(defun test-resolver ()
  (let ((newer (make-change id #"r" version 5 timestamp #"2026-02"))
        (older (make-change id #"r" version 3 timestamp #"2026-01")))
    (check "resolver accepts newer"
           (tuple 'accept newer)
           (sync-resolver-lww:resolve newer older (map)))
    (check "resolver rejects stale"
           (tuple 'reject 'stale)
           (sync-resolver-lww:resolve older newer (map)))))

(defun test-auth ()
  (sync-auth-jwt:init #"super-secret-key")
  (let ((tok (sync-auth-jwt:sign (map #"sub" #"user-1") 3600)))
    (case (sync-auth-jwt:authenticate tok)
      ((tuple 'ok claims)
       (check "auth scope" #"user-1" (sync-auth-jwt:scope claims))
       (check "auth authorize" 'ok
              (sync-auth-jwt:authorize claims #"user-1" 'pull)))
      (other (check "auth ok" 'ok other)))
    (check "auth tamper rejected"
           'error
           (element 1 (sync-auth-jwt:authenticate
                       (erlang:iolist_to_binary (list tok #"x")))))
    (check "auth malformed rejected"
           'error
           (element 1 (sync-auth-jwt:authenticate #"not-a-jwt")))
    (check "auth wrong-secret rejected"
           'error
           (element 1
            (progn
              (sync-auth-jwt:init #"different-secret")
              (let ((r (sync-auth-jwt:authenticate tok)))
                (sync-auth-jwt:init #"super-secret-key")
                r)))))
  (let ((expired-tok (sync-auth-jwt:sign (map #"sub" #"u") -10)))
    (check "auth expired rejected"
           (tuple 'error 'expired)
           (sync-auth-jwt:authenticate expired-tok))))

;;; --- HTTP smoke test -------------------------------------------------
;;; Needs cowboy + inets on the path. Run separately:
;;;   erl -noshell -pa ebin -pa _build/default/lib/*/ebin \
;;;       -eval "'sync-core-tests':'run-http'()." -s init stop

(defun http-post (url body)
  (case (httpc:request
         'post
         (tuple url (list) "application/json" (erlang:iolist_to_binary body))
         (list)
         (list))
    ((tuple 'ok (tuple (tuple _ status _) _ resp))
     (tuple status (json:decode (erlang:list_to_binary resp))))
    (other (tuple 'error other))))

(defun run-http ()
  (application:ensure_all_started 'cowboy)
  (application:ensure_all_started 'inets)
  (sync-store-ets:init)
  (sync-http:start 'sync-store-ets 3010)
  (io:format "sync-http (live, port 3010)~n")
  (let ((push-res
         (http-post "http://localhost:3010/sync/push"
                    (json:encode
                     (map #"scope" #"h1"
                          #"changes"
                          (list (map #"id" #"hc1" #"entityType" #"note"
                                     #"entityId" #"n1" #"op" #"create"
                                     #"payload" (map #"x" 1))
                                (map #"id" #"hc2" #"entityType" #"note"
                                     #"entityId" #"n2" #"op" #"create"
                                     #"payload" (map #"x" 2))))))))
    (case push-res
      ((tuple 200 pm)
       (check "http push version" 2 (maps:get #"version" pm))
       (check "http push acked"   2 (length (maps:get #"acked" pm))))
      (other (check "http push" 'ok other))))
  (let ((pull-res
         (http-post "http://localhost:3010/sync/pull"
                    (json:encode (map #"scope" #"h1" #"cursor" 0 #"limit" 10)))))
    (case pull-res
      ((tuple 200 qm)
       (check "http pull count"   2 (length (maps:get #"changes" qm)))
       (check "http pull next"    2 (maps:get #"next" qm))
       (check "http pull hasMore" 'false (maps:get #"hasMore" qm)))
      (other (check "http pull" 'ok other))))
  (sync-http:stop)
  (io:format "http smoke passed~n")
  'ok)

;;; --- SQLite store test ----------------------------------------------
;;; Needs esqlite on the path. Run separately:
;;;   erl -noshell -pa ebin -pa _build/default/lib/*/ebin \
;;;       -eval "'sync-core-tests':'run-sqlite'()." -s init stop

(defun run-sqlite ()
  (sync-store-sqlite:init ":memory:")
  (io:format "sync-store-sqlite~n")
  (sync-store-sqlite:append-changes
   #"q1"
   (list (make-change id #"a" scope #"q1" entity-type #"note"
                      entity-id #"e1" op 'create payload (map #"n" 1) version 1)
         (make-change id #"b" scope #"q1" entity-type #"note"
                      entity-id #"e2" op 'create payload (map #"n" 2) version 2)))
  (check "sqlite current-version" 2 (sync-store-sqlite:current-version #"q1"))
  (check "sqlite seen-ids"
         (list #"a")
         (sync-store-sqlite:seen-ids #"q1" (list #"a" #"zz")))
  (case (sync-store-sqlite:read-since #"q1" 0 10)
    ((tuple page next has-more)
     (check "sqlite read count"    2 (length page))
     (check "sqlite read next"     2 next)
     (check "sqlite read has-more" 'false has-more)
     (check "sqlite payload roundtrip"
            (map #"n" 1)
            (change-payload (lists:nth 1 page)))))
  (case (sync-store-sqlite:read-since #"q1" 0 1)
    ((tuple page _next has-more)
     (check "sqlite limit count"    1 (length page))
     (check "sqlite limit has-more" 'true has-more)))
  (sync-store-sqlite:put-cursor #"q1" #"d1" 5)
  (check "sqlite cursor roundtrip" 5 (sync-store-sqlite:get-cursor #"q1" #"d1"))
  (check "sqlite cursor default"   0 (sync-store-sqlite:get-cursor #"q1" #"d9"))
  ;; engine end-to-end against the sqlite store
  (let ((r (sync-engine:push
            'sync-store-sqlite #"q2"
            (list (make-change id #"p1" scope #"q2" entity-id #"e"
                               op 'create payload (map #"k" #"v"))))))
    (case r
      ((tuple 'ok res)
       (check "sqlite engine push version" 1 (maps:get 'version res)))))
  (case (sync-engine:pull 'sync-store-sqlite #"q2" 0 10)
    ((tuple page _next _hm)
     (check "sqlite engine pull count" 1 (length page))))
  ;; observability callbacks
  (check "sqlite count-changes" 2 (sync-store-sqlite:count-changes #"q1"))
  (check "sqlite list-cursors"
         (list (tuple #"d1" 5))
         (sync-store-sqlite:list-cursors #"q1"))
  (sync-store-sqlite:close)
  (io:format "sqlite tests passed~n")
  'ok)

;;; --- blob channel + materializer + rate-limit -----------------------
;;; Run separately:
;;;   erl -noshell -pa ebin -pa _build/default/lib/*/ebin \
;;;       -eval "'sync-core-tests':'run-blob'()." -s init stop

(defun materialize (scope change)
  (ets:insert 'mat-test (tuple (change-id change) scope))
  'ok)

(defun run-blob ()
  (sync-store-sqlite:init ":memory:")
  (sync-blob-sqlite:ensure-schema)
  (io:format "sync-blob-sqlite~n")
  (let* ((bytes #"hello blob world")
         (hash  (sync-blob-sqlite:sha256-hex bytes)))
    ;; put / get
    (check "blob put ok" 'ok
           (sync-blob-sqlite:put-blob #"b1" hash #"text/plain" bytes))
    (check "blob hash-mismatch rejected"
           (tuple 'error 'hash-mismatch)
           (sync-blob-sqlite:put-blob #"b1" #"deadbeef" #"text/plain" bytes))
    (check "blob get roundtrip"
           (tuple 'ok #"text/plain" bytes)
           (sync-blob-sqlite:get-blob #"b1" hash))
    (check "blob get miss" 'not-found
           (sync-blob-sqlite:get-blob #"b1" #"00ff"))
    ;; have-blobs (batch check)
    (check "blob have subset"
           (list hash)
           (sync-blob-sqlite:have-blobs #"b1" (list hash #"nope")))
    ;; scope isolation
    (check "blob scope isolated" 'not-found
           (sync-blob-sqlite:get-blob #"other" hash))
    ;; manifest
    (sync-blob-sqlite:put-manifest
     #"b1" #"book" #"bk1"
     (list (map #"hash" hash #"ord" 0 #"meta" (map #"href" #"img/1.png"))))
    (case (sync-blob-sqlite:get-manifest #"b1" #"book" #"bk1")
      ((list ref)
       (check "manifest hash"    hash (maps:get #"hash" ref))
       (check "manifest present" 'true (maps:get #"present" ref))
       (check "manifest size"    16 (maps:get #"size" ref))
       (check "manifest meta"    (map #"href" #"img/1.png") (maps:get #"meta" ref))))
    ;; stats
    (check "blob stats"
           (map #"count" 1 #"totalBytes" 16)
           (sync-blob-sqlite:blob-stats #"b1"))
    ;; gc: an unreferenced blob is collected, the referenced one survives
    (let ((orphan #"orphan bytes"))
      (sync-blob-sqlite:put-blob #"b1" (sync-blob-sqlite:sha256-hex orphan)
                                 #"application/octet-stream" orphan)
      (check "blob gc count" 1 (sync-blob-sqlite:gc-unreferenced #"b1"))
      (check "blob gc kept referenced"
             (tuple 'ok #"text/plain" bytes)
             (sync-blob-sqlite:get-blob #"b1" hash))))
  ;; materializer: a push projects into the registered module
  (io:format "sync-materializer~n")
  (ets:new 'mat-test (list 'named_table 'public 'set))
  (sync-materializer:set 'sync-core-tests)
  (sync-engine:push
   'sync-store-sqlite #"m1"
   (list (make-change id #"mc1" scope #"m1" entity-id #"e" op 'create)))
  (check "materializer saw change"
         (list (tuple #"mc1" #"m1"))
         (ets:lookup 'mat-test #"mc1"))
  (sync-materializer:set 'undefined)
  ;; rate limiter: 3rd hit in a window of 2 is limited
  (io:format "sync-rate-limit~n")
  (let ((tier (tuple 2 60000 'scope-device))
        (ctx  (map 'scope #"u1" 'device-id #"dA")))
    (check "rl first ok"  'ok (element 1 (sync-rate-limit:check-tier tier ctx)))
    (check "rl second ok" 'ok (element 1 (sync-rate-limit:check-tier tier ctx)))
    (check "rl third limited" 'rate_limited
           (element 1 (sync-rate-limit:check-tier tier ctx)))
    ;; a different device has its own budget
    (check "rl other device ok" 'ok
           (element 1 (sync-rate-limit:check-tier
                       tier (map 'scope #"u1" 'device-id #"dB")))))
  (sync-store-sqlite:close)
  (io:format "blob/materializer/rate-limit tests passed~n")
  'ok)

;;; --- realtime test (scope gen_server + pg fan-out + SSE) ------------
;;; Needs cowboy + esqlite paths. Run separately:
;;;   erl -noshell -pa ebin -pa _build/default/lib/*/ebin \
;;;       -eval "'sync-core-tests':'run-realtime'()." -s init stop

(defun http-post-auth (url token body)
  (case (httpc:request
         'post
         (tuple url
                (list (tuple "authorization"
                             (binary_to_list
                              (erlang:iolist_to_binary
                               (list #"Bearer " token)))))
                "application/json"
                (erlang:iolist_to_binary body))
         (list)
         (list))
    ((tuple 'ok (tuple (tuple _ status _) _ resp))
     (tuple status (json:decode (erlang:list_to_binary resp))))
    (other (tuple 'error other))))

(defun binary-contains (haystack needle)
  (case (binary:match haystack needle)
    ('nomatch 'false)
    (_ 'true)))

(defun recv-until (sock needle timeout)
  (recv-until sock needle timeout #""))

(defun recv-until (sock needle timeout acc)
  (if (binary-contains acc needle)
    (tuple 'ok acc)
    (case (gen_tcp:recv sock 0 timeout)
      ((tuple 'ok data)
       (recv-until sock needle timeout
                   (erlang:iolist_to_binary (list acc data))))
      ((tuple 'error _) (tuple 'timeout acc)))))

(defun test-sse (port token)
  (case (gen_tcp:connect "localhost" port (list 'binary (tuple 'active 'false)))
    ((tuple 'ok sock)
     (gen_tcp:send
      sock
      (erlang:iolist_to_binary
       (list "GET /sync/subscribe HTTP/1.1\r\n"
             "Host: localhost\r\n"
             "Cookie: sync_token=" token "\r\n\r\n")))
     (case (recv-until sock #"event: ready" 3000)
       ((tuple 'ok head)
        (check "sse status 200"   'true (binary-contains head #"200"))
        (check "sse content-type" 'true (binary-contains head #"text/event-stream"))
        (check "sse ready event"  'true (binary-contains head #"event: ready"))
        ;; a push on the same scope must arrive as a live event
        (http-post-auth
         (++ "http://localhost:" (integer_to_list port) "/sync/push")
         token
         (json:encode (map #"changes"
                           (list (map #"id" #"sse-live" #"op" #"create"
                                      #"entityType" #"note"
                                      #"payload" (map #"x" 9))))))
        (case (recv-until sock #"sync-updated" 3000)
          ((tuple 'ok _evt) (check "sse live event" 'true 'true))
          ((tuple 'timeout _) (check "sse live event" 'true 'false))))
       ((tuple 'timeout _)
        (check "sse ready event" 'true 'false)))
     (gen_tcp:close sock))
    (other (check "sse connect" 'ok other))))

(defun run-realtime ()
  (application:ensure_all_started 'cowboy)
  (application:ensure_all_started 'inets)
  (sync-http:ensure-infra)
  (sync-store-ets:init)
  (sync-auth-jwt:init #"realtime-secret")
  ;; (1) pg fan-out via the scope gen_server
  (io:format "realtime fan-out~n")
  (let ((parent (self)))
    (spawn (lambda ()
             (pg:join (sync-scope:pg-scope) #"r1" (self))
             (! parent 'joined)
             (receive
               ((tuple 'sync-updated v) (! parent (tuple 'got v)))
               (after 3000 (! parent (tuple 'got 'timeout))))))
    (receive ('joined 'ok) (after 2000 'ok))
    (sync-scope:push
     #"r1" 'sync-store-ets
     (list (make-change id #"rc1" scope #"r1" entity-id #"e" op 'create)))
    (receive
      ((tuple 'got v) (check "fan-out version" 1 v))
      (after 4000 (check "fan-out" 'received 'timeout))))
  ;; (2) HTTP with JWT auth
  (io:format "realtime http auth~n")
  (sync-http:start 'sync-store-ets 'sync-auth-jwt 3011)
  (let ((tok (sync-auth-jwt:sign (map #"sub" #"rt-user") 3600)))
    (case (http-post-auth
           "http://localhost:3011/sync/push" tok
           (json:encode (map #"changes"
                             (list (map #"id" #"ac1" #"entityType" #"note"
                                        #"op" #"create"
                                        #"payload" (map #"v" 1))))))
      ((tuple 200 pm) (check "authed push version" 1 (maps:get #"version" pm)))
      (other (check "authed push" 'ok other)))
    (check "unauthed push 401"
           401
           (element 1 (http-post "http://localhost:3011/sync/push"
                                 (json:encode (map #"changes" '())))))
    ;; (3) SSE over a raw socket
    (io:format "realtime sse~n")
    (test-sse 3011 tok))
  (sync-http:stop)
  (io:format "realtime tests passed~n")
  'ok)
