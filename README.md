# sync-engine

A composable, domain-agnostic sync engine — written in
[LFE](https://lfe.io) (Lisp Flavored Erlang).

It is the offline-first sync core extracted from a reading app, rebuilt to
be reusable: drop it into any project that needs per-user state synced
across devices (readers, flashcard apps, trackers, note apps). Clone it,
implement a handful of callbacks, done.

## The idea

The engine never knows what your data *is*. It moves opaque **changes**
through a generic pipeline — identity, versioning, cursors, ordering,
pull/push — and you plug in the domain-specific parts as behaviours.

```
            transport (HTTP / SSE / WebSocket)
                          |
                     sync-engine          <- orchestration
                     /          \
              sync-core       sync-store   <- pure logic | storage seam
           (pure transforms)   (behaviour)
                                    |
                       sync-store-ets / -sqlite / ...
```

- **`sync-core`** — pure functions over change lists. No I/O, no state.
  The trivially testable heart.
- **`sync-engine`** — composes the core with a store; what transports call.
- **`sync-store`** — behaviour hiding where changes and cursors live.
- **`sync-store-ets`** — reference in-memory store.

## The change record

Every synced thing is one shape (`include/sync-records.lfe`):

```
id  scope  entity-type  entity-id  op  payload  version  device-id  timestamp
```

`payload` is opaque — the engine never inspects it. `op` is
`create | update | delete` (deletes are tombstones, never absence).
`scope` is the isolation unit (usually a user id).

## Extension points (behaviours)

| Behaviour       | Plug in...                          | Status  |
|-----------------|-------------------------------------|---------|
| `sync-store`    | storage (ETS / SQLite / Postgres)   | done (ETS, SQLite) |
| `sync-auth`     | authentication (JWT / API key)      | done (HS256 JWT)   |
| `sync-resolver` | conflict resolution                 | done (LWW)         |
| `sync-schema`   | entity types + payload validation   | planned |

## Build & test

Requires Erlang/OTP and LFE (`lfe`, `lfec`).

```sh
# quick check, no deps:
mkdir -p ebin
lfec -I include -o ebin src/*.lfe test/*.lfe
erl -noshell -pa ebin -eval "'sync-core-tests':run()." -s init stop

# or via rebar3 (pulls the rebar3_lfe plugin):
rebar3 lfe compile
```

Note: the test runner is invoked through `erl -noshell` rather than
`lfe -eval` — the LFE shell's terminal driver crashes under OTP 28 when
run non-interactively. The eunit suite (planned) runs via `rebar3 lfe test`.

## HTTP transport & realtime

A Cowboy adapter exposes the engine over JSON + SSE. The core stays
transport-free — `sync-http` only parses, calls the engine, serialises.

```
(sync-store-ets:init)
(sync-http:start 'sync-store-ets)                     ; :3001, dev (no auth)
(sync-http:start 'sync-store-ets 'sync-auth-jwt 8080) ; with JWT auth
```

| Method + path          | Body / params        | Response                              |
|------------------------|----------------------|---------------------------------------|
| `POST /sync/pull`      | `{cursor, limit}`    | `{changes:[..], next, hasMore}`       |
| `POST /sync/push`      | `{changes:[..]}`     | `{acked:[..], applied:[..], version}` |
| `GET  /sync/subscribe` | SSE stream           | `event: sync-updated` / `data:{version}` |
| `GET  /health`         | —                    | `{status:"ok"}`                       |

With auth configured the scope is taken from the JWT `sub` claim — a
caller cannot touch another scope. Without auth (dev mode) the scope
comes from the request body / `?scope=`.

**Realtime.** A push routes through `sync-scope` — a per-scope
`gen_server` that serialises version assignment — which then fans the
new version out over `pg` to every SSE subscriber of that scope. A
subscriber treats the event as a hint and issues a normal pull.
Connections hibernate when idle; a keepalive comment every 30s holds
them open through proxies.

## Principles

- **Push is a hint, pull is the truth.** Realtime is an optimisation
  over a system already correct via polling.
- **The core is pure.** Side effects live at the edges, behind behaviours.
- **Scope is generic.** Usually a user, but it can be a workspace,
  document, or team.

## Status

Working end to end: pure core, ETS + SQLite stores, orchestration,
Cowboy HTTP transport, SSE realtime fan-out (per-scope `gen_server` +
`pg`), HS256 JWT auth, LWW conflict resolution. Only `sync-schema`
(entity-type validation) remains planned.

## License

MIT — clone freely.
