# oxygen-http MCP — Feature Plan

> Status: **Phases 1–5 implemented & green (23 new tests; no regressions); wired into the example web
> server via an env-resolved `EndpointMiddlewares` builder; MCP badge rendered in the docs UI.** See "What landed".
> Modules: `modules/http/zio` (oxygen-http), `modules/general/schema` (oxygen-schema), `modules/crypto/model` + `modules/crypto/service` (auth)
> Target MCP spec revision: **`2025-11-25`** (current stable). The `2026-07-28` release candidate
> (stateless core, Extensions, Tasks, MCP Apps) is a watch-item, not a target.

## Implementation status

| Phase | Status | What it covers |
|---|---|---|
| 1 — JSON Schema (draft 2020-12) emitter | ✅ done, 5 tests | `oxygen.schema.compiled.JsonSchemaEmitter`: compiled schema graph → standard JSON Schema (`$defs`/`$ref` for products/sums). |
| 2 — `@mcp.tool`/`@mcp.auth` + `McpEndpoint` derivation | ✅ done | Macro reads the annotations, populates `EndpointSchema.mcp`, compile-errors on streaming, threads into the docs spec. |
| 3 — `McpToolCodec` + JSON-RPC dispatcher + middleware | ✅ done, 6 tests | `McpToolCodec[In]` (decode JSON args → `In`, no HTTP transform), `DeriveMcpTools`/`McpTool`, `McpServer` dispatch, `McpEndpointMiddleware`. |
| 4 — Auth (OAuth 2.1 Resource Server) | ✅ done, 6 tests | `RegisteredClaims`/`Scopes`/`Audience`, `McpAuthService` (Live/NoAuth), per-message 401/403 gate, RFC 9728 PRM. JWT-only. |
| 5 — OIDC UserInfo / JIT provisioning | ✅ model + service | `OidcUserInfo` model (3 tests) + `UserInfoService` (ZIO service: `GET userinfo_endpoint` + bearer → `OidcUserInfo`). JIT-provisioning design below. |
| Wiring — `EndpointMiddlewares` builder + example | ✅ done, 3 tests | `EndpointMiddlewares.empty.add(...).addMcp[Api](...)` (mirrors `Endpoints`; resolves the API impl + `McpAuthService` from the env). Example `NoteApi` (Ref-backed, auth-less) served over HTTP **and** MCP in `WebServerMain`. |

## What landed (autonomous build — 2026-06-25 night)

**All green: 23 new tests** — 5 emitter (`utJVM`), 6 `McpServerSpec` + 6 `McpAuthServiceSpec` (`http-it`), 3 `OidcUserInfoSpec` (`crypto-model`), 3 `NoteMcpSpec` (`example-web-server`). No regressions: full `http-it` and `crypto-service` suites still pass; JVM **and** JS cross-compile clean (the docs-UI badge included).

Files added:
- `modules/general/schema/.../compiled/JsonSchemaEmitter.scala` (+ `JsonSchemaEmitterSpec`)
- `modules/http/zio/.../core/annotations.scala` — `@mcp.tool` / `@mcp.tool("name")` / `@mcp.auth`
- `modules/http/zio/.../schema/McpEndpoint.scala`; `core/McpToolCodec.scala`
- `modules/http/zio/.jvm/.../server/mcp/` — `McpTool` (+ `AppliedMcpTool`), `DeriveMcpTools`, `McpServer` (API-erased), `McpEndpointMiddleware`, `McpAuthService`, `UserInfoService`
- `modules/http/zio/.jvm/.../server/EndpointMiddlewares.scala` — env-resolved middleware builder; `CompiledEndpoints.endpointLayerFromEnv`
- `modules/crypto/model/.../{RegisteredClaims,Scopes,OidcUserInfo}.scala`
- it-tests: `McpServerSpec`, `McpAuthServiceSpec`; `crypto-model`: `OidcUserInfoSpec`
- example: `example/.../mcp/NoteApi.scala` (Ref-backed, auth-less, 3 `@mcp.tool` routes) + `NoteMcpSpec`; `WebServerMain` wiring
- docs UI: `ApiSpecPage.scala` — violet MCP badge per opted-in endpoint
- (edited) `RouteRepr.scala` (derivation), `EndpointSchema`/`DerivedServerEndpointImpl`/`EndpointRepr`/`model.scala`/`CompiledApiSpec.scala` (thread `mcp`), `build.sbt` (http JVM → crypto-service)

Autonomous decisions worth knowing:
- **Scopes are resource-server-wide, configured on `McpAuthConfig.requiredScopes`** — not per-tool/per-annotation. `@mcp.auth` stays a no-arg marker ("this tool needs a valid token"); the dispatcher enforces `requiredScopes ⊆ token.scope` for every authed tool (`403 insufficient_scope`) and advertises them in the PRM `scopes_supported`. This deliberately replaces the originally-planned per-tool annotation scopes (the `FromExprT` varargs work) — simpler, and the 403 path is now live + tested (`McpAuthServiceSpec`). Per-tool overrides can be layered on later without breaking this. (The dead `requiredScopes` fields on `McpEndpoint.Auth`/`McpTool`/`AppliedMcpTool` were removed.)
- **Tool name default is `apiName_endpointName`** (e.g. `UserApi_userById`); override with `@mcp.tool("name")`.
- **`McpToolCodec` requires `JsonSchema[A]` for each non-auth param and `PlainTextSchema[A]` for the `@mcp.auth` param** (the latter decodes the bearer string). Missing instances → clear derivation error.
- **`oxygen-http` JVM now depends on `oxygen-crypto-service`** (via `.jvmConfigure` — there was no prior example in `build.sbt`; it compiles).

Rough edges / not yet done (none block the core; all flagged for us to finish together):
- **Verified against a real MCP client (Claude Code) for the unauthed path** — `initialize`/`tools/list`/`tools/call` work end-to-end (this surfaced + fixed a missing `Content-Type` header and the `structuredContent`-must-be-object requirement). The authed path is not yet exercised against a live client.
- **DNS-rebinding `Origin` guard not implemented** — the spec recommends a resource server reachable from a browser context respond `403` to disallowed `Origin` headers. Not yet wired (no `Origin` check in `McpEndpointMiddleware`); deferred. `MCP-Protocol-Version` request-header handling is likewise not implemented (defensible for a stateless server).
- **Per-tool scope overrides** — scopes are currently server-wide (above); a future `@mcp.tool` scope override is possible but unbuilt.
- **`UserInfoService` live HTTP fetch is compiled but not exercised by a test** — `OidcUserInfo` JSON parsing *is* tested; the `GET userinfo_endpoint` round-trip would need an HTTP stub. JIT provisioning *hook* wiring (what to do with the fetched profile) is still design-only — see the UserInfo section.
- **`userinfo_endpoint` is configured directly**, not discovered from AS metadata yet.

---

## Goal

Auto-generate an **MCP server** from the endpoint schema oxygen-http already derives per endpoint —
the same way [`http-feature-plan.md`](../http-docs/http-feature-plan.md) auto-generated docs from it.
A user **opts a route in** with an `@mcp.tool` annotation; each opted-in endpoint becomes an MCP
**tool**, served on the same HTTP server.

> **Consumers:** Claude Code (as a remote MCP server) and Claude.ai web/desktop (as a custom
> connector). Both reach a remote HTTP MCP server; that is the only transport we target.

### Decisions locked

- **Transport: Streamable HTTP only.** No stdio. (This is an add-on to an already-launched HTTP
  server; stdio/local-spawn is explicitly out of scope.)
- **No streaming.** Every JSON-RPC response is a single `application/json` body — we never open an
  SSE stream. SSE is optional in the spec and we use none of the features that need it (progress
  notifications, server→client requests, `listChanged`).
- **SSE / JSON-stream endpoints hard-error at *compile time*.** `ReturningSSE` /
  `ResponseBodySchema.{ServerSentEvents, LineStream}` **cannot** be exposed as MCP tools — `tools/call`
  returns a single result, which has no clean analog for a streaming endpoint. Because MCP is a derived
  facet (see below), `@mcp.tool` on a streaming route **fails at macro expansion** — the strongest "you
  can't MCP-expose this," caught at build, not runtime. (Revisit post-`2026-07-28` via the Tasks extension.)
- **No JSON-RPC batching.** Batching was removed in the `2025-06-18` revision (still gone in
  `2025-11-25`). One JSON-RPC message per HTTP POST; the dispatcher never handles request arrays.
  "Parallel tool calls" is a *client* concept — each becomes a separate `tools/call` POST.
- **Stateless — no `Mcp-Session-Id`.** Each JSON-RPC call is an independent POST; we hold no
  per-session state (auth is per-request bearer, protocol version rides the per-request
  `MCP-Protocol-Version` header). So we never issue a session id, and the route at a fixed `/mcp` is
  fully stateless. Matches the `2026-07-28` RC's stateless-core direction.
- **Input-validation failures are Tool Execution Errors, not Protocol Errors** (SEP-1303): when args
  fail `inputSchema`, return a `CallToolResult` with `isError: true`, *not* a JSON-RPC error — lets the
  model self-correct. (A `403`-on-disallowed-`Origin` DNS-rebinding guard was planned but is **deferred /
  not yet implemented** — see "Rough edges".)
- **Surface: tools only.** Each opted-in endpoint → one MCP tool. No `resources`, no `prompts`.
  (Tool = model-invoked action; resource = app/user-curated read-only context addressed by URI. A
  parameterized `GET` is a tool, not a resource — resources would only fit fixed, curated, listable
  data, which is not what oxygen-http endpoints are. MCP also has no "skills" concept — that is a
  Claude concept, not part of MCP.)
- **Opt-in via the `@mcp.*` annotation family** (nested, mirroring `@route.get` / `@param.path`):
  - `@mcp.tool` — expose the route; tool name defaults to `apiName_endpointName` (dedupe on collision).
  - `@mcp.tool("customName")` — same, explicit name override.
  - `@mcp.auth` — marks the param fed by the validated bearer.
  Only `@mcp.tool` routes are exposed (opt-in, not opt-out).
- **Wiring: an `EndpointMiddleware`**, mirroring `ApiSpecEndpointMiddleware` — read every endpoint's
  `EndpointSchema` at startup, build tool definitions for the `@mcp.tool` ones, and append the MCP
  endpoint(s). The served set naturally excludes the MCP endpoint itself.
- **Tool input = the endpoint's parameter list as one JSON object** — one property per `@param`, keyed
  by the function parameter name, typed via the JSON Schema emitter. The model never sees the HTTP
  shape; the path/query/header/body split is internal middleware metadata used to reconstruct the
  `EndpointInput`. A product-typed body **nests under its param name** (avoids name collisions with
  query/header params). 1:1 with the function signature.
- **MCP is a first-class derived facet.** The endpoint-deriving macro reads `@mcp.tool` / `@mcp.auth`
  (alongside `@route` / `@param` / `@httpDoc`) and populates **`EndpointSchema.mcp: Option[McpEndpoint]`**.
  `McpEndpoint` (in the shared http-schema package, so the compiled spec + docs can see it) carries the
  MCP-specific extras: `toolName` and `auth: Option[{ authParamName }]` (scopes are **server-wide**
  config, not per-tool — see Auth below). The flattened `inputSchema` is still computed from the existing
  `RequestSchema` — not duplicated. The MCP middleware just **consumes `endpoint.mcp`** (no annotation
  re-parsing). `CompiledApiSpec.compile` threads `mcp: Option[McpEndpoint]` into `RawCompiledEndpoint`,
  and `ApiSpecPage` renders an **MCP badge** next to the existing method/status badges.
- **Auth: go straight for OAuth — JWT bearer only.** MCP server is a pure OAuth 2.1 Resource Server,
  stateless: it does **not** issue tokens, run any OAuth flow, handle refresh, or store credentials.
  Its burden is: serve Protected Resource Metadata, challenge with `401` + `WWW-Authenticate`, and
  validate audience-bound **JWT** bearer tokens. Opaque-token introspection is out of scope for v1
  (documented constraint: the AS must issue JWTs).
- **Auth reuses `modules/crypto` — zero external deps.** Validation = `BearerTokenService.Validator`
  (signature; holds a `HashKey.CanValidate`) + our claims gate. HS256 (`HashKey.Hmac`, shared secret,
  for local testing) and RS256 (`HashKey.RsaPublic`, validate-only with the AS's public key, for
  Okta/Entra/etc.) are both already supported, all JDK-backed.
- **New `JWT.RegisteredClaims`** (in `oxygen.crypto.model`, beside `StandardPayload`) models the loose
  RFC 7519 registered claims as third parties actually emit them — `iss`/`sub`/`aud`/`exp`/`nbf`/`iat`/
  `jti` (all optional; **`jti` is a `String`, not a UUID**; **`aud` decodes string-or-array**) plus
  OAuth `scope: Option[Scopes]`. This is the *consumption/validation* shape; `StandardPayload` stays the
  *issuance* shape, untouched. `Scopes` is a typed space-delimited set (`containsAll` for subset checks).
- **`McpAuthService` (`Live` | `NoAuth`)** is the middleware's auth dependency.
  - `Live` decodes the bearer → `JWT[RegisteredClaims]` (the model carries `.token: BearerToken`),
    validates the signature via `BearerTokenService.Validator`, then the **server-global** claims:
    `exp`/`nbf`, `aud` = our resource URI, `iss` = configured AS → returns `RegisteredClaims`. These are
    the **401-class** failures. Config: `McpAuthConfig{ validationKey: HashKey.CanValidate, expectedIssuer,
    audience, … }`.
  - **Scope authorization is server-wide** (`McpAuthConfig.requiredScopes`), enforced by the dispatcher
    for every authed tool: `requiredScopes ⊆ claims.scope` → else **`403 insufficient_scope`**, and the
    set is advertised in the PRM `scopes_supported`. Clean 401 (*who you are*) vs 403 (*what you may do*)
    split. (Originally planned per-tool/per-annotation; simplified to server-wide — per-tool overrides
    can be added later.)
  - `NoAuth` requires no config; but at **middleware apply-time**, if any exposed endpoint is authed
    while `NoAuth` is wired, **fail to start** (`ZIO.die` — `EndpointMiddleware.apply` is `URIO`, no error
    channel) with a message naming the offending endpoints. Servers with no authed MCP endpoints run
    `NoAuth` with zero config.
- **`@mcp.auth` threading.** The annotated param is **stripped from `inputSchema`** (model must not
  supply a token); the validated raw bearer is **re-injected into the reconstructed request's
  `Authorization` header**, so the endpoint's *own* auth param decodes it exactly as for a real HTTP
  request (the handler receives whatever its param type already produces — e.g. `JWT.Std[AppClaims]`).
  MCP gates on `RegisteredClaims`; the endpoint may decode richer app claims on top.
- **Mixed auth works per-message.** In Streamable HTTP each JSON-RPC call is its own HTTP POST, so:
  `initialize` / `tools/list` / public `tools/call` → `200`; an authed `tools/call` with no/invalid
  bearer → `401` + `WWW-Authenticate` on *that* POST → client runs OAuth, retries that call with a
  token. The gate is per-`tools/call`, not a blanket gate on the whole route. **If no `@mcp.auth` param
  exists anywhere, the server serves no PRM, never 401s, needs zero auth config.**

---

## How MCP works (the parts that constrain us)

### Protocol
JSON-RPC 2.0 over HTTP. A session opens with an **`initialize`** handshake (protocol-version + capability
negotiation), then normal request/response. The whole server, at the protocol level, is a small
dispatcher over four methods:

| Method | Handling |
|---|---|
| `initialize` | Respond with `protocolVersion`, `capabilities: { tools: {} }`, `serverInfo`. The `tools: {}` declaration is what authorizes the client to call `tools/list` / `tools/call`. |
| `notifications/initialized` | No-op ack (notification — no response). |
| `tools/list` | Enumerate endpoints-as-tools: `{ name, description, inputSchema }`. |
| `tools/call` | Flatten args → reconstruct the internal HTTP call → run the existing handler → return a single `CallToolResult`. |

HTTP-transport details that ride along: after `initialize`, requests carry an `MCP-Protocol-Version`
header; the stateful flavor uses an `Mcp-Session-Id` header (server sets it on the initialize response,
client echoes it). The `2026-07-28` RC trends stateless, so we keep session handling minimal.

### Where headers go (drives the param-flattening design)
Three layers of HTTP, only one carries model-chosen data:
- **Protocol headers** (fixed): `Authorization`, `MCP-Protocol-Version`, `Mcp-Session-Id`, `Accept`,
  `Content-Type`.
- **Static custom headers**: the *user* can configure fixed headers in their client config
  (per-connection constant, e.g. an API key). Not model-driven.
- **Per-call data**: everything the model decides lives in the tool's `arguments` JSON object,
  validated against `inputSchema`. **There is no way for the model to set an HTTP header per call.**

**Consequence:** when mapping an endpoint → tool, **all** of `@param.path` / `@param.query` /
`@param.header` / `@param.body` collapse into the single tool-input object; the middleware re-splits
them into a real internal HTTP call. `@param.header` is the awkward case (an auth-style header param
should *not* become a tool argument).

### Auth flow + storage (why the server stays stateless)
Three parties: **MCP client** (Claude Code / Claude.ai), **Authorization Server** (Okta/GitHub/Entra/
own — *out of scope*, never our server), and **our MCP server** (Resource Server).

1. Client hits the server with no token → `401` + `WWW-Authenticate: Bearer resource_metadata="…", scope="…"`.
2. Client reads our Protected Resource Metadata → finds the `authorization_servers` → discovers the AS.
3. Client runs the OAuth flow against the **AS** (login, consent, PKCE) → gets an audience-bound access token (+ refresh token).
4. Client retries with `Authorization: Bearer <token>` → we validate and serve.

| Artifact | Stored by |
|---|---|
| Access + refresh tokens | The **client** (Claude Code: locally; Claude.ai: Anthropic server-side, per connector). |
| User identity, sessions, consent, token lifecycle, **refresh** | The **Authorization Server**. |
| **Nothing auth-related** | **Our MCP server** — validates the bearer per request, holds no auth state. |

**Refresh is entirely client ↔ AS.** Our only role in refresh is to keep returning `401` on
expired/invalid tokens; the client refreshes against the AS and retries. We implement no refresh logic.

**When does the client authenticate?** Mechanically lazy (401-driven). Because each JSON-RPC call is
its own HTTP POST, the gate is **per-`tools/call`**, not on the whole route: `initialize` / `tools/list`
/ public tools answer `200`, and the first authed `tools/call` returns `401`, kicking off discovery
then. (See "Risks to verify" — this assumes clients re-auth on a mid-session 401.)

---

## Current State — what oxygen-http already gives us

The auto-docs work did most of the structural heavy lifting. Reuse points:

- **`RawCompiledApiSpec` / `RawCompiledEndpoint`** (`modules/http/zio/.../schema/compiled/model.scala`)
  — a complete, walkable, serializable model of every endpoint: method, path segments, query/header
  params, body, with each type a `CompiledSchemaRef` into a shared bundle. Each `RawCompiledParam`
  carries `name` / `doc` / `kind` (Required/Optional/Many) / `schema` — exactly the metadata a tool
  definition needs.
- **`CompiledApiSpec.compile(Seq[EndpointSchema])`** (`.jvm/.../schema/compiled/CompiledApiSpec.scala`)
  — the pure-runtime compiler; feed it `AppliedEndpoints.map(_.schema)`.
- **`ApiSpecEndpointMiddleware`** (`.jvm/.../server/ApiSpecEndpointMiddleware.scala`) — the template
  for the MCP middleware: `EndpointMiddleware.apply(AppliedEndpoints): URIO[Scope, AppliedEndpoints]`,
  reads schemas, appends an `AppliedEndpoint`.
- **Handler dispatch** — `AppliedEndpoint.handle: EndpointInput => Option[URIO[Scope, Option[Response]]]`
  already runs an endpoint end-to-end. `tools/call` ultimately routes through this.
- **JSON codec layer** — `oxygen.json.JsonCodec` for the JSON-RPC envelope and tool args.

Plus the existing **`modules/crypto`** — `BearerTokenService.Validator` (signature, HS256 + RS256 via
`HashKey.CanValidate`), `JWT[A]` (carries the underlying `BearerToken`), `JWT.StandardPayload`
(issuance shape). All JDK-backed, no external deps.

### Macro working notes (from the author — heed these)
- **Never use `private` on anything the macros touch** — it makes the macros "lose their minds" (see
  commit `eac32c1`). This is the one real gotcha.
- **`sbt clean` when a macro seems to ignore an edit** — stale macro cache, not a real bug. Reach for it
  before debugging.
- **Crib from existing macros — oxygen is full of them.** `grep -rl 'import oxygen.meta'` /
  `'import oxygen.quoted'`. Best templates: `RouteRepr` / `ParamRepr` / `ApiRepr` (annotation reading +
  endpoint derivation — what `@mcp.*` extends) and `JsonSchema.derived` (product/sum reflection — what
  the schema emitter mirrors). Don't invent macro patterns; reuse.

### The gaps (the actual new work)
1. **JSON Schema (draft 2020-12) emitter.** MCP tool `inputSchema` must be standard JSON Schema. The
   compiled schema layer (`RawCompiledJsonSchema` et al.) is oxygen's *own* format — `grep` finds no
   `$schema` / draft / `inputSchema` emitter anywhere. This is the biggest new piece. (Likely emit
   from `JsonSchema[A]` or from `RawCompiledJsonSchema`.)
2. **`@mcp.tool` / `@mcp.auth` annotations + `McpEndpoint` derivation.** New annotation family in the
   http annotations file; macro populates `EndpointSchema.mcp: Option[McpEndpoint]` and compile-errors on
   a streaming route. Thread a small `mcp` field into `RawCompiledEndpoint` + an MCP badge in `ApiSpecPage`.
3. **JSON-RPC layer.** A small request/response/notification envelope + the 4-method dispatcher. MCP
   is not REST; routing/dispatch we already have, the envelope we don't.
4. **REST→tool flattening.** Map the param list to one `arguments` object (keyed by function param
   name, body nested under its name), plus the reverse to reconstruct the `EndpointInput`. The
   `@mcp.auth` param is stripped and the validated bearer re-injected into the `Authorization` header.
5. **Auth claims layer.** `JWT.RegisteredClaims` + `Scopes` in `oxygen.crypto.model`; `McpAuthService`
   (`Live` | `NoAuth`) wrapping `BearerTokenService.Validator`. No new deps.

---

## Plan (phased)

### Phase 1 — JSON Schema (draft 2020-12) emitter
- Convert an oxygen schema (`JsonSchema[A]` / `RawCompiledJsonSchema`) → JSON Schema draft 2020-12.
- Cover the shapes the compiled model already produces: products→objects (+ `required`), sums→`oneOf`
  with discriminator, arrays, maps, option/nullable, plain-text/encoded leaves.
- This is independently useful (it's standard JSON Schema) and unblocks tool `inputSchema`.

### Phase 2 — `@mcp.*` annotations + `McpEndpoint` derivation
- Add the `@mcp.tool` / `@mcp.tool("name")` / `@mcp.auth` family to the http annotations file; macro
  populates `EndpointSchema.mcp: Option[McpEndpoint]` (`toolName` — default `apiName_endpointName`,
  override via `@mcp.tool("name")`; `auth: Option[{ authParamName, requiredScopes }]`).
- **Compile-error** when `@mcp.tool` is on a `ResponseBodySchema.{ServerSentEvents, LineStream}` (or
  streaming-body) route.
- Thread `mcp: Option[RawCompiledMcp]` (exposed + scopes) into `RawCompiledEndpoint`; add the MCP badge
  to `ApiSpecPage`.

### Phase 3 — Tool model + JSON-RPC dispatcher + MCP middleware
- Build one tool per `@mcp.tool` endpoint: `name` (`toolName`), `description` (`doc`), `inputSchema`
  (param list via Phase 1, body nested under its name, `@mcp.auth` param omitted).
- `McpEndpointMiddleware` (mirror `ApiSpecEndpointMiddleware`): consume `endpoint.mcp`, build the tool
  set at startup, append the MCP route (e.g. `POST /mcp`). Depends on an `McpAuthService` — if `NoAuth`
  and any tool is authed, `ZIO.die` at apply-time naming the offenders.
- Dispatcher over `initialize` / `notifications/initialized` / `tools/list` / `tools/call`, one
  JSON-RPC message per POST. Invalid args → `CallToolResult{ isError: true }` (not a protocol error).
- `tools/call`: validate args → reconstruct `EndpointInput` (re-split path/query/header/body; inject the
  validated bearer into `Authorization` if `@mcp.auth`) → run the existing handler → single
  `CallToolResult` (plain JSON, no SSE).

### Phase 4 — Auth (OAuth 2.1 Resource Server)
- **Claims layer:** `JWT.RegisteredClaims` (+ string-or-array `aud`, `String` `jti`) and `Scopes` in
  `oxygen.crypto.model`.
- **`McpAuthService.Live`**: decode → `JWT[RegisteredClaims]` → `BearerTokenService.Validator` (signature)
  → server-global claims (`exp`/`nbf`, `aud` = resource URI, `iss` = configured AS). 401-class failures.
  Config `McpAuthConfig{ validationKey: HashKey.CanValidate, expectedIssuer, audience }` — **single key
  for v1** (no JWKS/`kid` rotation yet). `McpAuthService.NoAuth` for unauthenticated servers.
- **Per-message gate** (each JSON-RPC call is its own POST): `initialize` / `tools/list` / public
  `tools/call` → `200`; authed `tools/call` with no/invalid bearer → `401` + `WWW-Authenticate`;
  insufficient scope (middleware checks `requiredScopes ⊆ claims.scope`) → `403 insufficient_scope`.
- **Serve Protected Resource Metadata** (RFC 9728) at `.well-known/oauth-protected-resource`
  (`resource`, `authorization_servers`, `scopes_supported`, `bearer_methods_supported`) — only when at
  least one exposed tool requires auth. We serve *only* resource metadata, never AS metadata.

---

## Risks to verify

- **Mid-session `tools/call` 401 handling.** The per-message mixed-auth design assumes Claude Code and
  Claude.ai treat a `401` on a `tools/call` (not just `initialize`) as the OAuth trigger and retry that
  call with a token. This *should* hold (401 is the universal re-auth signal) but is **unverified
  against real clients**. If a client only authenticates at connect-time, fall back to "the whole MCP
  route requires auth if any exposed tool does." Confirm empirically before locking the mixed model.

## Deferred (post-v1, no new deps when added)
- **JWKS fetch + `kid` key selection.** v1 uses a single configured `HashKey.CanValidate`.
  `JWTHeader` has no `kid` yet; multi-key rotation = add `kid`, fetch the AS JWKS (HTTP GET), pick by
  `kid`. JDK RSA only.
- **Opaque-token introspection** (RFC 7662). JWT-only for v1.

## Open questions
_(None blocking — all design questions resolved.)_

---

## OAuth UserInfo & JIT user auto-provisioning

**Question:** is there an OAuth standard for taking a token and getting the user's data (name, email,
groups, …)? **Yes — OpenID Connect (OIDC), via the UserInfo endpoint** (OIDC Core §5.3, verified
against the spec). For a resource server that must **JIT (just-in-time) auto-provision** users —
find-or-create a local record the first time it sees a `sub` — there are two identity sources:

1. **JWT access-token claims.** If the AS issues a *rich* access-token JWT, identity claims
   (`sub`, `email`, `name`, sometimes `groups`) are already on the validated token — read them off and
   provision, **no network call**. In our design the `@mcp.auth` param already hands the handler the
   decoded token (e.g. `JWT.Std[AppClaims]`), so this needs nothing new.
2. **OIDC UserInfo endpoint.** When the access token is *thin* (just `sub`/`scope` — common), `GET`
   the AS's `userinfo_endpoint` (discovered from the AS metadata document — field name is exactly
   `userinfo_endpoint`) with `Authorization: Bearer <token>`; it returns a JSON of profile claims
   (`sub`, `name`, `given_name`, `family_name`, `preferred_username`, `email`, `email_verified`,
   `picture`, …). **`groups`/`roles` are NOT core OIDC** — they're provider-specific (Okta/Entra/Auth0).
   The returned `sub` **MUST** equal the token's `sub` before trusting it.

**Built:** `oxygen.crypto.model.OidcUserInfo` — the standard claims typed, non-standard ones (groups,
roles, org_id) reachable via `.raw` / `.stringArrayClaim("groups")`; `fromJson` enforces `sub`. 3 tests.

**Recommended JIT design (keep provisioning in the app layer, where the DB env lives):**
- **Default / rich tokens:** the handler receives the verified identity through its `@mcp.auth` param
  and does find-or-create itself — it already has its own `R` (DB). Zero new MCP plumbing. This is the
  v1 recommendation.
- **Thin tokens:** **built** — `oxygen.http.server.mcp.UserInfoService` (`fetch(bearer): IO[UserInfoError, OidcUserInfo]`,
  `Live(client: Client, config)` does `GET {userinfo_endpoint}` + `Authorization: Bearer`, parses → `OidcUserInfo`;
  `Live.layer: URLayer[Client & Config, UserInfoService]`). A handler/provisioner calls it to enrich before
  find-or-create. Still deferred: a live round-trip test (needs an HTTP stub) and discovering `userinfo_endpoint`
  from the AS metadata document (currently configured directly).

**The "fully automatic" alternative (a server-level hook) and why it's deferred:** we *could* run a
`provision(claims, userInfo): ZIO[R, …, Unit]` on the auth path inside `McpServer`/the middleware,
before dispatch. The catch is the **env**: the middleware is currently `URIO[Scope, …]`, so a
provisioning hook with an app `R` forces the MCP route to become env-parametric
(`McpEndpointMiddleware[Api, R]`) or to pre-provide `R`. That's a real design choice (env threading vs.
handler-level provisioning) worth making **with you** rather than imposing at 11pm — hence model +
design now, wiring next. My lean: handler-level for v1 (simplest, no env churn), with `UserInfoClient`
available for enrichment; promote to a server hook only if you want provisioning to be transport-level
and uniform across all tools.
