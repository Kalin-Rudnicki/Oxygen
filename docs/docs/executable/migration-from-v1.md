# Migrating from the previous executable API

This page lists what changes when moving from the old `ExecutableApp` / `Parser` API to the current
annotation-driven `CliApp` model. The rewrite ships in the same Maven artifacts (`oxygen-executable`,
`oxygen-cli`) — bump your version and expect breaking changes.

!!! tip "When the new API is a good fit"

    - New apps with subcommands declared as methods
    - Apps that configure via env vars (`@envConfig`) rather than `-f` / `-e` bootstrap flags
    - Teams that want annotation-driven CLI with less boilerplate

!!! warning "When you will feel pain"

    - Apps using **pre-`--` bootstrap config** (`-f`, `-j`, `-e`, `-r`)
    - Apps with **JSON-driven logger config** in config files
    - Apps with **heavy `withCLIParser`** using v1-only combinators
    - Shell scripts that put **global flags before the subcommand name**

## API mapping

### Entry point

=== "v1"

    ```scala
    import oxygen.predef.executable.*

    object WebServerMain extends ExecutableApp {
      override val executable: Executable =
        Executable
          .withJsonConfig[Config]
          .withEnv[Env] { (config, _) => Env.layer(config) }
          .withExecute { ... }
    }
    ```

=== "Current"

    ```scala
    import oxygen.cli.*
    import oxygen.executable.*
    import zio.*

    final case class WebServerMain() extends CliApp[Any, WebServerMain.Env] derives CompiledCliApp.DeriveRootApp {

      def env(@envConfig("APP_CONFIG") config: WebServerMain.Config): EnvLayer =
        WebServerMain.Env.layer(config)

      @execute
      def run(): Effect = ...
    }
    object WebServerMain extends CliApp.Executable[WebServerMain]
    ```

### Subcommands

=== "v1"

    ```scala
    Executable.oneOf(
      "client" -> clientExecutable,
      "server" -> serverExecutable,
    )
    ```

=== "Current"

    ```scala
    final case class MyApp() extends CliApp[Any, Any] {
      @command def client(...): Effect = ...
      @command def server(...): Effect = ...
    }
    ```

Each v1 branch could have its own `withCLIParser` and env typing. In the current API, commands share
one class, one optional `def env`, and per-method parameters.

## Blockers

Features that **do not exist in the current API** today. You need a redesign or a library extension
before migrating.

### 1. Bootstrap config sources (`-f` / `-j` / `-e` / `-r` before `--`)

v1 parses flags **before** `--`, loads JSON from multiple sources, merges them, and passes one `Json`
to `withJsonConfig`:

```bash
java -jar app.jar -f=config.json -e=SECRETS:APP_SECRETS -- server --port 9000
```

| v1 flag | Source |
|---------|--------|
| `-f` / `--file` | filesystem JSON |
| `-j` / `--jar` | classpath/JAR resource |
| `-e` / `--env` | env var (optional `a.b.c:VAR` nesting) |
| `-r` / `--raw` | inline JSON (optional nesting) |

The current API has **no** equivalent bootstrap phase. Config is per-parameter `@envConfig("ENV_VAR")`
only (`ConfigLoader`: raw JSON string, file path, or directory of `.json` files).

**Impact:** Apps that relied on `-f` / merged JSON → `withJsonConfig` must either:

- export merged JSON into one env var / file and use `@envConfig("APP_CONFIG")`, or
- add bootstrap support to the library (not implemented yet)

The in-repo `WebServerMain` has been migrated to `@envConfig("APP_CONFIG")`.

### 2. Imperative `Executable` builder

`withJsonConfig`, `withCLIParser`, `withLoggerFromJson`, `withEnv`, `withExecute`, `limitError`,
`Executable.oneOf` — all replaced by the macro + `CliApp` model. Every v1 app rewrites its shape.

### 3. v1 `Parser` / `Params` expressiveness

Old `Parser` / `Params` features without a direct equivalent:

- `Params.FirstOfByArgIndex` (e.g. pick logger backend by flag position)
- `Params.ifPresent` (optional alternate parsers)
- Param **aliases** on `Params.value`

**Workaround:** `@custom` + `given ArgsParser[T]`, or restructure CLI.

### 4. JSON-driven logging

v1: `withLoggerFromJson`, `additionalLoggerDecoders`, logger section in app JSON.

Current API: `OXYGEN_LOGGER_TYPE`, `OXYGEN_LOG_LEVEL`, and `defaultLoggerType` only.

**Impact:** Apps that configure log targets/levels per environment in config files need a different
approach (env vars, or programmatic `LogConfig` inside the app after startup).

## Friction (workarounds exist)

### Logger CLI flags → environment variables

v1 had `--log-level`, `--oxygen-logger`, `--json-logger`, `--zio-logger`, `--keep-zio-logger`, etc.

The current API intentionally dropped these. Use:

```bash
OXYGEN_LOGGER_TYPE=OXYGEN_LEAN OXYGEN_LOG_LEVEL=debug ./my-app
```

Or override `defaultLoggerType` in your `CliApp.Executable` object.

### Subcommand vs root-flag order

```bash
# current API requires
my-app client --my-opt=x

# old style (fails now)
my-app --my-opt=x client
```

Update scripts and documentation.

### `@envConfig` vs v1 multi-source merge

Current API: one env var per `@envConfig`, no `a.b.c:` nesting prefix, no JAR resources, no merging
multiple independent bootstrap sources.

**Workaround:** CI/k8s composes one JSON blob or file path into a single env var.

### Error handling

v1: typed `ExecuteError` (parse, help, config source errors).

Current API: parse failures print help; other failures use `cause.prettyPrint` unless you use
`ExecutableError.ExitWith.exit(code)` for intentional exits.

### Tab completion

Regenerate completion scripts from a current build (`--: generate`). Protocol is the same; parser
walking is different. Bootstrap-flag completion (`-f`/`-e` before subcommand) is not replicated.

## Intentionally deferred (replaced by design)

| Previous | Current |
|----------|---------|
| Logger CLI pollution | Env-based `OXYGEN_*` logger config |
| `ExecutableContext` | Fixed bootstrap logging in `CliAppLogging.install` |
| `additionalLoggerParsers` | Dropped |
| Manual `Parsing.parse` | Macro-generated parsers |
| `--keep-zio-logger` | `DefaultLoggerType.Zio` or `OXYGEN_LOGGER_TYPE=ZIO` |

## Parity (already covered)

- `ZIOAppDefault` entry (`CliApp.Executable`)
- `--:` completion (`generate`, `complete`, bash script)
- `--help` / `--help-extra` with subcommand drill-down
- `def env` / ZLayer wiring
- Subcommands and nested `CliApp` types
- `@flag`, `@toggle`, `@named`, `@positional`, optional/repeated/list/tuple params
- `StrictEnum` + tab completion
- `@custom` parsers
- `ExitCode` / `ExitWith` for clean exits
- Config from env (subset of v1 `-e` behavior)

## Migration checklist

1. **Bump dependencies** — same artifacts (`oxygen-executable`, `oxygen-cli`), new major/minor version.
2. **Rewrite app class** — extend `CliApp`, add `@execute` or `@command` methods, `object extends CliApp.Executable`.
3. **Replace `withJsonConfig`** — `@envConfig("ENV")` on `env` or command params; ensure deploy sets env/file.
4. **Replace `withEnv`** — `def env(...): EnvLayer`.
5. **Replace `withCLIParser`** — annotations, or `@custom` parsers.
6. **Update invocations** — subcommand first; drop bootstrap flags or map them to env.
7. **Replace logger CLI** — `OXYGEN_LOGGER_TYPE` / `OXYGEN_LOG_LEVEL`.
8. **Regenerate bash completion** — see [Tab completion](completion.md).
9. **Drop `oxygen.predef.executable`** — import `oxygen.executable.*` and `oxygen.cli.*`.

## Example: `WebServerMain`

The repo's web server has been migrated. See
`example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala`.

Deploy with:

```bash
export APP_CONFIG=/etc/myapp/config.json
./web-server-app
```

Or for local dev from the repo:

```bash
APP_CONFIG=example/apps/web-server/src/main/resources/local.json sbt "example-web-server/run"
```

Until bootstrap `-f`/`-j`/merge returns, this env-or-file pattern is the supported migration path.