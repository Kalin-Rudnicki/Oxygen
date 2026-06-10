# Migrating from oxygen-executable (v1)

This page lists what changes when moving from `oxygen-executable` + `oxygen-cli` to
`oxygen-executable-2` + `oxygen-cli-2`.

!!! tip "When v2 is a good fit"

    - New apps with subcommands declared as methods
    - Apps that configure via env vars (`@config`) rather than `-f` / `-e` bootstrap flags
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

=== "v2"

    ```scala
    import oxygen.cli.*
    import oxygen.executable.*
    import zio.*

    final case class WebServerApp() extends CliApp[Any, Env] {

      def env(@config("APP_CONFIG") config: Config): EnvLayer =
        Env.layer(config)

      @execute
      def run(): Effect = ...
    }
    object WebServerApp extends CliApp.Executable[WebServerApp]
    ```

### Subcommands

=== "v1"

    ```scala
    Executable.oneOf(
      "client" -> clientExecutable,
      "server" -> serverExecutable,
    )
    ```

=== "v2"

    ```scala
    final case class MyApp() extends CliApp[Any, Any] {
      @command def client(...): Effect = ...
      @command def server(...): Effect = ...
    }
    ```

Each v1 branch could have its own `withCLIParser` and env typing. In v2, commands share one class,
one optional `def env`, and per-method parameters.

## Blockers

Features that **do not exist in v2** today. You need a redesign or a v2 extension before migrating.

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

v2 has **no** equivalent bootstrap phase. Config is per-parameter `@config("ENV_VAR")` only
(`ConfigLoader`: raw JSON string, file path, or directory of `.json` files).

**Impact:** The in-repo `WebServerMain` (`-f` / merged JSON → `withJsonConfig`) cannot migrate
without either:

- exporting merged JSON into one env var / file and using `@config("APP_CONFIG")`, or
- adding bootstrap support to v2 (not implemented yet)

### 2. Imperative `Executable` builder

`withJsonConfig`, `withCLIParser`, `withLoggerFromJson`, `withEnv`, `withExecute`, `limitError`,
`Executable.oneOf` — all replaced by the macro + `CliApp` model. Every v1 app rewrites its shape.

### 3. v1 `Parser` / `Params` expressiveness

cli-v1 features without a direct cli-2 equivalent:

- `Params.FirstOfByArgIndex` (e.g. pick logger backend by flag position)
- `Params.ifPresent` (optional alternate parsers)
- Param **aliases** on `Params.value`

**Workaround:** `@custom` + `given ArgsParser[T]`, or restructure CLI.

### 4. JSON-driven logging

v1: `withLoggerFromJson`, `additionalLoggerDecoders`, logger section in app JSON.

v2: `OXYGEN_LOGGER_TYPE`, `OXYGEN_LOG_LEVEL`, and `defaultLoggerType` only.

**Impact:** Apps that configure log targets/levels per environment in config files need a different
approach (env vars, or programmatic `LogConfig` inside the app after startup).

## Friction (workarounds exist)

### Logger CLI flags → environment variables

v1 had `--log-level`, `--oxygen-logger`, `--json-logger`, `--zio-logger`, `--keep-zio-logger`, etc.

v2 intentionally dropped these. Use:

```bash
OXYGEN_LOGGER_TYPE=OXYGEN_LEAN OXYGEN_LOG_LEVEL=debug ./my-app
```

Or override `defaultLoggerType` in your `CliApp.Executable` object.

### Subcommand vs root-flag order

```bash
# v2 requires
my-app client --my-opt=x

# v1-style (fails in v2)
my-app --my-opt=x client
```

Update scripts and documentation.

### `@config` vs v1 multi-source merge

v2: one env var per `@config`, no `a.b.c:` nesting prefix, no JAR resources, no merging multiple
independent bootstrap sources.

**Workaround:** CI/k8s composes one JSON blob or file path into a single env var.

### Error handling

v1: typed `ExecuteError` (parse, help, config source errors).

v2: parse failures print help; other failures use `cause.prettyPrint` unless you use
`ExecutableError.ExitWith.exit(code)` for intentional exits.

### Tab completion

Regenerate completion scripts from a v2 build (`--: generate`). Protocol is the same; parser
walking is different. Bootstrap-flag completion (v1 `-f`/`-e` before subcommand) is not replicated.

## Intentionally deferred (v2 replaces by design)

| v1 | v2 |
|----|-----|
| Logger CLI pollution | Env-based `OXYGEN_*` logger config |
| `ExecutableContext` | Fixed bootstrap logging in `CliAppLogging.install` |
| `additionalLoggerParsers` | Dropped |
| Manual `Parsing.parse` | Macro-generated parsers |
| `--keep-zio-logger` | `DefaultLoggerType.Zio` or `OXYGEN_LOGGER_TYPE=ZIO` |

## Parity (already covered in v2)

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

1. **Change dependencies** — `oxygen-executable-2` (and `oxygen-cli-2` if needed directly).
2. **Rewrite app class** — extend `CliApp`, add `@execute` or `@command` methods, `object extends CliApp.Executable`.
3. **Replace `withJsonConfig`** — `@config("ENV")` on `env` or command params; ensure deploy sets env/file.
4. **Replace `withEnv`** — `def env(...): EnvLayer`.
5. **Replace `withCLIParser`** — annotations, or `@custom` parsers.
6. **Update invocations** — subcommand first; drop bootstrap flags or map them to env.
7. **Replace logger CLI** — `OXYGEN_LOGGER_TYPE` / `OXYGEN_LOG_LEVEL`.
8. **Regenerate bash completion** — see [Tab completion](completion.md).
9. **Drop `oxygen.predef.executable`** — import `oxygen.executable.*` and `oxygen.cli.*`.

## Example: `WebServerMain`-style app

The repo's v1 web server loads config only via `-f` / bootstrap JSON — no CLI flags on the executable itself.

A v2-shaped version would look like:

```scala
final case class WebServerApp() extends CliApp[Any, WebServerApp.Env] {

  def env(@config("APP_CONFIG") config: Config): EnvLayer =
    WebServerApp.Env.layer(config)

  @execute
  def run(): Effect =
    MigrationService.migrate(migrations) *>
      ZIO.logInfo("Press space+enter to stop server") *>
      ZIO.attempt(java.lang.System.in.read()).unit *>
      ZIO.logInfo("Stopping...")

}
object WebServerApp extends CliApp.Executable[WebServerApp]
```

Deploy with:

```bash
export APP_CONFIG=/etc/myapp/config.json
./web-server-app
```

Until v2 gains bootstrap `-f`/`-j`/merge, this env-or-file pattern is the supported migration path.