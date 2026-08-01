# Oxygen Executable

`oxygen-executable` is the ZIO entry point for command-line applications in Oxygen.
You declare an app as a Scala class; macros derive parsers, help text, and bash completion from annotations.

Depends on:

- `oxygen-executable` — app model, macro, logging bootstrap, completion
- `oxygen-cli` — parsing, help, completion engine (usually pulled in transitively)

## Quick start

```scala
import oxygen.cli.*
import oxygen.executable.*
import zio.*

final case class MyApp() extends CliApp[Any, Any] derives CompiledCliApp.DeriveRootApp {

  @execute
  def run(): Effect =
    ZIO.logInfo("Hello")

}
object MyApp extends CliApp.Executable[MyApp]
```

> **Per-file derivation via `derives`.** Each `CliApp` derives itself. A runnable root uses
> `derives CompiledCliApp.DeriveRootApp` (its `RequiredEnv` must be `Any`) and its companion just
> `extends CliApp.Executable[Root]`. Every **sub-app** referenced by a `@command` uses
> `derives CompiledCliApp.DeriveSubApp` — no companion given needed; the env types are recovered from
> the app's `CliApp[R, P]` supertype. A parent summons the child's derived given rather than reflecting
> into the child, so derivation stays local to each file.

Run:

```bash
sbt run
# or after packaging:
java -jar my-app.jar
```

## Mental model

| Previous API (pre-0.5) | Current API |
|------------------------|-------------|
| `object X extends ExecutableApp` | `class X ... derives CompiledCliApp.DeriveRootApp` + `object X extends CliApp.Executable[X]` |
| `Executable.withJsonConfig[...].withEnv(...).withExecute(...)` | Class extends `CliApp` + `@execute` / `@command` |
| `oxygen.cli.Parser` / `Params` | Macro-generated parser from `@named`, `@positional`, etc. |
| Bootstrap flags `-f` / `-j` / `-e` / `-r` before `--` | Per-parameter `@envVar` / `@envConfig("ENV_VAR")` |
| Logger CLI flags | Environment variables (see below) |

See [Migrating from v1](migration-from-v1.md) for a full gap analysis.

## App shapes

### Single command (`@execute`)

One main entry point, no subcommands. Use for simple tools and for apps that previously used
`withExecute` only (e.g. a server that just runs).

```scala
final case class ServerApp() extends CliApp[Any, Env] derives CompiledCliApp.DeriveRootApp {

  def env(@envConfig("APP_CONFIG") config: AppConfig): EnvLayer =
    Env.layer(config)

  @execute
  def run(): Effect =
    ZIO.logInfo("Server starting...")

}
object ServerApp extends CliApp.Executable[ServerApp]
```

### Subcommands (`@command`)

Multiple methods become subcommand names (camelCase → dash-case: `runServer` → `run-server`).

```scala
final case class ToolApp() extends CliApp[Any, Any] derives CompiledCliApp.DeriveRootApp {

  @command
  def sync(@flag verbose: Boolean = false): Effect = ZIO.logInfo(s"sync (verbose=$verbose)")

  @command("import-data")
  def importData(@named path: String): Effect =
    ZIO.logInfo(s"import $path")

}
object ToolApp extends CliApp.Executable[ToolApp]
```

Invocation:

```bash
tool-app sync --verbose
tool-app import-data --path ./data.json
```

> **Roots are zero-arg.** A root `CliApp` is constructed by the runner, so it takes no constructor
> parameters — put shared options on `def env`, and per-command options on the command method. (A
> sub-app *may* take constructor params, but they're supplied by its parent `@command` method, not
> parsed from the CLI.)

### Nested apps

A `@command` method can return another `CliApp` type instead of an `Effect`. That type gets its own
subcommands, and derives itself with `DeriveSubApp`:

```scala
final case class Group() extends CliApp[Any, Any] derives CompiledCliApp.DeriveSubApp {
  @command def child(@positional x: Int): Effect = ZIO.logInfo(s"x=$x")
}

final case class Root() extends CliApp[Any, Any] derives CompiledCliApp.DeriveRootApp {
  @command def group: Group = Group()
}
object Root extends CliApp.Executable[Root]
```

See [CLI annotations](cli.md) for a full annotated example with nested apps.

### Environment (`def env`)

Optional `def env(...): EnvLayer` provides a `ZLayer` merged into every command's `RIO` environment.
Parameters on `env` use the same annotation vocabulary as command parameters.

### Command result types (`Effect` and `EffectE`)

Every `@execute` or `@command` method must return an effect that produces `Unit` or `ExitCode`.
`CliApp` defines two type aliases for that shape:

| Alias | Definition | When to use |
|-------|------------|-------------|
| `Effect` | `RIO[FullEnv & Scope, Unit \| ExitCode]` | Default — unconstrained error channel |
| `EffectE[E]` | `ZIO[FullEnv & Scope, E, Unit \| ExitCode]` where `E <: Throwable` | Command should only fail with a specific error type |

`Effect` is the usual choice for simple commands:

```scala
@execute
def run(): Effect =
  ZIO.logInfo("Hello")
```

Use `EffectE` when you want the compiler to enforce a narrow failure type inside a command:

```scala
enum AppError extends Throwable {
  case MissingInput(path: String)
  case InvalidConfig(message: String)
}

@command
def importData(@named path: String): EffectE[AppError] =
  if path.isBlank then ZIO.fail(AppError.MissingInput(path))
  else ZIO.logInfo(s"import $path")
```

`EffectE` is accepted anywhere `Effect` is — the macro treats it as a command effect. At runtime,
uncaught failures are still handled by `CliApp.Executable` (console message via `safeGetMessage`,
full cause logged at debug). Prefer `ExecutableError.ExitWith` when you need a specific exit code
without treating the outcome as a failure.

## Runtime environment variables

These are read at startup by `CliApp.Executable` — they are **not** CLI flags.

| Variable | Purpose | Values |
|----------|---------|--------|
| `OXYGEN_LOG_LEVEL` | Minimum log level | `RichLogLevel` names: `info`, `debug`, `trace`, `error`, … |
| `OXYGEN_LOGGER_TYPE` | Logger backend | `OXYGEN_ALL`, `OXYGEN_LEAN`, `ZIO` |
| `OXYGEN_EXECUTABLE_EXIT` | Call `System.exit` | `true` (default) or `false` (keep JVM alive; for embedding/tests) |
| `OXYGEN_CLI_JSON` | Emit `--help` as JSON instead of formatted text | `true` / `false` (default) |

Override the default logger type in code:

```scala
object MyApp extends CliApp.Executable[MyApp] {
  override def defaultLoggerType: DefaultLoggerType = DefaultLoggerType.OxygenAll
}
```

## Exit codes

Return `ExitCode` from an effect, or fail with a clean exit (no stack trace):

```scala
import oxygen.executable.ExecutableError.ExitWith

@execute
def run(): Effect =
  if bad then ExitWith.exit(2)
  else ZIO.succeed(ExitCode.success)
```

## From the environment (`@envVar`, `@envConfig`)

Two annotations read a command or `env` parameter from an environment variable instead of the CLI:

### `@envVar` — decode the raw value

`@envVar` reads the env var and decodes its raw string directly via the parameter's schema.

```scala
@envVar port: Int                 // env var name auto-derived: PORT
@envVar("DB_URL") dbUrl: String   // explicit name
@envVar token: Option[String]     // optional (absent var -> None)
```

The name is auto-derived from the parameter in `SCREAMING_SNAKE_CASE` when not given explicitly.

### `@envConfig` — load JSON config from a path/value

`@envConfig("VAR")` treats the env var value as a config source:

1. **Raw JSON** — parsed directly
2. **File path** — if the value is a file path, its contents are parsed as JSON
3. **Directory** — all `*.json` files in the directory are merged

```scala
@command
def client(@envConfig("APP_CONFIG") cfg: ClientConfig): Effect =
  ZIO.logInfo(cfg.toString)
```

`ClientConfig` needs a `JsonDecoder` (typically `derives JsonSchema`). Optional config and a fallback
path are both supported:

```scala
@envConfig("OPTIONAL_CONFIG") extra: Option[ClientConfig]      // absent -> None
@envConfig("APP_CONFIG", "./config.json") cfg: ClientConfig    // fall back to ./config.json if unset
```

There is **no** `-f=config.json` bootstrap flag. Point the env var at a file path, or export JSON inline.

## Help

Built-in flags (on every app):

- `--help` — usage for the current command
- `--help-extra` — includes nested subcommand help blocks

`@doc("line one", "line two")` on parameters and on `@command`/`@execute` methods appears in help output.

`--help` on a group of sub-commands lists each leaf command by its **full path** plus its `@doc`
(no parameters) — e.g. `deep level down bottom`. Drill into a specific command to see its arguments:

```bash
my-app --help            # lists commands (path + doc)
my-app deploy --help     # shows `deploy`'s arguments
```

Set `OXYGEN_CLI_JSON=true` to emit help as a JSON array (one object per leaf command: `command` path,
`doc`, and a typed `params` list) for tooling.

### Errors

Parse errors are reported **all at once**, not one-at-a-time: a missing positional, a missing flag,
and an unset required env var all surface together.

## Tab completion

Bash completion is built in via the `--:` protocol. See [Tab completion](completion.md).

## Example app

The repo ships a migrated example at
`example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala`
— a long-running server using `@execute`, `def env`, and `@envConfig("APP_CONFIG")`. A broader feature
showcase (flat/nested commands, every annotation, env layers) lives at
`example/apps/example-app/.../ShowcaseApp.scala`.

```bash
APP_CONFIG=example/apps/web-server/src/main/resources/local.json sbt "example-web-server/run"
```

## Further reading

- [CLI annotations & parameters](cli.md)
- [Migrating from v1](migration-from-v1.md)
- [Tab completion](completion.md)