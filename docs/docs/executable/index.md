# Oxygen Executable

`oxygen-executable-2` is the ZIO entry point for command-line applications in Oxygen.
You declare an app as a Scala class; macros derive parsers, help text, and bash completion from annotations.

Depends on:

- `oxygen-executable-2` — app model, macro, logging bootstrap, completion
- `oxygen-cli-2` — parsing, help, completion engine (usually pulled in transitively)

## Quick start

```scala
import oxygen.cli.*
import oxygen.executable.*
import zio.*

final case class MyApp() extends CliApp[Any, Any] {

  @execute
  def run(): Effect =
    ZIO.logInfo("Hello")

}
object MyApp extends CliApp.Executable[MyApp]
```

Run:

```bash
sbt run
# or after packaging:
java -jar my-app.jar
```

## Mental model

| v1 (`oxygen-executable`) | v2 (`oxygen-executable-2`) |
|--------------------------|----------------------------|
| `object X extends ExecutableApp` | `object X extends CliApp.Executable[X]` |
| `Executable.withJsonConfig[...].withEnv(...).withExecute(...)` | Class extends `CliApp` + `@execute` / `@command` |
| `oxygen.cli.Parser` / `Params` | Macro-generated `ArgsParser` from `@named`, `@positional`, etc. |
| Bootstrap flags `-f` / `-j` / `-e` / `-r` before `--` | Per-parameter `@config("ENV_VAR")` |
| Logger CLI flags | Environment variables (see below) |

See [Migrating from v1](migration-from-v1.md) for a full gap analysis.

## App shapes

### Single command (`@execute`)

One main entry point, no subcommands. Use for simple tools and for apps that previously used
`withExecute` only (e.g. a server that just runs).

```scala
final case class ServerApp() extends CliApp[Any, Env] {

  def env(@config("APP_CONFIG") config: AppConfig): EnvLayer =
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
final case class ToolApp(
    @named verbose: Boolean = false,
) extends CliApp[Any, Any] {

  @command
  def sync(): Effect = ZIO.logInfo(s"sync (verbose=$verbose)")

  @command("import-data")
  def importData(@named path: String): Effect =
    ZIO.logInfo(s"import $path")

}
object ToolApp extends CliApp.Executable[ToolApp]
```

Invocation:

```bash
tool-app sync
tool-app import-data --path ./data.json
```

**Argument order:** the subcommand name must come **before** root constructor flags:

```bash
# correct
tool-app sync --verbose

# rejected
tool-app --verbose sync
```

### Nested apps

A `@command` method can return another `CliApp` type instead of an `Effect`. That type gets its own
subcommands (see `ExampleApp` in the repo: `nested1`, `nested2`, `nested3`).

### Environment (`def env`)

Optional `def env(...): EnvLayer` provides a `ZLayer` merged into every command's `RIO` environment.
Parameters on `env` use the same annotation vocabulary as command parameters.

## Runtime environment variables

These are read at startup by `CliApp.Executable` — they are **not** CLI flags.

| Variable | Purpose | Values |
|----------|---------|--------|
| `OXYGEN_LOG_LEVEL` | Minimum log level | `RichLogLevel` names: `info`, `debug`, `trace`, `error`, … |
| `OXYGEN_LOGGER_TYPE` | Logger backend | `OXYGEN_ALL`, `OXYGEN_LEAN`, `ZIO` |
| `OXYGEN_EXECUTABLE_EXIT` | Call `System.exit` | `true` (default) or `false` (keep JVM alive; for embedding/tests) |

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

## Config from environment (`@config`)

`@config("VAR")` loads JSON for a command or `env` parameter. The env var value can be:

1. **Raw JSON** — parsed directly
2. **File path** — if the path is a file, its contents are parsed as JSON
3. **Directory** — all `*.json` files in the directory are merged

```scala
@command
def client(@config("APP_CONFIG") cfg: ClientConfig): Effect =
  ZIO.logInfo(cfg.toString)
```

`ClientConfig` needs a `JsonDecoder` (typically `derives JsonSchema`).

Optional config:

```scala
@config("OPTIONAL_CONFIG") extra: Option[ClientConfig]
```

There is **no** v1-style `-f=config.json` bootstrap flag. Point `APP_CONFIG` at a file path, or export JSON inline.

## Help

Built-in flags (on every app):

- `--help` / `-h` — usage for the current command
- `--help-extra` / `-H` — includes nested subcommand help blocks

`@doc("line one", "line two")` on parameters and methods appears in help output.

## Tab completion

Bash completion is built in via the `--:` protocol. See [Tab completion](completion.md).

## Example app

The repo ships a full example at
`modules/general/executable-2-test/src/main/scala/oxygen/executable/ExampleApp.scala`
covering subcommands, nested apps, `@config`, enums, lists, tuples, and `def env`.

```bash
sbt "oxygen-executable-2-test/runMain oxygen.executable.ExampleApp --help"
```

## Further reading

- [CLI annotations & parameters](cli.md)
- [Migrating from v1](migration-from-v1.md)
- [Tab completion](completion.md)