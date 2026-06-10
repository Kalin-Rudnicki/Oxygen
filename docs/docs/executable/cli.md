# CLI annotations & parameters

`oxygen-cli` annotations live in `oxygen.cli`. `oxygen-executable` adds `@command`, `@execute`,
and the `CliApp` base class.

## Function annotations (`oxygen.executable`)

| Annotation | Use |
|------------|-----|
| `@command` | Method becomes a subcommand (default name: method name, camelCase → dash-case) |
| `@command("custom-name")` | Explicit subcommand name |
| `@execute` | Single entry point (mutually exclusive with `@command` on the same class) |

A class may have **either** one `@execute` **or** one or more `@command` methods — not both.

Command methods return `Effect` (`RIO[…, Unit | ExitCode]`) or `EffectE[E]` (`ZIO[…, E, Unit | ExitCode]`
with `E <: Throwable`) when you want a typed error channel. See
[Command result types](index.md#command-result-types-effect-and-effecte) in the overview.

## Parameter annotations (`oxygen.cli`)

| Annotation | CLI shape | Notes |
|------------|-----------|-------|
| `@named` | `--long-name value` | Default long name from parameter name (camelCase → dash-case) |
| `@positional` | positional arg | |
| `@flag` | `--flag` / `--no-flag` | `Boolean`; default in Scala = absent value |
| `@toggle` | `--enable-x` / `--disable-x` | Required boolean choice |
| `@config("ENV_VAR")` | loaded from environment | JSON / file / directory (see [overview](index.md#config-from-environment-config)) |
| `@custom` | custom parser | requires `given ArgsParser[T]` in scope |

### Tweaks

| Annotation | Effect |
|------------|--------|
| `@longName("name")` | Override long option name |
| `@shortName('x')` | Short flag |
| `@shortName(none)` | No short flag |
| `@shortName(auto)` | Auto short name (default for `@named`) |
| `@doc("...", "...")` | Help text lines |

Toggle prefixes/names use `@longName.truePrefix`, `@longName.falsePrefix`, etc.

## Types & builders

The macro picks parsers from the parameter type:

| Type | Behavior |
|------|----------|
| `String`, `Int`, `Double`, … | plain-text schema |
| `Option[A]` | optional named/positional |
| `List[A]` | repeated (named: defaults to empty list) |
| `NonEmptyList[A]` | at least one value |
| `(A, B)` / tuples | multiple positional or named components |
| `enum ... derives StrictEnum` | enum with tab completion + help hints |

### Enum completion

Enums with `derives StrictEnum` get tab completion and `Enum: ...` help hints automatically:

```scala
enum Mood derives StrictEnum { case Chill, Hyped, Sleepy }

@command
def server(@named mood: Mood = Mood.Chill): Effect = ...
```

Completing `--mood hy` suggests `Hyped`.

### Custom completion

Implement `CompletionOptions[A]` for non-enum types:

```scala
import oxygen.cli.CompletionOptions
import zio.*

given CompletionOptions[Hostname] with
  def completionOptions(in: String): Task[Seq[String]] =
    ZIO.succeed(knownHosts.filter(_.startsWith(in)))

@named host: Hostname
```

### Custom parsers (`@custom`)

For full control, provide `given ArgsParser[MyType]` and mark the parameter `@custom`.

## `CliApp` type parameters

```scala
abstract class CliApp[RequiredEnv, ProvidedEnv]
```

- `RequiredEnv` — environment the app needs from outside (often `Any` at the root)
- `ProvidedEnv` — what `def env` provides to commands
- `FullEnv = RequiredEnv & ProvidedEnv` — available in `Effect`

Nested sub-apps must have `RequiredEnv` subtype of the parent's `FullEnv`.

## Full example (abbreviated)

Illustrative app showing subcommands, nested apps, `@config`, and `def env`:

```scala
final case class ExampleApp(
    @named
    @doc("Optional root-level", "configuration flag")
    myOpt: Option[String],
) extends CliApp[Any, String] {

  @doc("Provides runtime environment", "for subcommands")
  def env(@named host: String = "localhost"): EnvLayer =
    ZLayer.succeed(host)

  @command
  def client(
      @config("APP_CONFIG") cfg: ClientConfig,
      @named i: Option[String],
      @named p2: NonEmptyList[String],
  ): Effect =
    ZIO.logInfo(s"$cfg $i $p2")

  @command
  def server(
      @named port: Int = 8080,
      @named mood: Mood = Mood.Chill,
  ): Effect =
    ZIO.serviceWithZIO[String] { host =>
      ZIO.logInfo(s"host=$host port=$port mood=$mood myOpt=$myOpt")
    }

  @command
  def nested1: Nested1 = Nested1()
}
object ExampleApp extends CliApp.Executable[ExampleApp]
```

## `--` passthrough

Arguments after `--` are preserved for the app (`oxygen-cli` merges before/after `--` when parsing).
Unlike the previous API, the current parser does **not** reserve everything before `--` for bootstrap-only flags.