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
| `@envVar` / `@envVar("VAR")` | read from environment | decodes the raw env-var value; name auto-derived (`SCREAMING_SNAKE_CASE`) if omitted |
| `@envConfig("VAR")` / `@envConfig("VAR", "fallback-path")` | JSON config from environment | value is raw JSON / a file path / a directory (see [overview](index.md#from-the-environment-envvar-envconfig)) |
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

### Path completion under a base directory

`GlobalPath` is an opaque `String` wrapper whose completion root is a **compile-time string literal**
(`String & Singleton`). `CompletionOptions` and `PlainTextSchema` givens are derived automatically —
no boilerplate `given` at the use site.

```scala
@named config: GlobalPath["./configs"]
```

With `prod/app` on the command line, tab complete suggests matching entries under `./configs/prod/`.
Directories get a trailing `/`; paths that escape the base directory are ignored.

For a runtime-provided base, use `RelativePath.under("./configs")` or `CompletionOptions.underBasePath`
with a plain `String` parameter.

### Custom parsers (`@custom`)

For full control, provide `given ArgsParser[MyType]` and mark the parameter `@custom`. For example,
a three-positional-int color:

```scala
given ArgsParser[RgbColor] =
  (PositionalArgsParser.singlePlain[Int]("r") ^>>
     PositionalArgsParser.singlePlain[Int]("g") ^>>
     PositionalArgsParser.singlePlain[Int]("b")).map { case (r, g, b) => RgbColor(r, g, b) }
```

## `CliApp` type parameters

```scala
abstract class CliApp[RequiredEnv, ProvidedEnv]
```

- `RequiredEnv` — environment the app needs from outside (often `Any` at the root)
- `ProvidedEnv` — what `def env` provides to commands
- `FullEnv = RequiredEnv & ProvidedEnv` — available in `Effect`

A nested sub-app's `RequiredEnv` must line up with the env its parent runs commands at (the parent's
`FullEnv`) — e.g. a sub-app under a root that provides `RootCtx` is `CliApp[RootCtx, …]`.

## Full example (abbreviated)

Illustrative app showing subcommands, a nested app (with its own `given`), `@envConfig`, and `def env`:

```scala
final case class ExampleApp() extends CliApp[Any, String] {

  @doc("Provides runtime environment", "for subcommands")
  def env(@named host: String = "localhost"): EnvLayer =
    ZLayer.succeed(host)

  @command
  def client(
      @envConfig("APP_CONFIG") cfg: ClientConfig,
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
      ZIO.logInfo(s"host=$host port=$port mood=$mood")
    }

  @command
  def nested1: Nested1 = Nested1()
}
object ExampleApp extends CliApp.Executable[ExampleApp](CliApp.derive)

// the nested sub-app publishes its own derivation
final case class Nested1() extends CliApp[String, Any] { @execute def run(): Effect = ZIO.unit }
object Nested1 { given CliApp.Derived[Nested1, String] = CliApp.derive }
```

## `--` passthrough

Arguments after `--` are preserved for the app (`oxygen-cli` merges before/after `--` when parsing).
Unlike the previous API, the current parser does **not** reserve everything before `--` for bootstrap-only flags.