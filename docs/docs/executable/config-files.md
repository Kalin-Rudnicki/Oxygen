# Config Files

Oxygen gives you two complementary ways to work with JSON / YAML config files. They share the same
core loading and merging logic (single file, or a directory merged with `reduceLeft(_ ++ _)`), so
their behavior never diverges — they differ only in *when* and *how* you reach for them.

| | `@envConfig` | `ConfigFileService` |
|---|---|---|
| Phase | **Startup** — resolved once while the CLI parses its inputs | **Runtime** — call it any time while the app runs |
| Shape | A CLI-param annotation; the framework injects the decoded value | An injectable `ZLayer`-provided ZIO service |
| Direction | Read-only (load + decode) | Read **and** write (load / merge / list / save) |
| Source | One env var → a file *or* a directory | Any `Path` you hand it |

Both honor `FileSystem.current`, so both are testable by pointing at a temp directory (or a test
file-system).

## `@envConfig` — startup

Use it when a config is an *input* to the app: resolved once, up front, and injected already decoded.

```scala
final case class ServeCmd(
    @envConfig("APP_CONFIG") cfg: AppConfig, // env var holds a file path or a directory
) extends CliApp[Any, Any] derives CompiledCliApp.DeriveRootApp
```

If `APP_CONFIG` points at a **file** it is decoded (dispatch on `.json` / `.yaml` / `.yml`); if it
points at a **directory**, every supported file inside is merged (later files, sorted by path, win).

## `ConfigFileService` — runtime

Use it when the app needs to read or **write** config files while running — e.g. a CLI that manages
`./.my-cli/local.json` and `~/.my-cli/global.json`.

```scala
import oxygen.executable.config.*
import oxygen.zio.system.Path

for {
  local  <- Path.of("./.my-cli/local.json")
  cfg    <- ConfigFileService.load[AppConfig](local)
  updated = cfg.copy(port = 8080)
  _      <- ConfigFileService.save(local, updated) // atomic: temp-file + move
} yield ()
```

Provide the service with `ConfigFileService.live` (aliased as `.default`; `.test` is the same layer —
testability comes from `FileSystem.current`).

### Operations

- `load[A: JsonDecoder](file)` / `loadJson(file)` — read + decode a single file.
- `save[A: JsonEncoder](file, value)` — **atomic** write (serialize by extension → temp sibling →
  `moveTo`), creating parent directories as needed.
- `mergeDirectory[A: JsonDecoder](dir)` / `mergeDirectoryJson(dir)` — merge every supported file in
  a directory, `reduceLeft(_ ++ _)` (later files, sorted by path, override earlier ones).
- `loadResolved[A: JsonDecoder](path)` — the `@envConfig` semantics: file → load, directory → merge.
- `list(dir)` / `exists(file)`.

### Errors

All operations fail into the typed `ConfigFileError` ADT — `FileSystem` (wrapping `FileSystemError`),
`UnsupportedExtension`, `JsonDecodeFailure`, `YamlDecodeFailure`, `EmptyDirectory`,
`NotFileOrDirectory`, `PathDoesNotExist`.

### Supported extensions

`.json` (via `oxygen-json`), `.yaml` / `.yml` (via `oxygen-yaml`). Any other extension yields
`UnsupportedExtension`.
