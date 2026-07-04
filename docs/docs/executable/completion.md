# Tab completion

`oxygen-executable` apps expose built-in bash completion via the `--:` prefix. A wrapper script
talks to your app over environment variables; the app prints candidate completions separated by a
fixed join string.

## Prerequisites

- **bash** with programmable completion (`complete` builtin). macOS ships bash 3.2 by default;
  install a newer bash if needed: `brew install bash`
- A runnable app (JAR or native binary)

## JAR workflow

### 1. Build a runnable JAR

From the Oxygen repo (web server example):

```bash
sbt "example-web-server/assembly"
# JAR lands at target/artifacts/jars/example-web-server.jar
```

### 2. Generate the wrapper script

Run once next to the JAR:

```bash
java -jar /path/to/your-app.jar --: generate
```

This writes `your-app.sh` beside the JAR (same basename, `.sh` suffix) and prints a `source` command.

Optional fixed JVM args that must precede user args:

```bash
java -jar /path/to/your-app.jar --: generate --oxygen-args -Xmx512m --
java -jar /path/to/your-app.jar --: generate --args --some-flag
```

### 3. Install completion in your shell

```bash
. /path/to/your-app.sh --: export
```

That defines the completion function, aliases your command name to the wrapper, and registers
`complete -F` for both the script path and the alias.

Add the `source` line to `~/.bashrc` (or `~/.bash_profile` on macOS) to persist.

### 4. Try it

```bash
your-app <TAB>
your-app --<TAB>                # flags for the current command
your-app subcmd --<TAB>         # subcommand flags
```

The wrapper forwards normal invocations to `java -jar …`; only completion calls `--: complete`
with the `OXYGEN_CLI_COMPLETE__*` env vars.

## GraalVM native image

When the running binary is **not** named `*.jar`, `generate` assumes a native image and **prints**
the completion script to stdout instead of writing a file.

**Recommended** — no file to manage; load straight from the binary:

```bash
source <(./my-app --: generate)
```

Add that line to `~/.bashrc` to persist. The script embeds the binary's absolute path, so update
it if you move the executable.

**Alternative** — save to a file:

```bash
./my-app --: generate > ~/.local/share/bash-completion/completions/my-app
source ~/.local/share/bash-completion/completions/my-app
```

Unlike the JAR workflow, native images have no `--: export` step — `generate` prints a
self-contained script that registers completion directly.

The generated script calls the binary at `--: complete` (no `java -jar`).

## How it works (debugging)

The bash function sets:

| Env var | Meaning |
|---------|---------|
| `OXYGEN_CLI_COMPLETE__NUM_WORDS` | Number of words being completed |
| `OXYGEN_CLI_COMPLETE__ARG_IDX` | Index of the word under the cursor |
| `OXYGEN_CLI_COMPLETE__ACC_ARGS` | All words joined with the join string |
| `OXYGEN_CLI_COMPLETE__JOIN_STR` | Separator (newline + `-----separator-----` + newline) |

Manual smoke test:

```bash
JOIN=$'\n-----separator-----\n'
export OXYGEN_CLI_COMPLETE__NUM_WORDS=2
export OXYGEN_CLI_COMPLETE__ARG_IDX=1
export OXYGEN_CLI_COMPLETE__ACC_ARGS="my-app${JOIN}client"
export OXYGEN_CLI_COMPLETE__JOIN_STR="$JOIN"
java -jar /path/to/your-app.jar --: complete
```

Stdout should be completion candidates joined with the same separator.

## macOS / v1 script note

Older **oxygen-executable (v1)** completion scripts used `mapfile` and `awk` to split results.
That breaks on typical macOS setups (bash 3.2, BSD awk).

**oxygen-cli** uses a pure-bash split loop — no `awk`, no `mapfile`. Regenerate scripts from a current
build if you still have an old `.sh`.

## WebServerMain

```bash
# from repo root — build the JAR, then generate the wrapper
sbt "example-web-server/assembly"
java -jar target/artifacts/jars/example-web-server.jar --: generate
# source the printed path with --: export
```

For real installs, use a packaged JAR and the workflow above.

## What completes

- Subcommand names
- `--help`, `--help-extra`
- Parameter long/short names from help
- **Enum values** for parameters whose type has `CompletionOptions` (automatic for `StrictEnum`)
- Custom values if you provide `given CompletionOptions[A]`

Bootstrap config flags from the old API (`-f`, `-e`, …) are **not** completed — there is no bootstrap parser.