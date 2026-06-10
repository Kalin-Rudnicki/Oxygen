# CLI tab completion (oxygen-executable-2)

oxygen apps expose built-in completion via the `--:` prefix. A bash completion
function talks to your app over env vars; the app prints candidate completions
separated by a fixed join string.

## Prerequisites

- **bash** with programmable completion (`complete` builtin). macOS ships bash
  3.2 by default; install a newer bash if needed:

  ```bash
  brew install bash
  ```

- Your app must be built and runnable (JAR or native binary).

## JAR workflow

### 1. Build a runnable JAR

From the Oxygen repo (example app):

```bash
sbt "oxygen-executable-2JVM/Test/package"
```

Or build your own app’s assembly/fat JAR the way you normally ship it. You need
the path to the JAR that contains your `CliApp.Executable` main class.

### 2. Generate the wrapper script

Run once next to the JAR (or point at it):

```bash
java -jar /path/to/your-app.jar --: generate
```

This writes `your-app.sh` beside the JAR (same basename, `.sh` suffix) and
prints a `source` command.

Optional: pass fixed JVM args that must precede user args:

```bash
java -jar /path/to/your-app.jar --: generate --oxygen-args -Xmx512m --
java -jar /path/to/your-app.jar --: generate --args --some-flag
```

### 3. Install completion in your shell

```bash
. /path/to/your-app.sh --: export
```

That defines the completion function, aliases your command name to the wrapper,
and registers `complete -F` for both the script path and the alias.

Add the `source` line to `~/.bashrc` (or `~/.bash_profile` on macOS) to persist.

### 4. Try it

```bash
your-app <TAB>
your-app client --<TAB>
```

The wrapper forwards normal invocations to `java -jar …`; only completion calls
`--: complete` with the `OXYGEN_CLI_COMPLETE__*` env vars.

## GraalVM native image workflow

When the running binary is **not** named `*.jar`, `generate` assumes a native
image and **prints** the completion script to stdout instead of writing a file.

### 1. Build the native binary

Use your project’s GraalVM / native-image setup (e.g. `nativeImage` task) so you
have an executable, say `./my-app`.

### 2. Generate the script

```bash
./my-app --: generate > ~/.local/share/bash-completion/completions/my-app
chmod +x ~/.local/share/bash-completion/completions/my-app
```

Or save anywhere and `source` it from your shell rc.

The generated script calls `./my-app --: complete` directly (no `java -jar`).

### 3. Load it

```bash
source ~/.local/share/bash-completion/completions/my-app
```

Native images also register completion for `./my-app` and `my-app`.

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

## macOS / cli-v1 AWK issue

Older **oxygen-executable (v1)** completion scripts used `mapfile` and `awk` to
split results. That breaks on typical macOS setups:

- **bash 3.2** has no `mapfile`
- **BSD awk** behaves differently from gawk for multi-line records

**cli-2** (and an updated **cli-v1** script generator) use a pure-bash split
loop instead—no `awk`, no `mapfile`. If a coworker still has an old generated
`.sh`, regenerate it from a current build:

```bash
java -jar /path/to/app.jar --: generate
# then re-source the new script
```

## ExampleApp (this module)

```bash
# from repo root
sbt "oxygen-executable-2JVM/Test/runMain oxygen.executable.ExampleApp --: generate"
# source the printed path with --: export

# then e.g.
example-app client --<TAB>
```

When running via `sbt runMain`, the “JAR” path is sbt’s classpath artifact; for
real installs, use a packaged JAR as above.