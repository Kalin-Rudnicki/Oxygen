#!/usr/bin/env bash
# Run the Oxygen example web server locally, in one command.
#
# Does three things (each opt-out):
#   1. brings up the local Postgres (example/docker-compose.yaml, port 5210)
#   2. builds the Scala.js UI  (example-ui-web/webComp → example/apps/web-server/res/js/main.js)
#   3. runs the web server     (example-web-server/run, APP_CONFIG=config/local.yaml)
#
# Steps 2+3 share a single sbt session, so sbt boots once.
# Ctrl+C stops the server (and leaves the DB running — stop it with --db-down).
#
# Usage (from anywhere):
#   ./scripts/run-example-web.sh                 # db up + build UI + run  (http://localhost:3210)
#   ./scripts/run-example-web.sh --no-web-comp   # skip the UI build (res/js already built)
#   ./scripts/run-example-web.sh --full          # fullLinkJS UI build (slower, optimized)
#   ./scripts/run-example-web.sh --clean         # sbt clean first
#   ./scripts/run-example-web.sh --no-db         # don't touch docker (DB already up elsewhere)
#   ./scripts/run-example-web.sh --db-down       # just stop the docker DB and exit
#
# Env:
#   APP_CONFIG   config file (default: example/apps/web-server/config/local.yaml)
#   PORT         only used for the "open me" message (default: read from config, else 3210)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
COMPOSE_FILE="${ROOT_DIR}/example/docker-compose.yaml"
CONFIG="${APP_CONFIG:-${ROOT_DIR}/example/apps/web-server/config/local.yaml}"

# --- args ---------------------------------------------------------------------

do_db=true
do_db_down=false
do_web_comp=true
do_clean=false
full=false

usage() { sed -n '/^# Run the Oxygen/,/^#   PORT /p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; }

for arg in "$@"; do
  case "${arg}" in
    --no-db)        do_db=false ;;
    --db-down)      do_db_down=true ;;
    --no-web-comp)  do_web_comp=false ;;
    --clean)        do_clean=true ;;
    --full)         full=true ;;
    -h | --help)    usage; exit 0 ;;
    *) echo "error: unknown arg: ${arg}" >&2; usage >&2; exit 1 ;;
  esac
done

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "error: required command not found: $1" >&2; exit 1; }
}

# --- --db-down short-circuit --------------------------------------------------

if [[ "${do_db_down}" == true ]]; then
  require_cmd docker
  echo "🛑 stopping example DB"
  docker compose -f "${COMPOSE_FILE}" down
  exit 0
fi

# --- config sanity ------------------------------------------------------------

[[ -f "${CONFIG}" ]] || { echo "error: config not found: ${CONFIG}" >&2; exit 1; }

# port is only for the friendly URL; read http.port from config, fall back to 3210.
read_port() {
  awk '
    /^http:[[:space:]]*$/ { in_http = 1; next }
    /^[^[:space:]#]/      { in_http = 0 }
    in_http && $1 == "port:" { v = $2; gsub(/["\047]/, "", v); print v; exit }
  ' "${CONFIG}"
}
PORT="${PORT:-$(read_port || true)}"
PORT="${PORT:-3210}"

# --- 1. docker db -------------------------------------------------------------

if [[ "${do_db}" == true ]]; then
  require_cmd docker
  echo "🐘 ensuring example Postgres is up (docker compose up -d)"
  docker compose -f "${COMPOSE_FILE}" up -d
fi

# --- 2. build the UI (its own sbt invocation) ---------------------------------
#
# NOTE: webComp is a greedy sbt *input task* (spaceDelimited(...).parsed) — if it shares a
# command line with anything after it, it eats that as its own argument. So it CANNOT be
# combined with example-web-server/run in one `sbt "a" "b"` call (run would silently never
# execute). Build here, run below, as two separate invocations. The persistent sbt server
# keeps the second one fast.

require_cmd sbt
cd "${ROOT_DIR}"

build_cmds=()
[[ "${do_clean}" == true ]] && build_cmds+=("clean")
if [[ "${do_web_comp}" == true ]]; then
  if [[ "${full}" == true ]]; then
    build_cmds+=("example-ui-web/webComp --full")
  else
    build_cmds+=("example-ui-web/webComp")
  fi
fi

if [[ ${#build_cmds[@]} -gt 0 ]]; then
  echo
  echo "🔨 building UI: sbt ${build_cmds[*]}"
  echo "    (first run compiles a lot and may sit quietly for a few minutes — not a hang)"
  sbt "${build_cmds[@]}"
fi

# --- 3. run the server (its own sbt invocation; blocks until Ctrl+C) ----------

echo
echo "🚀 starting example web server"
echo "    config: ${CONFIG}"
echo "    sbt:    example-web-server/run"
echo
echo "    It's up when you see:  web-server started on port ${PORT}"
echo "    Then open http://localhost:${PORT}  (Ctrl+C stops it; DB stays up — --db-down to stop that)"
echo

export APP_CONFIG="${CONFIG}"
exec sbt "example-web-server/run"
