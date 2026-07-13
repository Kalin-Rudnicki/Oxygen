#!/usr/bin/env bash
# Interactive psql against the example local Postgres (example/docker-compose.yaml).
#
# Usage:
#   ./scripts/example-db.sh
#   ./scripts/example-db.sh -c 'SELECT 1'
#   PG_PASS=other ./scripts/example-db.sh
#
# Defaults match example/docker-compose.yaml and example/apps/web-server/config/local.yaml.

set -euo pipefail

PGHOST="${PGHOST:-localhost}"
PGPORT="${PGPORT:-5210}"
PGUSER="${PGUSER:-oxygen_username}"
PGDATABASE="${PGDATABASE:-oxygen_example}"
# Accept PG_PASS or standard PGPASSWORD
PGPASSWORD="${PG_PASS:-${PGPASSWORD:-oxygen_password}}"
export PGPASSWORD

exec psql \
  --host="$PGHOST" \
  --port="$PGPORT" \
  --username="$PGUSER" \
  --dbname="$PGDATABASE" \
  "$@"
