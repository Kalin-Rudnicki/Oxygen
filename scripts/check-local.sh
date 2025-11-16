
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

print_header() {
  local color="$1"
  local label_type="$2"
  local label="$3"

  printf "\n${color}--- $label_type: $label ---${NC}\n\n"
}

run_command() {
  local label="$1"
  local cmd_string="$2"
  local on_failure_cmd="$3"

  print_header "$CYAN" "Running" "$label"

  eval "$cmd_string"

  if [ $? -ne 0 ]; then
    if [ -n "$on_failure_cmd" ]; then
      print_header "$YELLOW" "Failure (will attempt to fix)" "$label"
      eval "$on_failure_cmd"
      echo "retrying..."
      run_command "$label" "$cmd_string"
    else
      print_header "$RED" "Failure" "$label"
      exit 1
    fi
  else
    print_header "$GREEN" "Success" "$label"
  fi
}

echo ""
echo "=====| Executing Local CI Script |====="
echo ""

run_command "Check for FIXME comments" "bash check-for-fix.sh"

run_command "Check Formatting" "sbt +scalafmtSbtCheck +scalafmtCheckAll +it/scalafmtCheckAll" "sbt fmt"

run_command "JVM Test" "sbt +jvm-test"
run_command "IT Test" "sbt +it/test"
run_command "Compiles on All Platforms" "sbt +test:compile"
