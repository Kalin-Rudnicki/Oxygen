#!/bin/bash

CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
  script_is_sourced=1
else
  script_is_sourced=0
fi

__print_error_message() {
    local message="$1"
    printf "\n${RED}${message}${NC}\n\n"
}

__detected_error_while() {
    local prev_call_res="$?"
    local prev_call_name="$1"

    if [[ "$prev_call_res" != "0" ]]; then
        __print_error_message "--- Detected error while $prev_call_name ---"
        return 1
    else
        return 0
    fi
}

__require_sourced() {
    if [[ "${script_is_sourced}" != "1" ]]; then
        __print_error_message "Error : The provided command requires the script to be sourced."
        return 1
    else
        return 0
    fi
}

__build_main() {

  case "$1" in
    example-web-server)
      __require_sourced || return 1

      printf "\n=====| Building JAR |=====\n\n"
      sbt example-web-server/assembly
      __detected_error_while "building jar" || return 1

      printf "\n=====| Generating Scripts |=====\n\n"
      java -jar target/artifacts/jars/example-web-server.jar --: generate target/artifacts/scripts/example-web-server.sh --oxygen-args \
           -f=example/apps/web-server/src/main/resources/local.json
      __detected_error_while "generating scripts" || return 1

      printf "\n=====| Sourcing Scripts |=====\n\n"
      . ./target/artifacts/scripts/example-web-server.sh --: export
      __detected_error_while "sourcing scripts" || return 1

      printf "\n=====| Complete |=====\n\n"
      return 0

      ;;
    *)
      __print_error_message "Invalid app: '$1'"
      return 1
  esac

}

if [[ "${script_is_sourced}" == "1" ]]; then
  __build_main "$@"
  return $?
else
  __build_main "$@"
  exit $?
fi
