#!/bin/bash

if git grep "FIX-PRE-MERGE" -- ":!scripts/check-for-fix.sh"; then
  if [[ "$ALLOW_FIX_PRE_MERGE" != "true" ]]; then
    echo "Detected 'FIX-PRE-MERGE'"
    exit 1
  else
    echo "Detected 'FIX-PRE-MERGE', allowing commit anyway"
  fi
fi
