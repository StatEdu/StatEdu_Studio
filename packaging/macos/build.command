#!/bin/sh
set -eu
cd "$(dirname "$0")"
PYTHON="${STATEDU_PYTHON:-python3}"
"$PYTHON" tools/prepare_macos.py --verify-stage .
"$PYTHON" tools/doctor_macos.py --stage .
"$PYTHON" tools/prepare_macos.py --check-stage . --build
