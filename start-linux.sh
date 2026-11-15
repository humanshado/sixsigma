#!/bin/bash
# Start Six Sigma Studio on Linux:  ./start-linux.sh   (add --no-browser --port=8080 to serve only)
cd "$(dirname "$0")"
if ! command -v Rscript >/dev/null; then
  echo "R was not found. Install R 4.1 or later (e.g. sudo apt install r-base) and try again."
  exit 1
fi
exec Rscript launch.R "$@"
