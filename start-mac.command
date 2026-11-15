#!/bin/bash
# Double-click to start Six Sigma Studio on macOS (Terminal opens and runs it).
cd "$(dirname "$0")"
RSCRIPT="$(command -v Rscript)"
[ -z "$RSCRIPT" ] && [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ] && RSCRIPT=/Library/Frameworks/R.framework/Resources/bin/Rscript
[ -z "$RSCRIPT" ] && [ -x /opt/homebrew/bin/Rscript ] && RSCRIPT=/opt/homebrew/bin/Rscript
if [ -z "$RSCRIPT" ]; then
  echo "R was not found. Install R 4.1 or later from https://cran.r-project.org/bin/macosx/ and try again."
  read -n 1 -s -r -p "Press any key to close."
  exit 1
fi
"$RSCRIPT" launch.R "$@"
