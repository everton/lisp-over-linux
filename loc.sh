#!/usr/bin/env bash
# loc.sh — how big is this effort? Lines of code/prose by language.
#
# Counts only git-TRACKED files (so build artifacts — lisp-init, *.cpio,
# *.fasl, bootx64.efi — never inflate the numbers) and buckets each by
# language. Binary assets (*.png, *.rgba) are reported as a file+byte tally,
# not line-counted. Lines are counted with awk's NR so a missing trailing
# newline still counts its last line (wc -l would drop it).
#
# Usage: ./loc.sh
set -euo pipefail
set -f                        # no pathname expansion: `for g in $2` must keep
                              # globs like *.org literal, not expand them vs cwd
cd "$(dirname "$0")"

# label|matcher — matcher is a shell case-glob tested against the basename.
CATS=(
  "Lisp|*.lisp"
  "C|*.c"
  "Shell|*.sh"
  "C header|*.h"
  "Org docs|*.org"
  "Markdown|*.md"
  "Kernel config|config-*"
  "Manifests/misc|*.list *.gitignore LICENSE"
)

declare -A CAT_FILES CAT_LINES
BIN_FILES=0 BIN_BYTES=0
OTHER_FILES=0 OTHER_LINES=0

match() {                     # $1=basename $2=space-separated globs
  local b=$1 g
  for g in $2; do case "$b" in $g) return 0 ;; esac; done
  return 1
}

while IFS= read -r f; do
  [ -f "$f" ] || continue
  base=${f##*/}
  case "$base" in
    *.png|*.rgba)             # binary assets — bytes, not lines
      BIN_FILES=$((BIN_FILES + 1))
      BIN_BYTES=$((BIN_BYTES + $(wc -c < "$f")))
      continue ;;
  esac
  placed=0
  for entry in "${CATS[@]}"; do
    label=${entry%%|*}; globs=${entry#*|}
    if match "$base" "$globs"; then
      CAT_FILES[$label]=$(( ${CAT_FILES[$label]:-0} + 1 ))
      CAT_LINES[$label]=$(( ${CAT_LINES[$label]:-0} + $(awk 'END{print NR}' "$f") ))
      placed=1; break
    fi
  done
  if [ "$placed" -eq 0 ]; then
    OTHER_FILES=$((OTHER_FILES + 1))
    OTHER_LINES=$((OTHER_LINES + $(awk 'END{print NR}' "$f")))
  fi
done < <(git ls-files)

# --- report ---------------------------------------------------------------
printf '\n  %-16s %7s %9s\n' "language" "files" "lines"
printf '  %-16s %7s %9s\n' "----------------" "-----" "---------"

code_files=0 code_lines=0 prose_files=0 prose_lines=0
emit() {   # $1=label
  local l=$1 fn=${CAT_FILES[$1]:-0} ln=${CAT_LINES[$1]:-0}
  [ "$fn" -eq 0 ] && return
  printf '  %-16s %7d %9d\n' "$l" "$fn" "$ln"
}
# code first, then prose, in a stable order
for l in "Lisp" "C" "C header" "Shell"; do
  emit "$l"
  code_files=$((code_files + ${CAT_FILES[$l]:-0}))
  code_lines=$((code_lines + ${CAT_LINES[$l]:-0}))
done
printf '  %-16s %7d %9d   <- code subtotal\n' "" "$code_files" "$code_lines"
for l in "Org docs" "Markdown"; do
  emit "$l"
  prose_files=$((prose_files + ${CAT_FILES[$l]:-0}))
  prose_lines=$((prose_lines + ${CAT_LINES[$l]:-0}))
done
printf '  %-16s %7d %9d   <- prose subtotal\n' "" "$prose_files" "$prose_lines"
emit "Kernel config"
emit "Manifests/misc"
[ "$OTHER_FILES" -gt 0 ] && printf '  %-16s %7d %9d\n' "Other" "$OTHER_FILES" "$OTHER_LINES"

printf '\n  binary assets  : %d files, %d KiB (png/rgba, not line-counted)\n' \
  "$BIN_FILES" "$((BIN_BYTES / 1024))"

total_files=$((code_files + prose_files + ${CAT_FILES["Kernel config"]:-0} \
  + ${CAT_FILES["Manifests/misc"]:-0} + OTHER_FILES))
total_lines=$((code_lines + prose_lines + ${CAT_LINES["Kernel config"]:-0} \
  + ${CAT_LINES["Manifests/misc"]:-0} + OTHER_LINES))
printf '  text total     : %d files, %d lines\n\n' "$total_files" "$total_lines"
