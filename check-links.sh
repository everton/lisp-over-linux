#!/usr/bin/env bash
#
# check-links.sh — verify every [[file:...]] link in our .org docs resolves.
#
# The .org files are this project's deliverable and they cross-link heavily —
# into each other, into initramfs/ and poc/, and into the gitignored ./linux and
# ./sbcl trees. A link that rots is invisible: org renders it as plain text and
# nothing complains. AGENTS.md ("Keep the docs in sync with reality") asks you to
# sweep them; this is the sweep.
#
# It checks two things per link:
#   1. the FILE exists, resolved relative to the doc the link lives in — depth
#      differs, `../` from doc/ but `../../` from doc/background/, which is the
#      single most common way to get one wrong;
#   2. the ::search anchor, if present, actually appears in that file — so
#      [[file:../initramfs/net.lisp::defun bring-up-interface]] fails loudly once
#      the function is renamed.
#
# Usage:
#   ./check-links.sh            # check everything; exit 1 if anything is broken
#   ./check-links.sh --quiet    # only print problems
#
# Links into ./linux and ./sbcl are reported as SKIPPED (not failures) when those
# gitignored symlinks are absent, since a fresh clone has neither — run deps.sh
# first if you want them checked too.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

QUIET=0
[ "${1:-}" = "--quiet" ] && QUIET=1

# Every .org we maintain. README.org is in the root; the rest live under doc/,
# plus the two subdirectory READMEs.
FILES=(README.org doc/*.org doc/background/*.org poc/README.org kernel/README.org)

bad=0 skipped=0 checked=0

for f in "${FILES[@]}"; do
  [ -f "$f" ] || continue
  dir="$(dirname "$f")"
  # Pull the target out of [[file:TARGET][desc]] and [[file:TARGET]] alike.
  while IFS= read -r link; do
    [ -n "$link" ] || continue
    path="${link%%::*}"
    search=""
    case "$link" in *::*) search="${link#*::}";; esac

    # Prose placeholders: code-browser.org §8½ writes a literal [[file:...]] as an
    # EXAMPLE of a link, not as one. Never a real target; skip both spellings.
    case "$path" in "..."|"…") continue;; esac

    full="$dir/$path"
    checked=$((checked + 1))

    # ./linux and ./sbcl are gitignored symlinks; absent on a fresh clone.
    case "$path" in
      *../linux/*|*../sbcl/*)
        if [ ! -e "$full" ]; then
          [ "$QUIET" -eq 1 ] || echo "SKIP    $f -> $link  (external tree not linked)"
          skipped=$((skipped + 1)); continue
        fi ;;
    esac

    if [ ! -e "$full" ]; then
      echo "MISSING $f -> $link"
      bad=$((bad + 1)); continue
    fi
    if [ -n "$search" ] && [ -f "$full" ] && ! grep -qF -- "$search" "$full"; then
      echo "ANCHOR  $f -> $link   (no '$search' in $path)"
      bad=$((bad + 1))
    fi
  done < <(grep -o '\[\[file:[^]]*\]' "$f" | sed 's/^\[\[file://; s/\]$//')
done

echo "checked $checked link(s); $bad broken, $skipped skipped"
[ "$bad" -eq 0 ]
