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
#      the function is renamed. A ::#id search names an Org CUSTOM_ID property,
#      the stable way to cite a SECTION: [[file:containers.org::#layout]] in a
#      doc, [[#layout]] within one.
#
# It also checks the code (*.lisp, *.c, *.sh) and AGENTS.md: a comment cites a doc's section as
# the plain Org link  file:../doc/containers.org::#layout  (relative to the
# citing file; Emacs's org-open-at-point-global and the environment's C-c C-o
# both follow it). Each must resolve, and a section NUMBER (containers.org §3)
# is refused — numbers move whenever a doc is reorganised; ids don't.
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

# Does FILE hold the ::SEARCH a link names? "#id" is an Org CUSTOM_ID property
# (the stable way to cite a section); anything else is searched as plain text.
has_search() {
  case "$1:$2" in
    *.org:\#*) grep -qE "^[[:space:]]*:CUSTOM_ID:[[:space:]]+${2#\#}[[:space:]]*$" "$1" ;;
    *)   grep -qF -- "$2" "$1" ;;
  esac
}

for f in "${FILES[@]}"; do
  [ -f "$f" ] || continue
  dir="$(dirname "$f")"
  # Pull the target out of [[file:TARGET][desc]] and [[file:TARGET]] alike.
  while IFS= read -r link; do
    [ -n "$link" ] || continue
    path="${link%%::*}"
    search=""
    case "$link" in *::*) search="${link#*::}";; esac

    # Prose placeholders: code-browser.org writes a literal [[file:...]] as an
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
    if [ -n "$search" ] && [ -f "$full" ] && ! has_search "$full" "$search"; then
      echo "ANCHOR  $f -> $link   (no '$search' in $path)"
      bad=$((bad + 1))
    fi
  done < <(grep -o '\[\[file:[^]]*\]' "$f" | sed 's/^\[\[file://; s/\]$//')

  # [[#id][...]]: a section of this same file, by its CUSTOM_ID.
  while IFS= read -r id; do
    [ -n "$id" ] || continue
    checked=$((checked + 1))
    if ! has_search "$f" "#$id"; then
      echo "ANCHOR  $f -> #$id   (no CUSTOM_ID '$id' in this file)"
      bad=$((bad + 1))
    fi
  done < <(grep -o '\[\[#[^]]*\]' "$f" | sed 's/^\[\[#//; s/\]$//')
done

# Code cites a doc's section as  file:RELATIVE/doc.org::#id  (Org's own link
# syntax: M-x org-open-at-point-global follows it from any buffer, and the
# environment's C-c C-o does too). The path is relative to the citing file.
# A section NUMBER (doc.org §4) is refused: numbers move when a doc is rewritten.
CODE="$(git ls-files '*.lisp' '*.c' '*.sh' | grep -v -e '^poc/org-data/' -e '^check-links.sh$') AGENTS.md"
while IFS=: read -r f lineno link; do
  [ -n "$link" ] || continue
  path="${link#file:}"; path="${path%%::*}"; id="${link#*::}"
  # AGENTS.md's conventions quote a code comment's link (../doc/...), not its own.
  case "$f:$path" in AGENTS.md:../*) continue;; esac
  full="$(dirname "$f")/$path"
  checked=$((checked + 1))
  if [ ! -f "$full" ]; then
    echo "MISSING $f:$lineno -> $link"; bad=$((bad + 1))
  elif ! has_search "$full" "$id"; then
    echo "ANCHOR  $f:$lineno -> $link   (no CUSTOM_ID '${id#\#}')"; bad=$((bad + 1))
  fi
done < <(grep -noE 'file:[^] ")]*(doc/[^] ")]*|README)\.org::#[A-Za-z0-9_-]+' $CODE)  # tests' fixtures (file:x.org) are not docs

while IFS= read -r hit; do
  [ -n "$hit" ] || continue
  echo "NUMBER  $hit   (cite file:...::#id, not a section number)"
  bad=$((bad + 1))
done < <(grep -nE '[A-Za-z0-9_-]+\.org\]?\]?[ ,]*§ ?[0-9]' $CODE)

echo "checked $checked link(s); $bad broken, $skipped skipped"
[ "$bad" -eq 0 ]
