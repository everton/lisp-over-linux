#!/usr/bin/env bash
# loc.sh — how big is this effort? Lines of code/prose by language.
#
# Counts only git-TRACKED files (so build artifacts — lisp-init, *.cpio,
# *.fasl, bootx64.efi — never inflate the numbers) and buckets each by
# language. Binary assets (*.png, *.rgba) are reported as a file+byte tally,
# not line-counted. Lines are counted with awk's NR so a missing trailing
# newline still counts its last line (wc -l would drop it).
#
# A second table splits the CODE by ORIGIN instead of language — the
# measurement STEPS never published (doc/steps.org):
#   ours         what we designed: its size is ours to win or lose
#   conformance  what implements someone else's spec (a wire protocol, a file
#                format, SBCL's own machine code): its floor is set by the spec
#   tests, tooling, vendored — counted, but outside that comparison
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
  "HTML/CSS/JS|*.html *.css *.js"
  "Kernel config|config-*"
  "Manifests/misc|*.list *.gitignore LICENSE"
)

# origin|matcher — matcher is a case-glob tested against the FULL path (in a
# case pattern * also matches /). First match wins, so the order matters.
# file-views.lisp stays in "ours" although it carries tar/zip/ELF readers: they
# are a minority of a file that is otherwise views.
# initramfs/lol-ct.c is "ours" too: it calls the kernel's namespace ABI, but which
# mounts, in which order, with which fallbacks is a design (doc/containers.org);
# ansi.lisp stays "conformance" with its SGR READER added, since both directions
# implement ECMA-48's colour codes. initramfs/web-cdp.lisp is "conformance" (the
# DevTools protocol's framing, plus the JSON and base64 it rides on); web.lisp,
# which decides what a web tab IS in the environment, is "ours". The network:
# dns/sntp/http are RFC wire formats and tls.lisp is an FFI binding to OpenSSL's
# API — conformance; wifi.lisp and net-views.lisp are "ours" (which supplicant
# requests, when, what to keep — a design, though it speaks wpa_supplicant's
# control protocol). poc/terminal.lisp (the VT engine) is conformance too — its
# floor is xterm's behaviour, not our design; pty.lisp and terminal-pane.lisp are ours.
# Persistence (journal.lisp, file-out.lisp, desk.lisp) is "ours": the record shape,
# the replay rules and the desk's reference whitelist are designs
# (doc/session-persistence.org), even though they print with the Lisp printer.
# poc/morph.lisp and poc/morphic.lisp (Morphic, doc/morphic.org) are "ours": the
# vocabulary is Self's and Squeak's, but no spec fixes the code — the damage rules,
# the desk and the trail windows are designs.
# agent-tools.lisp is "ours" although it carries html-text: a skimmer for the
# model, not an HTML parser; agent-store.lisp (the conversation file) is ours too.
ORIGINS=(
  "vendored|poc/vendor/*"
  "tests|check-*.lisp *-test.lisp host-client/guest-harness.lisp host-client/http-test-server.lisp host-client/mock-anthropic.lisp"
  "conformance|poc/ttf.lisp initramfs/json.lisp initramfs/agent-wire.lisp poc/inflate.lisp poc/image.lisp poc/x11.lisp poc/bdf2font.lisp poc/stbi-shim.c initramfs/dhcp.lisp initramfs/dns.lisp initramfs/sntp.lisp initramfs/http.lisp initramfs/tls.lisp initramfs/ansi.lisp poc/terminal.lisp initramfs/logcount-fix.lisp initramfs/web-cdp.lisp host-client/png.lisp host-client/qmp.lisp"
  "tooling|*.sh host-client/* poc/render-frame.lisp doc/guide/*.lisp"
  "ours|*"
)
ORIGIN_ORDER=(ours conformance tests tooling vendored)

declare -A CAT_FILES CAT_LINES ORIG_FILES ORIG_LINES
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
  placed=0 n=$(awk 'END{print NR}' "$f")
  for entry in "${CATS[@]}"; do
    label=${entry%%|*}; globs=${entry#*|}
    if match "$base" "$globs"; then
      CAT_FILES[$label]=$(( ${CAT_FILES[$label]:-0} + 1 ))
      CAT_LINES[$label]=$(( ${CAT_LINES[$label]:-0} + n ))
      placed=1; break
    fi
  done
  case "$label" in Lisp|C|"C header"|Shell)   # code only, never prose
    if [ "$placed" -eq 1 ]; then
      for entry in "${ORIGINS[@]}"; do
        o=${entry%%|*}
        if match "$f" "${entry#*|}"; then
          ORIG_FILES[$o]=$(( ${ORIG_FILES[$o]:-0} + 1 ))
          ORIG_LINES[$o]=$(( ${ORIG_LINES[$o]:-0} + n ))
          break
        fi
      done
    fi ;;
  esac
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
emit "HTML/CSS/JS"     # design mock-ups (media/) and the guide (doc/guide/)
emit "Kernel config"
emit "Manifests/misc"
[ "$OTHER_FILES" -gt 0 ] && printf '  %-16s %7d %9d\n' "Other" "$OTHER_FILES" "$OTHER_LINES"

printf '\n  binary assets  : %d files, %d KiB (png/rgba, not line-counted)\n' \
  "$BIN_FILES" "$((BIN_BYTES / 1024))"

total_files=$((code_files + prose_files + ${CAT_FILES["HTML/CSS/JS"]:-0} + ${CAT_FILES["Kernel config"]:-0} \
  + ${CAT_FILES["Manifests/misc"]:-0} + OTHER_FILES))
total_lines=$((code_lines + prose_lines + ${CAT_LINES["HTML/CSS/JS"]:-0} + ${CAT_LINES["Kernel config"]:-0} \
  + ${CAT_LINES["Manifests/misc"]:-0} + OTHER_LINES))
printf '  text total     : %d files, %d lines\n\n' "$total_files" "$total_lines"

# --- code by origin (doc/steps.org) -----------------------------------------
printf '  %-16s %7s %9s\n' "code by origin" "files" "lines"
printf '  %-16s %7s %9s\n' "----------------" "-----" "---------"
for o in "${ORIGIN_ORDER[@]}"; do
  printf '  %-16s %7d %9d\n' "$o" "${ORIG_FILES[$o]:-0}" "${ORIG_LINES[$o]:-0}"
  if [ "$o" = conformance ]; then
    printf '  %-16s %7d %9d   <- the system we wrote\n' "" \
      "$(( ${ORIG_FILES[ours]:-0} + ${ORIG_FILES[conformance]:-0} ))" \
      "$(( ${ORIG_LINES[ours]:-0} + ${ORIG_LINES[conformance]:-0} ))"
  fi
done
echo
