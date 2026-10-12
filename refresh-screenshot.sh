#!/usr/bin/env bash
#
# refresh-screenshot.sh — regenerate the README screenshots.
#
# TWO demos, same machinery, selected by --mode:
#
#   repl    (default)  the framebuffer CONSOLE: leave the environment the image
#                      boots into ('q'), then 'r' from the supervisor menu
#                      into the Lisp REPL, a few colored forms, and the
#                      Land-of-Lisp alien in the top-right corner.
#                      -> media/screenshots/screenshot-<date>.png
#   browser            the pixel ENVIRONMENT the image boots into: the
#                      code browser full-screen on /dev/fb0 under KD_GRAPHICS
#                      (fbcon stops drawing; every pixel is ours).
#                      -> media/screenshots/browser-<date>.png
#
# Both boot the built image HEADLESS in QEMU and drive it with no human at the
# keyboard.
#
#   scene NAME         one CONCEPT illustration for doc/code-browser.org: the
#                      module index, the dispatch matrix, the live debugger,
#                      the COLLIDE stage,
#                      the Workspace, the Spotter; and, booted with the
#                      stand-in disk (test-disk.img), the file manager.
#                      -> media/screenshots/<name>-<date>.png
#
#   ./refresh-screenshot.sh                    the REPL shot (default)
#   ./refresh-screenshot.sh --mode browser     the code-browser shot
#   ./refresh-screenshot.sh --scene matrix     one doc illustration
#   ./refresh-screenshot.sh --scenes           list the scene names
#   ./refresh-screenshot.sh --no-build         use the CURRENT iso_root as-is
#
# HOW IT WORKS (two channels into one QEMU boot):
#   * The framebuffer console (what the screendump captures) is driven purely by
#     QMP `send-key` — we type keystrokes into the guest over the QMP socket.
#   LOL_REPL_PORT=<n> (default 4005) moves the host side of that hostfwd, so a
#   headless capture can run while an INTERACTIVE QEMU (./build.sh --interactive) is
#   already holding 4005. Without it the boot silently produced no QMP socket, which
#   reads as a mysterious tooling failure rather than "the port is taken".
#
#   * The network REPL on host port 4005 (the hostfwd) is used ONLY out-of-band
#     to lower the kernel printk level, so a late 'random: crng init done' printk
#     (entropy stirred up by our own keystroke interrupts) can't interleave into
#     the console mid-demo. Nothing typed over 4005 appears in the screenshot.
#
# The screenshot reflects WHATEVER is currently in iso_root — including the font
# baked into the kernel. So to change the font (e.g. 8x16 vs TER16x32), rebuild
# the kernel first (./build.sh --kernel); this script does not touch the kernel.
# By default it DOES refresh the userland (fast ./build.sh) so the shot matches
# the current Lisp sources; pass --no-build to skip that.
#
# To change WHAT the repl demo types, edit the *forms* list in the Lisp driver
# below; for the browser demo, edit BROWSER_KEYS.
#
# See doc/sbcl-init.org / doc/framebuffer.org (§9 for KD_GRAPHICS) and
# doc/code-browser.org for the surrounding machinery.

set -euo pipefail

MICRO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ISO_ROOT="$MICRO/iso_root"
# Screenshots are archived one-per-date under media/screenshots/, and
# media/screenshot.png (what the README links) is a symlink to the latest — so
# the project's visual evolution is browsable over time. See the memory note.
SHOTS="$MICRO/media/screenshots"

REPL_PORT="${LOL_REPL_PORT:-4005}"        # host side of the net-REPL hostfwd

# A busy port is the one failure mode that does not announce itself: QEMU exits, no
# QMP socket ever appears, and the driver reports only that. Say so up front instead.
if command -v ss >/dev/null 2>&1 && ss -ltn 2>/dev/null | grep -q ":$REPL_PORT "; then
  echo "port $REPL_PORT is already in use (an interactive QEMU?)." >&2
  echo "  re-run with:  LOL_REPL_PORT=4105 $0 $*" >&2
  exit 1
fi

OVMF_CODE="/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS="/usr/share/OVMF/OVMF_VARS_4M.fd"

say() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

# ---- args ------------------------------------------------------------------
DO_BUILD=1
MODE=repl
SCENE=""
# The named SCENES (--scene NAME) are the concept illustrations embedded in
# doc/code-browser.org. Each is defined in host-client/drive-scene.lisp as a menu key
# plus a deterministic key sequence; they are reproducible because the browser's
# lists are alphabetical and its default selection is row 0. Keep this list and
# the SCENES dict in the driver in step.
SCENE_NAMES="modules package matrix debugger workspace spotter stage stage-debugger stage-accept stage-inspect stage-workspace closure worker files files-archive spotter-path sbcl-source"
# These boot with the STAND-IN disk attached (test-disk.img from ./make-test-disk.sh,
# snapshot=on so nothing persists), so the file manager shows its fake home. Never
# the real disk — doc/storage.org says why.
DISK_SCENES="files files-archive spotter-path sbcl-source"
while [ $# -gt 0 ]; do
  case "$1" in
    --no-build) DO_BUILD=0 ;;
    --mode)     shift; MODE="${1:-}" ;;
    --mode=*)   MODE="${1#*=}" ;;
    --browser)  MODE=browser ;;          # shorthand
    --scene)    shift; MODE=scene; SCENE="${1:-}" ;;
    --scene=*)  MODE=scene; SCENE="${1#*=}" ;;
    --scenes)   echo "$SCENE_NAMES" | tr ' ' '\n'; exit 0 ;;
    -h|--help)  sed -n '3,32p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $1" >&2; exit 1 ;;
  esac
  shift
done
case "$MODE" in
  repl)    BASENAME=screenshot; LINK="$MICRO/media/screenshot.png" ;;
  browser) BASENAME=browser;    LINK="$MICRO/media/browser.png" ;;
  scene)
    case " $SCENE_NAMES " in
      *" $SCENE "*) : ;;
      *) echo "unknown --scene: '$SCENE' (try --scenes)" >&2; exit 1 ;;
    esac
    BASENAME="$SCENE"; LINK="$MICRO/media/$SCENE.png" ;;
  *) echo "unknown --mode: $MODE (expected 'repl', 'browser' or 'scene')" >&2; exit 1 ;;
esac

# ---- preflight -------------------------------------------------------------
command -v qemu-system-x86_64 >/dev/null || { echo "ERROR: qemu-system-x86_64 not found" >&2; exit 1; }
[ -e "$OVMF_CODE" ] || { echo "ERROR: OVMF not found: $OVMF_CODE (apt install ovmf)" >&2; exit 1; }

# ---- 1. refresh the userland so the shot matches current sources -----------
if [ "$DO_BUILD" -eq 1 ]; then
  say "Refreshing userland (./build.sh)"
  "$MICRO/build.sh" >/dev/null
fi
[ -e "$ISO_ROOT/efi/boot/bootx64.efi" ] || { echo "ERROR: no kernel in iso_root — run ./build.sh --kernel first" >&2; exit 1; }
[ -e "$ISO_ROOT/initramfs.cpio" ]        || { echo "ERROR: no initramfs in iso_root — run ./build.sh first" >&2; exit 1; }

DISK_ARGS=()
if [ "$MODE" = scene ] && case " $DISK_SCENES " in *" $SCENE "*) true ;; *) false ;; esac; then
  [ -f "$MICRO/test-disk.img" ] || { echo "ERROR: scene '$SCENE' needs test-disk.img — run ./make-test-disk.sh" >&2; exit 1; }
  DISK_ARGS=(-drive file="$MICRO/test-disk.img",format=raw,if=virtio,snapshot=on)
fi

# ---- scratch dir + cleanup -------------------------------------------------
WORK="$(mktemp -d)"
QPID=""
cleanup() { [ -n "$QPID" ] && kill "$QPID" 2>/dev/null || true; rm -rf "$WORK"; }
trap cleanup EXIT

QMP="$WORK/qmp.sock"; PPM="$WORK/shot.ppm"; PNG="$WORK/shot.png"
cp "$OVMF_VARS" "$WORK/vars.fd"          # fresh writable NVRAM

# ---- 2. boot headless with a QMP control socket ----------------------------
# Same machine/mem/OVMF/drive/netdev as build.sh's --run, but serial->file,
# -display none, and a -qmp unix socket we drive keystrokes through.
say "Booting headless (QEMU + QMP)"
qemu-system-x86_64 -machine q35,accel=kvm:tcg -m 2048 \
  -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
  -drive if=pflash,format=raw,file="$WORK/vars.fd" \
  -drive file=fat:rw:"$ISO_ROOT",format=raw,if=ide \
  -netdev user,id=net0,hostfwd=tcp::$REPL_PORT-:4005 \
  -fw_cfg name=opt/lol/netrepl,string=1 \
  -device virtio-net-pci,netdev=net0 \
  "${DISK_ARGS[@]}" \
  -serial file:"$WORK/serial.log" -display none -no-reboot \
  -qmp unix:"$QMP",server,nowait 2>"$WORK/qemu.err" &
QPID=$!

# ---- 3. drive the console REPL over QMP send-key, then screendump -----------
say "Driving the guest over QMP send-key (mode: $MODE)"
SBCL_HOME="$MICRO/sbcl/obj/sbcl-home" "$MICRO/sbcl/src/runtime/sbcl" \
    --core "$MICRO/sbcl/output/sbcl.core" \
    --script "$MICRO/host-client/drive-scene.lisp" "$QMP" "$PPM" "$MODE" "$SCENE"

kill "$QPID" 2>/dev/null || true; QPID=""

# ---- 4. PPM (P6) -> PNG with only the stdlib (same converter as build.sh) ---
say "Converting PPM -> PNG"
SBCL_HOME="$MICRO/sbcl/obj/sbcl-home" "$MICRO/sbcl/src/runtime/sbcl" \
    --core "$MICRO/sbcl/output/sbcl.core" \
    --script "$MICRO/host-client/ppm2png.lisp" "$PPM" "$PNG"

# ---- 5. install: archive under today's date, repoint the symlink -----------
mkdir -p "$SHOTS"
STAMP="$(date +%F)"                       # e.g. 2026-07-07 (ISO date)
DATED="$SHOTS/$BASENAME-$STAMP.png"
cp "$PNG" "$DATED"
ln -sfn "screenshots/$BASENAME-$STAMP.png" "$LINK"   # relative -> portable

# Point the README's inline image at the real dated PNG. GitHub does NOT follow
# repo symlinks when rendering images, so linking media/screenshot.png (a symlink)
# would break on github.com — we reference the concrete file and keep it current.
# Each mode owns its own link line, matched on the basename it writes.
README="$MICRO/README.org"
if [ -f "$README" ] && [ "$MODE" != scene ]; then
  # The date is matched digit-by-digit, not as [^]]*, because scene names share
  # prefixes: with a loose pattern, --scene stage rewrote the link to
  # stage-debugger-<date>.png, silently repointing a different illustration.
  sed -i -E "s#\[\[\./media/screenshots/$BASENAME-[0-9]{4}-[0-9]{2}-[0-9]{2}\.png\]\]#[[./media/screenshots/$BASENAME-$STAMP.png]]#" "$README"
  # first run of a mode: the README may still point at the pre-archive path
  [ "$MODE" = repl ] && \
    sed -i -E "s#\[\[\./media/screenshot\.png\]\]#[[./media/screenshots/screenshot-$STAMP.png]]#" "$README"
fi

# The docs embed these shots too. They live in doc/, so their links are one level
# up (../media/...). Run this for EVERY mode, not just scenes: otherwise a --mode
# browser refresh would update the README and silently leave a doc pointing at a
# dated file that no longer exists. Same rule as the README — reference the
# concrete dated file, never the symlink, so it renders on github.com.
#
# EVERY .org under doc/, not a named list. This used to name code-browser.org
# alone, which was true when it was the only doc with pictures and quietly stopped
# being a rule the moment a second one wanted them: the new doc's links would
# simply never be rewritten, and would rot without anything failing. A list would
# have had the same shape as the bug — correct until the next addition — so the
# glob is the fix rather than one more entry. The sed only matches the exact
# dated pattern, so a doc with no screenshots is untouched.
for DOC in "$MICRO"/doc/*.org "$MICRO"/doc/background/*.org; do
  [ -f "$DOC" ] || continue
  sed -i -E "s#\[\[file:\.\./media/screenshots/$BASENAME-[0-9]{4}-[0-9]{2}-[0-9]{2}\.png\]\]#[[file:../media/screenshots/$BASENAME-$STAMP.png]]#" "$DOC"
  sed -i -E "s#\[\[file:\.\./\.\./media/screenshots/$BASENAME-[0-9]{4}-[0-9]{2}-[0-9]{2}\.png\]\]#[[file:../../media/screenshots/$BASENAME-$STAMP.png]]#" "$DOC"
done

say "Wrote $DATED ($(stat -c%s "$DATED") bytes)"
echo "  $(basename "$LINK") -> $(readlink "$LINK")   (local convenience symlink)"
echo "  README.org image link -> media/screenshots/$BASENAME-$STAMP.png"
echo "  review it, then: git add media/screenshots media/*.png README.org && git commit"
