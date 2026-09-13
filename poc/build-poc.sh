#!/usr/bin/env bash
#
# build-poc.sh — build & boot the graphics POC on the LoL machine.
#
# Deliberately SEPARATE from the project's build.sh: it touches nothing in
# initramfs/, and the real supervisor is untouched. It builds its own throwaway
# lisp-init whose toplevel is poc-main (poc/run-guest.lisp) and boots it headless
# in QEMU, screenshotting the framebuffer through QMP.
#
#   ./poc/build-poc.sh
#
# Result: poc/screen-fb.png — the same scene run-host.lisp puts in an X11 window,
# but rendered by /dev/fb0 with SBCL as PID 1.
set -euo pipefail

POC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$POC/.." && pwd)"
KERNEL="$ROOT/linux"; SBCL="$ROOT/sbcl"
OUT="$POC/out"; mkdir -p "$OUT"

OVMF_CODE="/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS="/usr/share/OVMF/OVMF_VARS_4M.fd"

say() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

# ---- 1. the SBCL image: canvas + fb + scene + run-guest, toplevel = poc-main --
say "Building poc lisp-init"
BUILD="$(mktemp /tmp/poc-build.XXXXXX.lisp)"
trap 'rm -f "$BUILD"' EXIT
cat > "$BUILD" <<LISP
(load "$POC/canvas.lisp")
(load "$POC/fb.lisp")
(load "$POC/scene.lisp")
(load "$POC/run-guest.lisp")
(sb-ext:save-lisp-and-die "$OUT/lisp-init" :executable t :toplevel #'poc-main)
LISP
SBCL_HOME="$SBCL/obj/sbcl-home" "$SBCL/src/runtime/sbcl" --core "$SBCL/output/sbcl.core" \
  --no-userinit --no-sysinit --non-interactive --load "$BUILD"

# ---- 2. cpio: reuse the project's preinit, add the font + the alien ----------
say "Packing poc initramfs"
cat > "$OUT/poc.cpio.list" <<LIST
dir  /dev                       0755 0 0
dir  /proc                      0755 0 0
dir  /sys                       0755 0 0
dir  /tmp                       1777 0 0
dir  /sbin                      0755 0 0
dir  /lib64                     0755 0 0
dir  /usr                       0755 0 0
dir  /usr/lib                   0755 0 0
dir  /usr/lib/x86_64-linux-gnu  0755 0 0
nod  /dev/console 0600 0 0 c 5 1
nod  /dev/null    0666 0 0 c 1 3
nod  /dev/tty     0666 0 0 c 5 0
file /init      initramfs/preinit 0755 0 0
file /sbin/lisp poc/out/lisp-init 0755 0 0
file /cozette.lolf poc/cozette.lolf 0644 0 0
file /alien.rgba initramfs/alien.rgba 0644 0 0
file /lib64/ld-linux-x86-64.so.2         /usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 0755 0 0
file /usr/lib/x86_64-linux-gnu/libc.so.6 /usr/lib/x86_64-linux-gnu/libc.so.6            0755 0 0
file /usr/lib/x86_64-linux-gnu/libm.so.6 /usr/lib/x86_64-linux-gnu/libm.so.6            0755 0 0
LIST
( cd "$ROOT" && "$KERNEL/usr/gen_init_cpio" "$OUT/poc.cpio.list" > "$OUT/initramfs.cpio" )

# The ESP the firmware boots: the project's existing bzImage + OUR initramfs.
say "Staging ESP"
rm -rf "$OUT/esp"; mkdir -p "$OUT/esp/efi/boot"
cp "$ROOT/iso_root/efi/boot/bootx64.efi" "$OUT/esp/efi/boot/bootx64.efi"
cp "$OUT/initramfs.cpio"                 "$OUT/esp/initramfs.cpio"

# ---- 3. boot headless, screenshot the framebuffer via QMP -------------------
say "Booting QEMU (headless) — screenshot in ${QEMU_WAIT:-40}s"
cp "$OVMF_VARS" "$OUT/vars.fd"
QMP="/tmp/poc-qmp.sock"; rm -f "$QMP"
qemu-system-x86_64 -machine q35 -m 2048 \
  -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
  -drive if=pflash,format=raw,file="$OUT/vars.fd" \
  -drive file=fat:rw:"$OUT/esp",format=raw,if=ide \
  -serial file:"$OUT/serial.log" -display none -no-reboot \
  -qmp unix:"$QMP",server,nowait 2>"$OUT/qemu.err" &
QPID=$!
sleep "${QEMU_WAIT:-40}"

# Screenshot + PNG, both in Lisp (host-client/screenshot.lisp -> qmp.lisp +
# png.lisp) — this was two blocks of embedded Python and a zlib call.
SBCL_HOME="$SBCL/obj/sbcl-home" "$SBCL/src/runtime/sbcl" --core "$SBCL/output/sbcl.core" \
    --script "$ROOT/host-client/screenshot.lisp" \
    "$QMP" "$OUT/screen.ppm" "$POC/screen-fb.png" || true
kill "$QPID" 2>/dev/null || true

echo; echo "  serial:"; grep -aE '\[poc\]' "$OUT/serial.log" | sed 's/^/    /' || true
