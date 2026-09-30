#!/usr/bin/env bash
#
# build.sh — rebuild (and optionally boot) the SBCL-as-init micro-linux.
#
# The fast path rebuilds only the userland — the whole point of the "separate
# initrd" design: changing the Lisp does NOT need a kernel rebuild.
#
#   ./build.sh                 preinit + lisp-init + initramfs.cpio   (seconds)
#   ./build.sh --kernel        ALSO rebuild the kernel + BOOTX64.EFI  (minutes)
#   ./build.sh --run           build, then boot HEADLESS in QEMU and screenshot it
#   ./build.sh --interactive   build, then boot in a QEMU WINDOW you can type into
#   ./build.sh --kernel --run  the full cycle
#   ./build.sh --install-host  build, then copy kernel+cpio to /boot/lisp (sudo)
#                              for the host's GRUB entry — boot without the USB
#
# --run is non-interactive (boots, screenshots, kills QEMU) — for CI/quick checks.
# --install-host (-H) only copies the two files, and only if they changed; the
# GRUB menuentry itself is a one-time manual step (doc/micro-distro.org,
# "Booting from the host's GRUB").
# --interactive (-i) opens a real QEMU window: PID-1's console is tty0 (the
# framebuffer VT, per the console= cmdline), so you drive the REPL from that
# window; the launching terminal mirrors the serial/kernel log. It runs in the
# foreground until you pick "s) shut down" in the menu (QEMU exits) or close it.
#
# Flags combine in any order. Env:
#   QEMU_WAIT=<seconds>   (default 50) how long --run waits before the screenshot.
#   QEMU_DISPLAY=<gtk|sdl> (default gtk) the window backend for --interactive.
#   QEMU_ZOOM=fit         (gtk only) scale the guest display to the window — drag it
#                         bigger to magnify the small font. (Any gtk window also
#                         zooms live with Ctrl-+ / Ctrl-- ; Ctrl-0 resets.)
#
# See doc/sbcl-init.org for the full explanation of every step.

set -euo pipefail

# ---- paths (the script locates itself; external trees are local symlinks) --
# MICRO is wherever this script lives, so the whole project is relocatable.
# KERNEL and SBCL are gitignored symlinks in the project root pointing at a
# Linux source tree and an SBCL source tree — create them with ./deps.sh
# (see README.org). Nothing here hardcodes $HOME.
MICRO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KERNEL="$MICRO/linux"
SBCL="$MICRO/sbcl"

INITRAMFS_DIR="$MICRO/initramfs"

# registry.lisp is loaded FIRST, with a plain (load), because it defines
# load-recording — the loader that captures each module's verbatim source into the
# image (the code browser's definition registry; see doc/code-browser.org §2). The
# modules below are then loaded with (load-recording ...) instead of (load ...),
# which loads them identically AND records their source.
REGISTRY_SRC="$INITRAMFS_DIR/registry.lisp"

# The Lisp userland, split by concern and loaded IN THIS ORDER into the image.
# Ordering constraints: ansi before line-editor+repl (color helpers); line-editor
# before repl (the with-raw-mode macro) AND before shell (which reuses line-editor's
# *completer* / token-start / longest-common-prefix for Tab completion in the editor).
# The .lisp sources are baked into lisp-init by save-lisp-and-die; they are NOT
# shipped in the cpio (only the compiled binary is).
# The graphics toolkit (shared with the poc/ host prototype) is baked in too, so
# the code browser can run on /dev/fb0 from the supervisor menu. x11.lisp is NOT
# baked — it is host-only; the guest presents via lol.fb (framebuffer).
POC_DIR="$MICRO/poc"
LISP_SOURCES=(
  "$INITRAMFS_DIR/model.lisp"        # CLOS/introspection model: classes, GFs, categories, inspector
  "$INITRAMFS_DIR/present.lisp"      # the drill-down present protocol + commands-for (uses model)
  "$INITRAMFS_DIR/examples.lisp"     # didactic CLOS specimens (COLLIDE: a real 2-axis dispatch matrix)
  "$INITRAMFS_DIR/stage.lisp"        # the COLLIDE stage (S3b): bodies, both-ways dispatch, eql enact
  "$INITRAMFS_DIR/worker.lisp"       # a second PROCESS that contributes a view (doc/processes.org S2)
  "$POC_DIR/canvas.lisp"             # pixel buffer + font + draw-string + modifier predicates
  "$POC_DIR/textview.lisp"           # editable text view (uses canvas)
  "$POC_DIR/widgets.lisp"            # phase 3 toolkit: panel/list/scrollbar/grid/card (uses canvas)
  "$POC_DIR/fb.lisp"                 # /dev/fb0 presenter + KD_GRAPHICS (uses canvas)
  "$INITRAMFS_DIR/ansi.lisp"         # 16-color SGR helpers (used by line-editor + repl)
  "$INITRAMFS_DIR/line-editor.lisp"  # the "poor man's readline" toolkit (with-raw-mode, read-escape,
                                     #   *completer* / token-start / longest-common-prefix) — before
                                     #   shell.lisp, which reuses them for Tab completion in the editor
  "$POC_DIR/shell.lisp"              # the drill-down accordion shell (uses present + canvas + textview + line-editor)
  "$INITRAMFS_DIR/process.lisp"      # worker-main, spawn-worker, power-off
  "$INITRAMFS_DIR/framebuffer.lisp"  # draw-alien
  "$INITRAMFS_DIR/meminfo.lisp"      # report-memory (the "m" menu action)
  "$INITRAMFS_DIR/fb-browser.lisp"   # run-browser-fb: the code browser on /dev/fb0 (uses shell+fb+line-editor)
  "$INITRAMFS_DIR/repl.lisp"         # run-repl (uses line-editor)
  "$INITRAMFS_DIR/net.lisp"          # interface ioctls + TCP REPL (uses repl + sb-bsd-sockets)
  "$INITRAMFS_DIR/dhcp.lisp"         # DHCP client + configure-eth0 (uses net + sb-bsd-sockets)
  "$INITRAMFS_DIR/supervisor.lisp"   # the menu loop + init-toplevel (entry)
)
ISO_ROOT="$MICRO/iso_root"
CPIO_OUT="$ISO_ROOT/initramfs.cpio"
BOOTX64="$ISO_ROOT/efi/boot/bootx64.efi"

GEN_INIT_CPIO="$KERNEL/usr/gen_init_cpio"
SBCL_RUNTIME="$SBCL/src/runtime/sbcl"
SBCL_CORE="$SBCL/output/sbcl.core"

OVMF_CODE="/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS="/usr/share/OVMF/OVMF_VARS_4M.fd"

# Paravirtual NIC for QEMU boots: user-mode (SLIRP) networking — no root, no host
# bridge. Gives the guest 10.0.2.15 / gateway 10.0.2.2 / DNS 10.0.2.3 in software,
# and forwards host port 4005 -> guest 4005 so a future Lisp TCP REPL is reachable
# from the host with `nc localhost 4005`. Needs kernel virtio-net (tag #21+).
# See doc/networking.org §4. Shared by both the --interactive and --run QEMU lines.
QEMU_NET=(-netdev "user,id=net0,hostfwd=tcp::4005-:4005" \
          -device virtio-net-pci,netdev=net0)

say()   { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
usage() { sed -n '3,30p' "$0" | sed 's/^# \{0,1\}//'; }

# ---- argument parsing (flags combine) --------------------------------------
DO_KERNEL=0; DO_RUN=0; DO_INTERACTIVE=0; DO_INSTALL_HOST=0
for arg in "$@"; do
  case "$arg" in
    -k|--kernel)       DO_KERNEL=1 ;;
    -r|--run)          DO_RUN=1 ;;
    -i|--interactive)  DO_INTERACTIVE=1 ;;
    -H|--install-host) DO_INSTALL_HOST=1 ;;
    -h|--help)        usage; exit 0 ;;
    *) echo "unknown option: $arg" >&2; usage; exit 1 ;;
  esac
done
# --interactive and the headless --run are mutually exclusive (one screenshots
# and kills, the other hands you the window). If both are asked for, interactive
# wins — the foreground window is clearly what you wanted.
if [ "$DO_INTERACTIVE" -eq 1 ] && [ "$DO_RUN" -eq 1 ]; then
  echo "note: --interactive overrides --run (can't screenshot a window you're driving)." >&2
  DO_RUN=0
fi

# ---- preflight: make sure the tools/inputs exist ---------------------------
# the two external trees are symlinks materialized by ./deps.sh
for link in "$KERNEL:linux" "$SBCL:sbcl"; do
  path="${link%:*}"; name="${link#*:}"
  [ -e "$path" ] || { echo "ERROR: missing ./$name symlink (the $name source tree)." >&2
                      echo "       run  ./deps.sh  to fetch/link it — see README.org" >&2; exit 1; }
done
for f in "$GEN_INIT_CPIO" "$SBCL_RUNTIME" "$SBCL_CORE" \
         "$INITRAMFS_DIR/preinit.c" "$REGISTRY_SRC" "${LISP_SOURCES[@]}" \
         "$INITRAMFS_DIR/initramfs.sbcl.list"; do
  [ -e "$f" ] || { echo "ERROR: missing required input: $f" >&2; exit 1; }
done
command -v musl-gcc >/dev/null || { echo "ERROR: musl-gcc not found (apt install musl-tools)" >&2; exit 1; }

# ---- 1. the C preinit (musl static, a few KB) ------------------------------
say "Building preinit (musl static C)"
musl-gcc -static -O2 -s -o "$INITRAMFS_DIR/preinit" "$INITRAMFS_DIR/preinit.c"

# ---- 2. the SBCL image with the supervisor baked in ------------------------
say "Building lisp-init (SBCL save-lisp-and-die)"
BUILD_LISP="$(mktemp /tmp/build-sup.XXXXXX.lisp)"
trap 'rm -f "$BUILD_LISP"' EXIT
# Bake the sb-bsd-sockets contrib INTO the frozen heap: require it here (loaded
# from $SBCL_HOME's contrib fasls), and save-lisp-and-die freezes it in — so the
# running image has sockets with no fasl/SBCL_HOME needed at runtime. net.lisp
# references the package at load time, so this must come first.
: > "$BUILD_LISP"
printf '(require :sb-bsd-sockets)\n' >> "$BUILD_LISP"
# sb-introspect (who-calls &c) is frozen in the same way; registry.lisp's SENDERS
# wrapper references its package, so require it before loading registry.lisp.
printf '(require :sb-introspect)\n' >> "$BUILD_LISP"
printf '(load "%s")\n' "$REGISTRY_SRC" >> "$BUILD_LISP"   # defines load-recording
for src in "${LISP_SOURCES[@]}"; do            # then record+load each module, in order
  printf '(load-recording "%s")\n' "$src" >> "$BUILD_LISP"
done
cat >> "$BUILD_LISP" <<LISP
(sb-ext:save-lisp-and-die "$INITRAMFS_DIR/lisp-init"
  :executable t :toplevel #'init-toplevel)
LISP
# --no-userinit --no-sysinit so a personal ~/.sbclrc (quicklisp) can't interfere.
# SBCL_HOME points at the built tree's contrib fasls so (require :sb-bsd-sockets)
# resolves at build time (it is frozen into the image, so runtime needs neither).
SBCL_HOME="$SBCL/obj/sbcl-home" "$SBCL_RUNTIME" --core "$SBCL_CORE" \
                --no-userinit --no-sysinit \
                --non-interactive --load "$BUILD_LISP"

# ---- 3. pack the initramfs cpio (no root needed) ---------------------------
# Run from $MICRO so the *relative* source paths in initramfs.sbcl.list
# (initramfs/preinit, …) resolve against the project root — keeps the list
# free of any machine-specific absolute path.
say "Packing initramfs.cpio (gen_init_cpio)"
( cd "$MICRO" && "$GEN_INIT_CPIO" "$INITRAMFS_DIR/initramfs.sbcl.list" > "$CPIO_OUT" )

# ---- 4. (optional) rebuild the kernel and refresh BOOTX64.EFI --------------
if [ "$DO_KERNEL" -eq 1 ]; then
  say "Rebuilding kernel (make bzImage)"
  make -C "$KERNEL" -j"$(nproc)" bzImage
  cp "$KERNEL/arch/x86/boot/bzImage" "$BOOTX64"
fi

# ---- summary ---------------------------------------------------------------
say "Build done"
printf '  %-28s %s bytes\n' "preinit"        "$(stat -c%s "$INITRAMFS_DIR/preinit")"
printf '  %-28s %s bytes\n' "lisp-init"      "$(stat -c%s "$INITRAMFS_DIR/lisp-init")"
printf '  %-28s %s bytes\n' "initramfs.cpio" "$(stat -c%s "$CPIO_OUT")"
printf '  %-28s %s bytes\n' "bootx64.efi"    "$(stat -c%s "$BOOTX64")"
[ "$DO_KERNEL" -eq 0 ] && echo "  (kernel NOT rebuilt — pass --kernel if you changed kernel config)"

# ---- 4b. (optional) install into the HOST's /boot for its GRUB -------------
# The same two files the USB stick carries, but read by the host's GRUB from
# its own ext4 /boot. GRUB's `initrd` command hands the cpio to the EFI stub
# through the LINUX_EFI_INITRD_MEDIA_GUID device path, which the stub tries
# BEFORE the built-in `initrd=initramfs.cpio` — so that cmdline entry is simply
# never consulted here (linux/drivers/firmware/efi/libstub/efi-stub-helper.c,
# efi_load_initrd). `cmp` first, so an unchanged file needs no sudo at all.
if [ "$DO_INSTALL_HOST" -eq 1 ]; then
  HOST_DIR=/boot/lisp
  say "Installing into $HOST_DIR (host GRUB entry)"
  [ -d "$HOST_DIR" ] || sudo mkdir -p "$HOST_DIR"
  for pair in "$BOOTX64:bzImage" "$CPIO_OUT:initramfs.cpio"; do
    src="${pair%%:*}"; dst="$HOST_DIR/${pair##*:}"
    if cmp -s "$src" "$dst"; then
      echo "  $dst  unchanged"
    else
      sudo cp "$src" "$dst" && echo "  $dst  updated"
    fi
  done
  grep -qs '/boot/lisp/bzImage' /boot/grub/grub.cfg \
    || echo "  note: no GRUB entry yet — see doc/micro-distro.org, \"Booting from the host's GRUB\"."
fi

# ---- 5a. (optional) boot it INTERACTIVELY in a QEMU window -----------------
# Foreground, visible window, no screenshot/kill. The window IS the console
# (tty0); type the menu choices there. The terminal you launched from shows the
# serial log + the QEMU monitor (toggle with Ctrl-A C; quit monitor with 'quit').
if [ "$DO_INTERACTIVE" -eq 1 ]; then
  DISPLAY_BACKEND="${QEMU_DISPLAY:-gtk}"
  # QEMU_ZOOM=fit lets the guest framebuffer SCALE to the window (gtk only): drag the
  # window bigger and everything magnifies — the quick way to make the small 6x15
  # font readable until the in-app font-scale lands (see doc/code-browser.org §6.2 /
  # memory: font-scaling-plan). Any gtk window also zooms live with Ctrl-+ / Ctrl-- .
  DISPLAY_OPT="$DISPLAY_BACKEND"
  if [ "${QEMU_ZOOM:-}" = "fit" ] || [ "${QEMU_ZOOM:-}" = "on" ]; then
    if [ "$DISPLAY_BACKEND" = "gtk" ]; then
      DISPLAY_OPT="gtk,zoom-to-fit=on"
    else
      echo "note: QEMU_ZOOM=fit needs the gtk backend; sdl scales via its own window resize." >&2
    fi
  fi
  VARS="$MICRO/ovmf_vars_test.fd"

  [ -e "$OVMF_CODE" ] || { echo "ERROR: OVMF not found: $OVMF_CODE (apt install ovmf)" >&2; exit 1; }
  # gtk/sdl need a graphical session; warn early instead of a cryptic QEMU abort.
  case "$DISPLAY_BACKEND" in
    gtk|sdl)
      if [ -z "${DISPLAY:-}" ] && [ -z "${WAYLAND_DISPLAY:-}" ]; then
        echo "WARNING: no \$DISPLAY/\$WAYLAND_DISPLAY — the '$DISPLAY_BACKEND' window may fail to open." >&2
        echo "         Run this on a graphical session, or set QEMU_DISPLAY (gtk|sdl)." >&2
      fi ;;
  esac

  say "Booting in QEMU ($DISPLAY_BACKEND window) — pick 's) shut down' to exit"
  cp "$OVMF_VARS" "$VARS"                 # fresh writable NVRAM

  # Foreground (no '&'): the script blocks here until QEMU exits. -no-reboot
  # makes the menu's power-off actually quit QEMU (RB_POWER_OFF -> exit).
  qemu-system-x86_64 -machine q35,accel=kvm:tcg -m 2048 \
    -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
    -drive if=pflash,format=raw,file="$VARS" \
    -drive file=fat:rw:"$ISO_ROOT",format=raw,if=ide \
    "${QEMU_NET[@]}" \
    -serial mon:stdio -display "$DISPLAY_OPT" -no-reboot
  exit 0
fi

# ---- 5b. (optional) boot it HEADLESS in QEMU and grab a screenshot ----------
if [ "$DO_RUN" -eq 1 ]; then
  WAIT="${QEMU_WAIT:-50}"
  VARS="$MICRO/ovmf_vars_test.fd"
  SERIAL="$MICRO/serial.log"
  SHOT_PPM="$MICRO/screen.ppm"; SHOT_PNG="$MICRO/screen.png"
  QMP="/tmp/micro-qmp.sock"

  [ -e "$OVMF_CODE" ] || { echo "ERROR: OVMF not found: $OVMF_CODE (apt install ovmf)" >&2; exit 1; }
  say "Booting in QEMU (OVMF, headless) — screenshot after ${WAIT}s"
  cp "$OVMF_VARS" "$VARS"                 # fresh writable NVRAM
  : > "$SERIAL"; rm -f "$SHOT_PPM" "$SHOT_PNG"

  qemu-system-x86_64 -machine q35,accel=kvm:tcg -m 2048 \
    -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
    -drive if=pflash,format=raw,file="$VARS" \
    -drive file=fat:rw:"$ISO_ROOT",format=raw,if=ide \
    "${QEMU_NET[@]}" \
    -serial file:"$SERIAL" -display none -no-reboot \
    -qmp unix:"$QMP",server,nowait 2>/tmp/micro-qemu.err &
  QPID=$!
  sleep "$WAIT"

  # Screenshot + PNG, both in Lisp (host-client/screenshot.lisp -> qmp.lisp +
  # png.lisp). This used to be two blocks of embedded Python leaning on zlib, which
  # made python3 a hard dependency of a Lisp project for the sake of one conversion.
  SBCL_HOME="$SBCL/obj/sbcl-home" "$SBCL_RUNTIME" --core "$SBCL_CORE" \
      --script "$MICRO/host-client/screenshot.lisp" \
      "$QMP" "$SHOT_PPM" "$SHOT_PNG" || true
  kill "$QPID" 2>/dev/null || true

  echo "  serial markers:"
  grep -aE 'Unpacking initramfs|Freeing initrd|Run /init|supervisor|worker' "$SERIAL" \
       | head -8 | sed 's/^/    /' || true
fi

cat <<'TIP'

Next:
  Live window  :  add -display gtk to the qemu line (drop -display none) to watch it boot
  Refresh USB  :  sudo cp iso_root/efi/boot/bootx64.efi  /mnt/EFI/BOOT/BOOTX64.EFI
                  sudo cp iso_root/initramfs.cpio         /mnt/initramfs.cpio
  Host GRUB    :  ./build.sh --install-host   (then pick the entry at boot)
TIP
