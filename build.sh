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
#                         bigger to magnify everything. (The environment's own text
#                         size is SuperL-= / SuperL-- / SuperL-0 — doc/fonts.org.)
#   LOL_FONT=<file.ttf>   the TrueType font to ship (default: the host's DejaVu Sans
#                         Mono); see step 2a' below.
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
  "$INITRAMFS_DIR/logcount-fix.lisp" # FIRST: repairs SBCL 2.6.8's no-POPCNT LOGCOUNT (qemu64 has
                                     #   no POPCNT; without this, compiling arithmetic crashes the guest)
  "$POC_DIR/deflate.lisp"            # lol.deflate: DEFLATE/zlib/gzip WRITERS (gzips the last boot's logs)
  "$INITRAMFS_DIR/session-log.lisp"  # ~/.lol/log: session log, pulse, stall watcher, crash capture,
                                     #   per-boot archiving — early, so every later module can log
  "$INITRAMFS_DIR/sbcl-source.lisp" # SBCL's OWN source, read from the disk on demand (the ./sbcl tree on /home)
  "$INITRAMFS_DIR/model.lisp"       # CLOS/introspection model: classes, GFs, categories, inspector
  "$INITRAMFS_DIR/present.lisp"      # the drill-down present protocol + commands-for (uses model)
  "$INITRAMFS_DIR/tour.lisp"         # the guided tour (subj-tour): stops = live definitions (doc/tour.org)
  "$INITRAMFS_DIR/examples.lisp"     # didactic CLOS specimens (COLLIDE: a real 2-axis dispatch matrix)
  "$INITRAMFS_DIR/stage.lisp"        # the COLLIDE stage (S3b): bodies, both-ways dispatch, eql enact
  "$INITRAMFS_DIR/worker.lisp"       # a second PROCESS that contributes a view (doc/processes.org S2)
  "$POC_DIR/ttf.lisp"                # lol.ttf: TrueType parser + anti-aliased rasteriser (doc/fonts.org)
  "$POC_DIR/canvas.lisp"             # pixel buffer + font + draw-string + modifier predicates
  "$POC_DIR/textview.lisp"           # editable text view (uses canvas)
  "$POC_DIR/widgets.lisp"            # phase 3 toolkit: panel/list/scrollbar/grid/card (uses canvas)
  "$POC_DIR/fb.lisp"                 # /dev/fb0 presenter + KD_GRAPHICS (uses canvas)
  "$INITRAMFS_DIR/ansi.lisp"         # 16-color SGR helpers (used by line-editor + repl)
  "$INITRAMFS_DIR/line-editor.lisp"  # the "poor man's readline" toolkit (with-raw-mode, read-escape,
                                     #   *completer* / token-start / longest-common-prefix) — before
                                     #   shell.lisp, which reuses them for Tab completion in the editor
  "$POC_DIR/clipboard.lisp"          # lol.clipboard: history + PRIMARY + rectangles (doc/clipboard.org)
  "$INITRAMFS_DIR/sysinfo.lisp"      # systray data: battery (sysfs power_supply) + clock (doc/systray.org)
  "$POC_DIR/inflate.lisp"            # lol.inflate: DEFLATE / zlib / gzip in pure Lisp (gzip previews)
  "$POC_DIR/image.lisp"              # lol.image: PNG/GIF/BMP/... -> canvas pixels (image previews; uses inflate)
  "$INITRAMFS_DIR/files.lisp"        # the file system as data: lstat/readdir/rename/... via sb-unix + libc
                                     #   (doc/file-manager.org; after sysinfo for *utc-offset*)
  "$INITRAMFS_DIR/settings.lisp"     # ~/.config/lol/settings: the user's preferences, e.g. the font size (uses files)
  "$INITRAMFS_DIR/file-views.lisp"   # folders & files as SUBJECTS: present/preview/commands (uses present + files)
  "$POC_DIR/terminal.lisp"           # lol.terminal: the VT engine — bytes in, a grid of cells out (doc/terminal.org)
  "$INITRAMFS_DIR/pty.lisp"          # pseudo-terminals via posix_spawn: a program on a pty, as a session leader
  "$POC_DIR/shell.lisp"              # the drill-down accordion shell (uses present + canvas + textview + line-editor)
  "$POC_DIR/file-manager.lisp"       # the file manager's keys, prompts and operations (uses shell + file-views)
  "$INITRAMFS_DIR/runs.lisp"         # run a program, stream its output into a live pane (uses shell + ansi)
  "$INITRAMFS_DIR/containers.lisp"   # the host's Debian as a container: lol-ct's Lisp side (doc/containers.org)
  "$INITRAMFS_DIR/json.lisp"         # lol.json: JSON in/out, objects as a class (doc/agent.org S5)
  "$INITRAMFS_DIR/agent-wire.lisp"   # SSE framing + a plain-http transport (until/besides http.lisp)
  "$INITRAMFS_DIR/agent.lisp"        # the agent: conversation, Messages client, tools, approval, (ask ...)
  "$INITRAMFS_DIR/agent-ui.lisp"     # the agent in the environment: conversation panes, C-c C-a, tick
  "$INITRAMFS_DIR/agent-tools.lisp"  # more tools: lisp-inspect/-applicable/-backtrace, browse, web-fetch (uses agent-ui)
  "$POC_DIR/terminal-pane.lisp"      # SUBJ-TERMINAL: the terminal as a pane (uses shell + terminal + pty)
  "$INITRAMFS_DIR/process.lisp"      # worker-main, spawn-worker, power-off
  "$INITRAMFS_DIR/framebuffer.lisp"  # draw-alien
  "$INITRAMFS_DIR/meminfo.lisp"      # report-memory (the "m" menu action)
  "$INITRAMFS_DIR/fb-browser.lisp"   # run-browser-fb: the code browser on /dev/fb0 (uses shell+fb+line-editor)
  "$INITRAMFS_DIR/web-cdp.lisp"      # the Chrome DevTools protocol over a pipe: JSON, base64, the process
  "$INITRAMFS_DIR/web.lisp"          # the web pane: a headless Chrome tab as a subject (uses shell+fb-browser
                                     #   +image; doc/web-browser.org). Chrome itself comes from the host's disk
  "$INITRAMFS_DIR/worker-live.lisp"  # a LIVE second process: handles, (:invalidate h), a pipe in the poll
                                     #   (uses worker + shell + fb-browser's services + web-cdp's set-nonblocking)
  "$INITRAMFS_DIR/sandbox.lisp"      # Accept tries the definition in a child first (uses worker-live's wire)
  "$INITRAMFS_DIR/repl.lisp"         # run-repl (uses line-editor)
  "$INITRAMFS_DIR/net.lisp"          # interface ioctls + TCP REPL (uses repl + sb-bsd-sockets)
  "$INITRAMFS_DIR/dhcp.lisp"         # DHCP client, the LEASE object, default route, netd renewals (uses net)
  "$INITRAMFS_DIR/dns.lisp"          # stub resolver: resolve-host, TTL cache, /etc/resolv.conf (uses dhcp)
  "$INITRAMFS_DIR/sntp.lisp"         # SNTP: clock drift, stepped through lol-rootd (uses dns)
  "$INITRAMFS_DIR/tls.lisp"          # HTTPS crypto: the HOST's libssl, loaded at RUNTIME only
  "$INITRAMFS_DIR/http.lisp"         # HTTP/1.1 client: http-request, chunked, redirects (uses dns + tls)
  "$INITRAMFS_DIR/wifi.lisp"         # Wi-Fi: rfkill, the HOST's wpa_supplicant, its control socket (uses dhcp)
  "$INITRAMFS_DIR/net-views.lisp"    # the network as a subject: SuperL-n, the Spotter (uses shell + wifi)
  "$INITRAMFS_DIR/journal.lisp"      # every Accept appended to ~/.local/share/lol/changes.lisp, replayed
                                     #   at boot, + the journal view (doc/session-persistence.org)
  "$INITRAMFS_DIR/file-out.lisp"     # write live edits back into the checkout's .lisp files (uses journal)
  "$INITRAMFS_DIR/desk.lisp"         # the desk: tabs + Workspace text saved/restored (uses every subject)
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
# The fw_cfg blob is what switches the REPL ON: since tag #26 the guest serves it
# only when asked (net.lisp, NET-REPL-WANTED-P), and only QEMU has fw_cfg.
QEMU_NET=(-netdev "user,id=net0,hostfwd=tcp::4005-:4005" \
          -fw_cfg name=opt/lol/netrepl,string=1 \
          -device virtio-net-pci,netdev=net0)

# A stand-in for the laptop's disk, attached as virtio-blk when it exists:
# test-disk.img, made by ./make-test-disk.sh with the SAME ext4 UUID as the
# host's root, so lol.conf finds it exactly as it finds the NVMe on the laptop.
# NEVER pass the real /dev/nvme0n1 to QEMU: the host has it mounted. LOL_TEST_DISK
# names another image; set it to "" to boot diskless. doc/storage.org.
LOL_TEST_DISK="${LOL_TEST_DISK-$MICRO/test-disk.img}"
QEMU_DISK=()
if [ -n "$LOL_TEST_DISK" ] && [ -f "$LOL_TEST_DISK" ]; then
  QEMU_DISK=(-drive "file=$LOL_TEST_DISK,format=raw,if=virtio")
fi

say()   { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
usage() { sed -n '3,34p' "$0" | sed 's/^# \{0,1\}//'; }

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
         "$INITRAMFS_DIR/preinit.c" "$INITRAMFS_DIR/lol-ct.c" "$REGISTRY_SRC" "${LISP_SOURCES[@]}" \
         "$INITRAMFS_DIR/initramfs.sbcl.list"; do
  [ -e "$f" ] || { echo "ERROR: missing required input: $f" >&2; exit 1; }
done
command -v musl-gcc >/dev/null || { echo "ERROR: musl-gcc not found (apt install musl-tools)" >&2; exit 1; }

# ---- 1. the C preinit (musl static, a few KB) ------------------------------
say "Building preinit (musl static C)"
musl-gcc -static -O2 -s -o "$INITRAMFS_DIR/preinit" "$INITRAMFS_DIR/preinit.c"

# ---- 1b. lol-ct, the container helper (musl static, like preinit) ----------
# Runs a command inside the host's own Debian, in user+mount+pid+uts namespaces
# (doc/containers.org §5.1). C for the same reason preinit is C, and a separate
# program because unshare(CLONE_NEWUSER) refuses a multithreaded caller — which
# our Lisp PID 1 is.
say "Building lol-ct (musl static C)"
musl-gcc -static -O2 -s -o "$INITRAMFS_DIR/lol-ct" "$INITRAMFS_DIR/lol-ct.c"

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
# The image has no zoneinfo; local time = UTC + one offset, taken from the build
# host (= the laptop the image boots on). Fixed: it does not follow DST
# (doc/systray.org).
printf '(set-utc-offset "%s")\n' "$(date +%z)" >> "$BUILD_LISP"
# Which build this is, baked into the heap: the git revision (+dirty when the tree had
# uncommitted changes) and the UTC time. Every journal record carries the stamp of
# the image that wrote it; replay runs a record freely only under the SAME stamp, and
# shows one from any other build as "pending" (journal.lisp, doc/session-persistence.org).
BUILD_REV="$(git -C "$MICRO" rev-parse --short HEAD 2>/dev/null || echo norev)"
[ -n "$(git -C "$MICRO" status --porcelain 2>/dev/null)" ] && BUILD_REV="$BUILD_REV+dirty"
BUILD_STAMP="$BUILD_REV $(date -u +%Y-%m-%dT%H:%M:%SZ)"
printf '(setf *build-stamp* "%s")\n' "$BUILD_STAMP" >> "$BUILD_LISP"
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

# ---- 2a. libstbi.so: stb_image for JPEG/PSD/HDR previews (doc/previews.org) --
# The one native library the previews use: a 47 KB shared object around the
# public-domain stb_image.h, linked against nothing but libc + libm (which the
# cpio already ships). lol.image loads it lazily from /usr/lib/lol/; without it,
# every pure-Lisp format still decodes and JPEG says why it cannot.
say "Building libstbi.so (stb_image shim)"
"$POC_DIR/build-stbi.sh" "$POC_DIR/out"

# ---- 2a'. the outline font: a monospace TrueType file from THIS host ---------
# The environment draws text with a pure-Lisp TrueType rasteriser (poc/ttf.lisp,
# doc/fonts.org), so it needs a .ttf — and the build host has plenty. Like firmware,
# the font is someone else's work and is never committed: it is copied from the host
# into the (gitignored) initramfs/mono.ttf and from there into the cpio at
# /usr/share/lol/mono.ttf. LOL_FONT=/path/to/font.ttf chooses one; the default is
# DejaVu Sans Mono (Bitstream Vera licence — README.org), else whatever fontconfig
# calls "monospace". No TrueType file at all is not fatal: an EMPTY mono.ttf ships,
# the image fails to parse it and falls back to the committed Cozette bitmap.
FONT_OUT="$INITRAMFS_DIR/mono.ttf"
if [ -z "${LOL_FONT:-}" ]; then
  for cand in /usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf \
              "$(fc-match -f '%{file}' monospace 2>/dev/null || true)"; do
    case "$cand" in *.ttf|*.TTF) [ -f "$cand" ] && { LOL_FONT="$cand"; break; } ;; esac
  done
fi
if [ -n "${LOL_FONT:-}" ] && [ -f "$LOL_FONT" ]; then
  cp "$LOL_FONT" "$FONT_OUT"
  say "font: $LOL_FONT"
else
  : > "$FONT_OUT"
  say "font: no TrueType file found (set LOL_FONT) — the image will use the Cozette bitmap"
fi

# ---- 2b. /etc/lol.conf: who preinit becomes, which disk it mounts ----------
# The build host IS the laptop the image boots on, so take both from here: the
# user running the build, and the UUID of the filesystem that holds $HOME (the
# laptop's ext4 root). preinit reads it; lol.user= / lol.disk= on the kernel
# cmdline override it. LOL_USER / LOL_DISK override it at build time
# (LOL_DISK=none builds an image that never touches a disk). doc/storage.org.
LOL_USER="${LOL_USER:-$(id -un)}"
LOL_DISK="${LOL_DISK:-UUID=$(findmnt -no UUID -T "$HOME" 2>/dev/null || true)}"
[ "$LOL_DISK" = "UUID=" ] && LOL_DISK=none
printf '# generated by build.sh — see doc/storage.org\nuser=%s\ndisk=%s\n' \
  "$LOL_USER" "$LOL_DISK" > "$INITRAMFS_DIR/lol.conf"
say "lol.conf: user=$LOL_USER disk=$LOL_DISK"

# ---- 2c. Wi-Fi firmware, copied from the HOST — never committed -------------
# The driver (iwlwifi, built in since tag #26) loads the card's firmware from
# /lib/firmware the moment it probes; the kernel's firmware loader waits for the
# initramfs to be unpacked first (drivers/base/firmware_loader/main.c,
# wait_for_initramfs), so files in the cpio are found with no user helper
# (CONFIG_FW_LOADER_USER_HELPER is off). The files are Intel's binary blobs:
# redistributable, but we keep this repo source-only and MIT, so they are read
# from the build host's /lib/firmware at pack time and go into the cpio only —
# exactly like the kernel itself (doc/networking.org §7, README licensing table).
#
# CARD-KEYED: one table entry per card. LOL_WIFI_CARD=auto picks it from the build
# host's PCI ids (the host IS the laptop); LOL_WIFI_FIRMWARE=0 leaves Wi-Fi out.
#   ax201  Intel Wi-Fi 6 AX201 (CNVi, "Qu" MAC + "Hr" radio; PCI 8086:34f0 and its
#          siblings in pcie/drv.c). The kernel asks for exactly ONE API version:
#          IWL_22000_UCODE_API_MIN = MAX in cfg/22000.c, read from ./linux below,
#          in one of three silicon steppings (Qu-b0, Qu-c0, QuZ-a0) decided at
#          probe time from a chip register — so all three go in (~1.4 MB each).
#          No .pnvm: that is for AX210 and later.
# The regulatory database (which channels/powers are legal where) must be the
# UPSTREAM-signed one: CFG80211_REQUIRE_SIGNED_REGDB + USE_KERNEL_REGDB_KEYS trust
# only the keys in linux/net/wireless/certs/ (sforshee, wens). Debian's default
# regulatory.db is re-signed by Debian (benh@debian.org) and would be rejected;
# its regulatory.db-upstream twin is signed by wens. Checked with
# `openssl pkcs7 -inform DER -print` on both .p7s files.
LOL_WIFI_FIRMWARE="${LOL_WIFI_FIRMWARE:-1}"
LOL_WIFI_CARD="${LOL_WIFI_CARD:-auto}"
HOST_FIRMWARE="${LOL_HOST_FIRMWARE:-/lib/firmware}"
FW_LIST="$INITRAMFS_DIR/firmware.list"          # generated, gitignored
: > "$FW_LIST"
if [ "$LOL_WIFI_CARD" = auto ]; then
  LOL_WIFI_CARD=none
  for d in /sys/bus/pci/devices/*; do
    [ "$(cat "$d/vendor" 2>/dev/null)" = 0x8086 ] || continue
    case "$(cat "$d/device" 2>/dev/null)" in
      0x34f0|0x02f0|0x06f0|0x3df0|0x4df0|0x43f0|0xa0f0) LOL_WIFI_CARD=ax201 ;;
    esac
  done
fi
if [ "$LOL_WIFI_FIRMWARE" != 0 ] && [ "$LOL_WIFI_CARD" != none ]; then
  FW_FILES=()
  case "$LOL_WIFI_CARD" in
    ax201)
      API=$(awk '/define IWL_22000_UCODE_API_MAX/ {print $3}' \
                "$KERNEL/drivers/net/wireless/intel/iwlwifi/cfg/22000.c")
      for pre in iwlwifi-Qu-b0-hr-b0 iwlwifi-Qu-c0-hr-b0 iwlwifi-QuZ-a0-hr-b0; do
        FW_FILES+=("$pre-$API.ucode:$pre-$API.ucode")
      done ;;
    *) echo "WARNING: no firmware table entry for LOL_WIFI_CARD=$LOL_WIFI_CARD" >&2 ;;
  esac
  if [ -e "$HOST_FIRMWARE/regulatory.db-upstream" ]; then
    FW_FILES+=("regulatory.db-upstream:regulatory.db" "regulatory.db.p7s-upstream:regulatory.db.p7s")
  else
    FW_FILES+=("regulatory.db:regulatory.db" "regulatory.db.p7s:regulatory.db.p7s")
  fi
  printf 'dir /lib 0755 0 0\ndir /lib/firmware 0755 0 0\n' >> "$FW_LIST"
  n=0
  for pair in "${FW_FILES[@]}"; do
    src="${pair%%:*}"; dst="${pair##*:}"
    real="$(readlink -f "$HOST_FIRMWARE/$src" 2>/dev/null || true)"   # Debian links into intel/iwlwifi/
    if [ -n "$real" ] && [ -r "$real" ]; then
      printf 'file /lib/firmware/%s %s 0644 0 0\n' "$dst" "$real" >> "$FW_LIST"
      n=$((n + 1))
    else
      echo "WARNING: Wi-Fi firmware $HOST_FIRMWARE/$src not found (apt install firmware-iwlwifi wireless-regdb)" >&2
    fi
  done
  say "Wi-Fi firmware: card=$LOL_WIFI_CARD, $n file(s) from $HOST_FIRMWARE"
else
  say "Wi-Fi firmware: none (LOL_WIFI_FIRMWARE=$LOL_WIFI_FIRMWARE, card=$LOL_WIFI_CARD)"
fi

# ---- 3. pack the initramfs cpio (no root needed) ---------------------------
# Run from $MICRO so the *relative* source paths in initramfs.sbcl.list
# (initramfs/preinit, …) resolve against the project root — keeps the list
# free of any machine-specific absolute path. The firmware fragment (2c) is
# appended: its paths are the build host's own /lib/firmware, by nature.
say "Packing initramfs.cpio (gen_init_cpio)"
CPIO_LIST="$(mktemp /tmp/initramfs-list.XXXXXX)"
cat "$INITRAMFS_DIR/initramfs.sbcl.list" "$FW_LIST" > "$CPIO_LIST"
( cd "$MICRO" && "$GEN_INIT_CPIO" "$CPIO_LIST" > "$CPIO_OUT" )
rm -f "$CPIO_LIST"

# ---- 4. (optional) rebuild the kernel and refresh BOOTX64.EFI --------------
if [ "$DO_KERNEL" -eq 1 ]; then
  say "Rebuilding kernel (make bzImage)"
  make -C "$KERNEL" -j"$(nproc)" bzImage
  cp "$KERNEL/arch/x86/boot/bzImage" "$BOOTX64"
fi

# ---- summary ---------------------------------------------------------------
say "Build done"
printf '  %-28s %s bytes\n' "preinit"        "$(stat -c%s "$INITRAMFS_DIR/preinit")"
printf '  %-28s %s bytes\n' "lol-ct"         "$(stat -c%s "$INITRAMFS_DIR/lol-ct")"
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
  # window bigger and everything magnifies pixels. Rarely needed now: the environment's
  # own font resizes with SuperL-= / SuperL-- / SuperL-0 (doc/fonts.org); QEMU's
  # gtk window keeps Ctrl-+ / Ctrl-- for its own zoom.
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
    "${QEMU_DISK[@]}" \
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
    "${QEMU_DISK[@]}" \
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
