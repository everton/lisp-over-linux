#!/usr/bin/env bash
#
# refresh-screenshot.sh — regenerate media/screenshot.png (the README shot).
#
# The README shows a *driven* REPL session on the framebuffer console: the
# supervisor menu, then 'r' into the Lisp REPL, then a few colored forms, with
# the Land-of-Lisp alien blitted into the top-right corner. This script boots
# the built image HEADLESS in QEMU and reproduces that session automatically,
# then screenshots it — no human at the keyboard.
#
#   ./refresh-screenshot.sh              refresh userland, boot, drive, screenshot
#   ./refresh-screenshot.sh --no-build   use the CURRENT iso_root as-is (no rebuild)
#
# HOW IT WORKS (two channels into one QEMU boot):
#   * The framebuffer console (what the screendump captures) is driven purely by
#     QMP `send-key` — we type keystrokes into the guest over the QMP socket.
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
# To change WHAT the demo types, edit the FORMS list in the Python driver below.
#
# See doc/sbcl-init.org / doc/framebuffer.org for the surrounding machinery.

set -euo pipefail

MICRO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ISO_ROOT="$MICRO/iso_root"
# Screenshots are archived one-per-date under media/screenshots/, and
# media/screenshot.png (what the README links) is a symlink to the latest — so
# the project's visual evolution is browsable over time. See the memory note.
SHOTS="$MICRO/media/screenshots"
LINK="$MICRO/media/screenshot.png"

OVMF_CODE="/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS="/usr/share/OVMF/OVMF_VARS_4M.fd"

say() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

# ---- args ------------------------------------------------------------------
DO_BUILD=1
for arg in "$@"; do
  case "$arg" in
    --no-build) DO_BUILD=0 ;;
    -h|--help)  sed -n '3,26p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $arg" >&2; exit 1 ;;
  esac
done

# ---- preflight -------------------------------------------------------------
command -v qemu-system-x86_64 >/dev/null || { echo "ERROR: qemu-system-x86_64 not found" >&2; exit 1; }
command -v python3            >/dev/null || { echo "ERROR: python3 not found" >&2; exit 1; }
[ -e "$OVMF_CODE" ] || { echo "ERROR: OVMF not found: $OVMF_CODE (apt install ovmf)" >&2; exit 1; }

# ---- 1. refresh the userland so the shot matches current sources -----------
if [ "$DO_BUILD" -eq 1 ]; then
  say "Refreshing userland (./build.sh)"
  "$MICRO/build.sh" >/dev/null
fi
[ -e "$ISO_ROOT/efi/boot/bootx64.efi" ] || { echo "ERROR: no kernel in iso_root — run ./build.sh --kernel first" >&2; exit 1; }
[ -e "$ISO_ROOT/initramfs.cpio" ]        || { echo "ERROR: no initramfs in iso_root — run ./build.sh first" >&2; exit 1; }

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
qemu-system-x86_64 -machine q35 -m 2048 \
  -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" \
  -drive if=pflash,format=raw,file="$WORK/vars.fd" \
  -drive file=fat:rw:"$ISO_ROOT",format=raw,if=ide \
  -netdev user,id=net0,hostfwd=tcp::4005-:4005 \
  -device virtio-net-pci,netdev=net0 \
  -serial file:"$WORK/serial.log" -display none -no-reboot \
  -qmp unix:"$QMP",server,nowait 2>"$WORK/qemu.err" &
QPID=$!

# ---- 3. drive the console REPL over QMP send-key, then screendump -----------
say "Driving the REPL over QMP send-key"
QMP_SOCK="$QMP" PPM_OUT="$PPM" python3 - <<'PY'
import socket, json, time, sys, os

QMP = os.environ["QMP_SOCK"]
PPM = os.environ["PPM_OUT"]

# The literal forms typed into the console REPL, in order. EDIT HERE to change
# what the screenshot demonstrates. Kept stable across font versions so the shots
# stay comparable: green => 42, the green "SBCL" string, the Linux kernel release
# (software-version) — the "Lisp over Linux" theme in one line — then magenta
# :announce.
FORMS = [
    "(+ 40 2)",
    "(lisp-implementation-type)",
    "(software-version)",
    "(draw-alien :announce t)",
]

# --- char -> QMP QKeyCode(s). Shifted chars send a [shift, key] chord. -------
PLAIN = {' ': 'spc', '-': 'minus'}
for c in "abcdefghijklmnopqrstuvwxyz0123456789":
    PLAIN[c] = c
SHIFT = {'(': '9', ')': '0', '+': 'equal', ':': 'semicolon', '"': 'apostrophe',
         '*': '8', '_': 'minus'}

def keys_for(ch):
    if ch in PLAIN: return [PLAIN[ch]]
    if ch in SHIFT: return ['shift', SHIFT[ch]]
    raise ValueError(f"no keymap for {ch!r} — extend PLAIN/SHIFT")

class Qmp:
    def __init__(self, path):
        self.s = socket.socket(socket.AF_UNIX); self.s.connect(path)
        self.buf = b""
        self._read()                       # server greeting
        self.cmd("qmp_capabilities")
    def _read(self):
        while b"\n" not in self.buf:
            self.buf += self.s.recv(65536)
        line, self.buf = self.buf.split(b"\n", 1)
        return json.loads(line)
    def cmd(self, execute, **args):
        msg = {"execute": execute}
        if args: msg["arguments"] = args
        self.s.sendall((json.dumps(msg) + "\n").encode())
        while True:                        # skip async events, wait for return/error
            r = self._read()
            if "return" in r or "error" in r:
                return r
    def key(self, ch):
        self.cmd("send-key", keys=[{"type": "qcode", "data": k} for k in keys_for(ch)])
    def ret(self):
        self.cmd("send-key", keys=[{"type": "qcode", "data": "ret"}])

def type_str(q, s, cps=0.05):
    for ch in s:
        q.key(ch); time.sleep(cps)

# --- readiness + console-quieting, over ONE connection ----------------------
# Two things must happen out-of-band before we type on the console, and both go
# through the net REPL:
#   (a) confirm the guest REPL is actually SERVING — QEMU hostfwd opens the HOST
#       side of :4005 at t=0, so merely connecting proves nothing; we EVAL a form
#       and wait for the answer, which only works once start-net-repl has run.
#   (b) lower the kernel console loglevel to 1, so late printks (e.g. 'random:
#       crng init done', stirred by our keystroke interrupts) can't paint over
#       the demo on tty0.
# CRUCIAL: the REPL's listen backlog is 1 (net.lisp: socket-listen server 1), so
# a second queued connection triggers a 'TCP: Possible SYN flooding' printk that
# lands on the console. We therefore make EXACTLY ONE connection to the live
# socket and do both (a) and (b) on it — never a second overlapping connect.
def drain(sock, t):
    end = time.time() + t
    while time.time() < end:
        try:
            if not sock.recv(65536): break
        except OSError:
            break

def bring_up_quietly(timeout=120):
    t0 = time.time()
    while time.time() - t0 < timeout:
        try:
            s = socket.create_connection(("127.0.0.1", 4005), timeout=2)
        except OSError:
            time.sleep(1.5); continue            # port refused: REPL not up yet
        s.settimeout(3)
        try:
            drain(s, 1.0)                         # swallow any banner
            s.sendall(b"(+ 1 1)\n")               # (a) is it really evaluating?
            data = b""; end = time.time() + 3; up = False
            while time.time() < end:
                c = s.recv(65536)
                if not c: break
                data += c
                if b"2" in data: up = True; break
            if not up:
                s.close(); time.sleep(1.5); continue
            # (b) up! lower console loglevel to 1 on this SAME socket.
            s.sendall(b'(ignore-errors (with-open-file (o "/proc/sys/kernel/printk" '
                      b':direction :output :if-exists :append) '
                      b'(write-string "1" o) (terpri o)) :quieted)\n')
            drain(s, 2.0)
            try: s.sendall(b":quit\n"); time.sleep(0.3)
            except OSError: pass
            s.close()
            return True
        except OSError:
            s.close(); time.sleep(1.5)
    return False

print("waiting for the guest REPL to serve...", flush=True)
if not bring_up_quietly():
    print("ERROR: net REPL on :4005 never answered", file=sys.stderr); sys.exit(1)
print("guest REPL up, kernel console quieted", flush=True)
# The REPL comes up at start-net-repl, still BEFORE the supervisor clears the
# screen and paints the menu (show-net-interfaces + a sleep + the ESC[2J clear).
# Wait past that so 'r' lands in the menu's read-line, on a clean screen.
time.sleep(6)

q = Qmp(QMP)
print("connected to QMP; driving the console REPL...", flush=True)

# menu: choose 'r' to enter the REPL (cooked read-line -> needs Enter)
q.key('r'); time.sleep(0.2); q.ret(); time.sleep(1.5)

# each Enter triggers eval + a colored result line
for form in FORMS:
    type_str(q, form); time.sleep(0.3); q.ret(); time.sleep(1.5)

time.sleep(1.0)                            # let the last redraw + alien settle
print("screendump:", q.cmd("screendump", filename=PPM), flush=True)
time.sleep(1.0)
print("done", flush=True)
PY

kill "$QPID" 2>/dev/null || true; QPID=""

# ---- 4. PPM (P6) -> PNG with only the stdlib (same converter as build.sh) ---
say "Converting PPM -> PNG"
python3 - "$PPM" "$PNG" <<'PY'
import sys, zlib
from struct import pack
ppm, png = sys.argv[1], sys.argv[2]
d = open(ppm, "rb").read(); assert d[:2] == b'P6'
i = 2; f = []
while len(f) < 3:
    while d[i] in b' \t\n\r': i += 1
    if d[i:i+1] == b'#':
        while d[i] not in b'\n': i += 1
        continue
    j = i
    while d[j] not in b' \t\n\r': j += 1
    f.append(int(d[i:j])); i = j
i += 1; w, h, _ = f; px = d[i:i+w*h*3]
def ch(t, x): return pack(">I", len(x)) + t + x + pack(">I", zlib.crc32(t+x) & 0xffffffff)
raw = bytearray()
for y in range(h):
    raw.append(0); raw += px[y*w*3:(y+1)*w*3]
open(png, "wb").write(b'\x89PNG\r\n\x1a\n'
    + ch(b'IHDR', pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
    + ch(b'IDAT', zlib.compress(bytes(raw), 9)) + ch(b'IEND', b''))
print("  %d x %d" % (w, h))
PY

# ---- 5. install: archive under today's date, repoint the symlink -----------
mkdir -p "$SHOTS"
STAMP="$(date +%F)"                       # e.g. 2026-07-07 (ISO date)
DATED="$SHOTS/screenshot-$STAMP.png"
cp "$PNG" "$DATED"
ln -sfn "screenshots/screenshot-$STAMP.png" "$LINK"   # relative -> portable

# Point the README's inline image at the real dated PNG. GitHub does NOT follow
# repo symlinks when rendering images, so linking media/screenshot.png (a symlink)
# would break on github.com — we reference the concrete file and keep it current.
README="$MICRO/README.org"
if [ -f "$README" ]; then
  sed -i -E "s#\[\[\./media/screenshot[^]]*\.png\]\]#[[./media/screenshots/screenshot-$STAMP.png]]#" "$README"
fi

say "Wrote $DATED ($(stat -c%s "$DATED") bytes)"
echo "  media/screenshot.png -> $(readlink "$LINK")   (local convenience symlink)"
echo "  README.org image link -> media/screenshots/screenshot-$STAMP.png"
echo "  review it, then: git add media/screenshots media/screenshot.png README.org && git commit"
