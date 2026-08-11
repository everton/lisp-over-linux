#!/usr/bin/env bash
#
# refresh-screenshot.sh — regenerate the README screenshots.
#
# TWO demos, same machinery, selected by --mode:
#
#   repl    (default)  the framebuffer CONSOLE: the supervisor menu, then 'r'
#                      into the Lisp REPL, a few colored forms, and the
#                      Land-of-Lisp alien in the top-right corner.
#                      -> media/screenshots/screenshot-<date>.png
#   browser            the pixel ENVIRONMENT: 'b' from the menu launches the
#                      code browser full-screen on /dev/fb0 under KD_GRAPHICS
#                      (fbcon stops drawing; every pixel is ours).
#                      -> media/screenshots/browser-<date>.png
#
# Both boot the built image HEADLESS in QEMU and drive it with no human at the
# keyboard.
#
#   scene NAME         one CONCEPT illustration for doc/code-browser.org: the
#                      module index, the dispatch matrix, the live debugger,
#                      the Workspace, the Spotter.
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
# To change WHAT the repl demo types, edit the FORMS list in the Python driver
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

OVMF_CODE="/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS="/usr/share/OVMF/OVMF_VARS_4M.fd"

say() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

# ---- args ------------------------------------------------------------------
DO_BUILD=1
MODE=repl
SCENE=""
# The named SCENES (--scene NAME) are the concept illustrations embedded in
# doc/code-browser.org. Each is defined in the Python driver below as a menu key
# plus a deterministic key sequence; they are reproducible because the browser's
# lists are alphabetical and its default selection is row 0. Keep this list and
# the SCENES dict in the driver in step.
SCENE_NAMES="modules package matrix debugger workspace spotter"
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
say "Driving the guest over QMP send-key (mode: $MODE)"
QMP_SOCK="$QMP" PPM_OUT="$PPM" MODE="$MODE" SCENE="$SCENE" python3 - <<'PY'
import socket, json, time, sys, os

QMP   = os.environ["QMP_SOCK"]
PPM   = os.environ["PPM_OUT"]
MODE  = os.environ.get("MODE", "repl")
SCENE = os.environ.get("SCENE", "")

# The literal forms typed into the console REPL, in order. EDIT HERE to change
# what the screenshot demonstrates. Kept stable across font versions so the shots
# stay comparable: green => 42, the green "SBCL" string, the Linux kernel release
# (software-version) — the "Lisp over Linux" theme in one line — then a small
# multi-line loop that spells numbers out with (format ~r), chosen to show off
# the live highlighter (rainbow-nested parens, a green "~r" string, a dim ;
# comment, and the dim "  ...>" continuation prompt), and finally magenta
# :announce. A '\n' inside a form drives the REPL's multi-line continuation.
FORMS = [
    "(+ 40 2)",
    "(lisp-implementation-type)",
    "(software-version)",
    "(loop for n in '(1 42 2026)   ; spell them out\n"
    "      collect (format nil \"~r\" n))",
    "(draw-alien :announce t)",
]

# The browser demo, as QMP QKeyCodes sent one at a time after 'b' has launched
# the environment. EMPTY on purpose: the DEFAULT view is already the best
# picture. Rooted at CL-USER, the browser opens the package list (kind-coloured
# rows, 319 definitions) in the left pane with the SELECTED definition's
# syntax-highlighted source beside it — tabs on top, commands in the status bar.
# That fills the screen; drilling with ["down","down","ret"] lands on whatever
# short function happens to be second in the list and leaves most of the canvas
# empty. EDIT HERE to change the shot; each key repaints the whole canvas.
# 'esc' is deliberately NOT sent; we want to end INSIDE the browser.
BROWSER_KEYS = []

# --- the named SCENES: the concept illustrations in doc/code-browser.org -----
# Each is (menu-key, [steps]). A step is one of:
#   ("k",  qcode)      a single key
#   ("c",  [qcodes])   a chord, e.g. ("c", ["ctrl","c"])
#   ("h",  (mod,key))  press key while mod is HELD (see Qmp.hold)
#   ("m",  (x,y))      left-click at absolute x,y (see Qmp.click)
#   ("t",  "text")     type a literal string
#   ("w",  seconds)    extra dwell
# These are deterministic because every browser list is sorted alphabetically and
# a fresh pane opens with row 0 selected — so "down N times" always lands in the
# same place. If you add a definition to examples.lisp, re-check 'matrix'.
#
# NOT a scene: the effective-method onion. A :matrix pane has no keyboard cell
# cursor (matrix-cell-click in poc/shell.lisp is the only way in), so it needs a
# scripted mouse click — and Qmp.click could not land one. The slam-to-corner
# origin works, but the follow-up move never takes effect, most likely because
# the ~32 PS/2 packets of the slam overflow QEMU's queue and later events are
# dropped. Try stepping the cursor in small increments if you pick this up.
SCENES = {
    # the module index (§4⅔): the table of contents, first module previewed beside
    "modules":   ("b", []),
    # the flat CL-USER package view — the old default root, kept on 'l'
    "package":   ("l", []),
    # the Workspace (§3a): a scratch buffer whose verbs eval
    "workspace": ("p", []),
    # the dispatch matrix (§4a) for COLLIDE, the 2-axis showcase in examples.lisp:
    #   modules -> initramfs/examples (3rd) -> collide (3rd) -> "dispatch matrix"
    "matrix":    ("b", [("k","down"),("k","down"),("k","ret"),
                        ("k","down"),("k","down"),("k","ret"),
                        ("w",1.0),("k","ret")]),
    # the live debugger (phase 6): Workspace -> a form that errors -> Debug it
    "debugger":  ("p", [("w",1.0),("t","(/ 1 0)"),("w",0.8),
                        ("c",["ctrl","c"]),("c",["ctrl","d"]),("w",2.0)]),
    # the Spotter (§4¾): the floating palette over the dimmed browser. Super must
    # be HELD (see Qmp.hold) — a send-key chord releases it too fast to survive
    # the evdev-modifier / cooked-tty split.
    "spotter":   ("l", [("w",1.0),("h",("meta_l","spc")),("w",1.5),("t","coll")]),
}

# --- char -> QMP QKeyCode(s). Shifted chars send a [shift, key] chord. -------
PLAIN = {' ': 'spc', '-': 'minus', "'": 'apostrophe', ';': 'semicolon',
         '/': 'slash', '.': 'dot', ',': 'comma'}
for c in "abcdefghijklmnopqrstuvwxyz0123456789":
    PLAIN[c] = c
SHIFT = {'(': '9', ')': '0', '+': 'equal', ':': 'semicolon', '"': 'apostrophe',
         '*': '8', '_': 'minus', '~': 'grave_accent', '?': 'slash'}

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
    def ev(self, *events):
        self.cmd("input-send-event", events=list(events))
    def hold(self, mod, key):
        """Press KEY while MOD is genuinely held down.

        send-key auto-releases the whole chord immediately, which is too fast for
        the browser: it reads modifiers from the raw evdev KEYBOARD but the
        character from the cooked tty (fb-browser.lisp's poll loop refreshes
        Ctrl/Shift/Super first, then reads the byte). If the modifier is already
        released by the time the byte is read, the chord is lost — which is
        exactly how a Super-Space came back as a plain space. So hold it."""
        k = lambda d, s: {"type": "key", "data": {"down": d,
                                                  "key": {"type": "qcode", "data": s}}}
        self.ev(k(True, mod)); time.sleep(0.2)
        self.ev(k(True, key)); time.sleep(0.1); self.ev(k(False, key)); time.sleep(0.2)
        self.ev(k(False, mod))
    def click(self, x, y):
        """Left-click at absolute (X, Y).

        The guest's PS/2 mouse is RELATIVE, and the browser accumulates deltas
        into its own *cursor-x*/*cursor-y* (clamped to the screen). So we first
        slam to the top-left corner with a huge negative delta — which the clamp
        turns into a known origin of (0,0) — and only then move by exactly (x,y).
        That makes the click independent of wherever the cursor happened to be."""
        rel = lambda a, v: {"type": "rel", "data": {"axis": a, "value": v}}
        btn = lambda d: {"type": "btn", "data": {"down": d, "button": "left"}}
        # The dwell after the slam is NOT cosmetic. Queued relative deltas are
        # summed before the browser clamps them, so slam and move sent close
        # together become one -4000+x motion that still clamps to 0 — the cursor
        # never leaves the corner. Give the guest time to consume the slam (it
        # repaints the whole 1280x800 canvas in Lisp per event) so the clamp has
        # actually happened before the second move is queued.
        self.ev(rel("x", -4000), rel("y", -4000)); time.sleep(2.0)
        self.ev(rel("x", x), rel("y", y));         time.sleep(2.0)
        self.ev(btn(True)); time.sleep(0.12); self.ev(btn(False))

def type_str(q, s, cps=0.05):
    for ch in s:
        if ch == '\n':
            q.ret(); time.sleep(0.6)      # submit a continuation line, await "...>"
        else:
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
print("connected to QMP; driving mode=%s..." % MODE, flush=True)

if MODE == "repl":
    # menu: choose 'r' to enter the REPL (cooked read-line -> needs Enter)
    q.key('r'); time.sleep(0.2); q.ret(); time.sleep(1.5)

    # each Enter triggers eval + a colored result line
    for form in FORMS:
        type_str(q, form); time.sleep(0.3); q.ret(); time.sleep(1.5)

    time.sleep(1.0)                        # let the last redraw + alien settle
else:
    # menu: a single letter launches run-browser-fb rooted somewhere ('b' the
    # CL-USER package, 'l' the module index, 'p' the Workspace). The menu read is
    # still COOKED, so it needs the Enter; from then on the browser owns the
    # keyboard in raw mode and single keycodes are what it wants.
    menu_key, steps = ('b', [("k", k) for k in BROWSER_KEYS])
    if MODE == "scene":
        menu_key, steps = SCENES[SCENE]
    q.key(menu_key); time.sleep(0.2); q.ret()
    # Give it time to load the font, build the canvas, open evdev, and paint the
    # first full-screen frame. This is much slower than a REPL prompt: it is a
    # 1280x800 canvas rendered pixel by pixel in Lisp.
    time.sleep(8.0)
    for kind, arg in steps:
        if kind == "k":
            q.cmd("send-key", keys=[{"type": "qcode", "data": arg}])
            time.sleep(1.2)                # each keypress repaints the whole canvas
        elif kind == "c":
            q.cmd("send-key", keys=[{"type": "qcode", "data": k} for k in arg])
            time.sleep(1.2)
        elif kind == "h":
            q.hold(arg[0], arg[1]); time.sleep(1.2)
        elif kind == "m":
            q.click(arg[0], arg[1]); time.sleep(1.2)
        elif kind == "t":
            type_str(q, arg, cps=0.08)
            time.sleep(0.5)
        elif kind == "w":
            time.sleep(arg)
    time.sleep(2.0)                        # let the final frame settle
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
DATED="$SHOTS/$BASENAME-$STAMP.png"
cp "$PNG" "$DATED"
ln -sfn "screenshots/$BASENAME-$STAMP.png" "$LINK"   # relative -> portable

# Point the README's inline image at the real dated PNG. GitHub does NOT follow
# repo symlinks when rendering images, so linking media/screenshot.png (a symlink)
# would break on github.com — we reference the concrete file and keep it current.
# Each mode owns its own link line, matched on the basename it writes.
README="$MICRO/README.org"
if [ -f "$README" ] && [ "$MODE" != scene ]; then
  sed -i -E "s#\[\[\./media/screenshots/$BASENAME-[^]]*\.png\]\]#[[./media/screenshots/$BASENAME-$STAMP.png]]#" "$README"
  # first run of a mode: the README may still point at the pre-archive path
  [ "$MODE" = repl ] && \
    sed -i -E "s#\[\[\./media/screenshot\.png\]\]#[[./media/screenshots/screenshot-$STAMP.png]]#" "$README"
fi

# doc/code-browser.org embeds these shots too (the scenes, plus the 'browser' one
# it reuses for the accordion). It lives in doc/, so its links are one level up
# (../media/...). Run this for EVERY mode, not just scenes: otherwise a --mode
# browser refresh would update the README and silently leave the doc pointing at
# a dated file that no longer exists. Same rule as the README — reference the
# concrete dated file, never the symlink, so it renders on github.com.
CB="$MICRO/doc/code-browser.org"
if [ -f "$CB" ]; then
  sed -i -E "s#\[\[file:\.\./media/screenshots/$BASENAME-[^]]*\.png\]\]#[[file:../media/screenshots/$BASENAME-$STAMP.png]]#" "$CB"
fi

say "Wrote $DATED ($(stat -c%s "$DATED") bytes)"
echo "  $(basename "$LINK") -> $(readlink "$LINK")   (local convenience symlink)"
echo "  README.org image link -> media/screenshots/$BASENAME-$STAMP.png"
echo "  review it, then: git add media/screenshots media/*.png README.org && git commit"
