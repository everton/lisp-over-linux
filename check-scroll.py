#!/usr/bin/env python3
"""check-scroll.py — assert the browser's scrolling behaviour in the BOOTED guest.

    ./check-scroll.py            # boots iso_root under QEMU, exits 0 if all pass

poc/render-frame.lisp checks what a frame LOOKS like. This checks what the machine
DOES with real input, which no host render can: it boots the actual image, drives
the framebuffer browser over QMP — send-key for keys, and input-send-event with the
wheel-up/wheel-down buttons, which QEMU's PS/2 mouse turns into genuine evdev
REL_WHEEL records — and then reads the browser's OWN state back over the net REPL.
The assertions are on (pane-top …), (pane-sel …) and (tv-scroll …), not on pixels.

Run ./build.sh first if you changed the Lisp; this boots iso_root as it stands.

TWO REUSABLE PIECES, if you write another guest assertion:

  Qmp.hold(mod, key) — press KEY while MOD is genuinely held. A plain send-key
  chord auto-releases too fast for the browser, which reads modifiers from the raw
  evdev KEYBOARD and the character from the cooked tty; release the modifier before
  the byte is read and the chord is lost (a Super-Space arrives as a plain space).

  Repl.probe(form) — the guest serves the EDITING repl, so it echoes every
  character back wrapped in ANSI colour and a naive read returns echo, not answers.
  probe() makes the answer self-identifying: the form prints <<value>> and we regex
  <<([^<>~]*)>>. The echo of the source can never match, because the echo contains
  the literal ~s.

TWO WAYS THIS TEST PROVED NOTHING BEFORE, both fixed here and worth remembering:
the module index is ~19 rows and FITS on an 800-line screen, so its viewport cannot
move and every wheel assertion passed vacuously; and drilling whatever happened to
be under the cursor landed on a three-line defparameter, which is shorter than its
viewport and so cannot scroll either. Hence the flat CL-USER list below, and
reaching a long definition deliberately through the Spotter.
"""
import json, os, re, socket, subprocess, sys, tempfile, time

MICRO = os.path.dirname(os.path.abspath(__file__))
WORK = os.environ.get("WORK") or tempfile.mkdtemp(prefix="lol-check-scroll.")
os.makedirs(WORK, exist_ok=True)
OVMF_CODE = "/usr/share/OVMF/OVMF_CODE_4M.fd"
OVMF_VARS = "/usr/share/OVMF/OVMF_VARS_4M.fd"
QMP = os.path.join(WORK, "qmp.sock")

for p in (OVMF_CODE, OVMF_VARS):
    if not os.path.exists(p):
        sys.exit("missing %s" % p)

subprocess.run(["cp", OVMF_VARS, os.path.join(WORK, "vars.fd")], check=True)
if os.path.exists(QMP):
    os.unlink(QMP)

qemu = subprocess.Popen([
    "qemu-system-x86_64", "-machine", "q35", "-m", "2048",
    "-drive", "if=pflash,format=raw,readonly=on,file=%s" % OVMF_CODE,
    "-drive", "if=pflash,format=raw,file=%s" % os.path.join(WORK, "vars.fd"),
    "-drive", "file=fat:rw:%s/iso_root,format=raw,if=ide" % MICRO,
    "-netdev", "user,id=net0,hostfwd=tcp::4005-:4005",
    "-device", "virtio-net-pci,netdev=net0",
    "-serial", "file:%s/serial.log" % WORK, "-display", "none", "-no-reboot",
    "-qmp", "unix:%s,server,nowait" % QMP,
], stderr=open(os.path.join(WORK, "qemu.err"), "w"))


class Qmp:
    def __init__(self, path):
        for _ in range(120):
            try:
                self.s = socket.socket(socket.AF_UNIX); self.s.connect(path); break
            except OSError:
                time.sleep(1)
        else:
            sys.exit("no QMP socket")
        self.f = self.s.makefile("rw", encoding="utf-8", newline="\n")
        self._read()
        self.cmd("qmp_capabilities")

    def _read(self):
        return json.loads(self.f.readline())

    def cmd(self, execute, **args):
        msg = {"execute": execute}
        if args:
            msg["arguments"] = args
        self.s.sendall((json.dumps(msg) + "\n").encode())
        while True:
            r = self._read()
            if "return" in r or "error" in r:
                return r

    def key(self, qcode):
        self.cmd("send-key", keys=[{"type": "qcode", "data": qcode}])

    def ev(self, *events):
        self.cmd("input-send-event", events=list(events))

    def hold(self, mod, key):
        """Press KEY while MOD is genuinely held. send-key auto-releases the whole
        chord immediately, which is too fast for the browser: it reads modifiers from
        the raw evdev KEYBOARD and the character from the cooked tty, so a chord
        released before the byte is read is lost (a Super-Space arrives as a space)."""
        k = lambda d, sc: {"type": "key",
                           "data": {"down": d, "key": {"type": "qcode", "data": sc}}}
        self.ev(k(True, mod)); time.sleep(0.2)
        self.ev(k(True, key)); time.sleep(0.1); self.ev(k(False, key)); time.sleep(0.2)
        self.ev(k(False, mod))

    def type_str(self, text, cps=0.08):
        for ch in text:
            self.key(ch); time.sleep(cps)

    def wheel(self, direction, notches=1):
        """One or more wheel notches. QEMU models the wheel as BUTTONS
        (wheel-up / wheel-down), which its PS/2 mouse turns into REL_WHEEL
        records — exactly what read-mouse decodes."""
        btn = lambda d: {"type": "btn",
                         "data": {"down": d, "button": "wheel-%s" % direction}}
        for _ in range(notches):
            self.ev(btn(True)); time.sleep(0.05); self.ev(btn(False))
            time.sleep(0.9)


class Repl:
    """One long-lived net-REPL connection (the listen backlog is 1, so never two)."""
    def __init__(self):
        for _ in range(120):
            try:
                self.s = socket.create_connection(("127.0.0.1", 4005), timeout=3)
            except OSError:
                time.sleep(1.5); continue
            self.s.settimeout(4)
            self.drain(1.0)
            if self.ev("(+ 1 1)", want="2"):
                return
            self.s.close(); time.sleep(1.5)
        sys.exit("net REPL never answered")

    def drain(self, t):
        end = time.time() + t
        buf = b""
        while time.time() < end:
            try:
                c = self.s.recv(65536)
                if not c:
                    break
                buf += c
            except OSError:
                break
        return buf.decode(errors="replace")

    def ev(self, form, want=None, t=3.0):
        self.s.sendall((form + "\n").encode())
        out = self.drain(t)
        if want is not None:
            return want in out
        return out.strip()

    def probe(self, form, t=3.0):
        """Evaluate FORM and return its printed value as a string.

        The guest serves the EDITING repl, which echoes every keystroke back
        wrapped in ANSI colour, so the raw stream is mostly echo. We make the
        answer self-identifying: the form prints <<value>>, and the echo of the
        source can never match, because the echo contains the literal ~s."""
        self.s.sendall(('(format t "~%%<<~s>>~%%" (ignore-errors %s))\n' % form).encode())
        raw = self.drain(t)
        clean = re.sub(r"\x1b\[[0-9;]*[A-Za-z]", "", raw)
        hits = re.findall(r"<<([^<>~]*)>>", clean)
        return hits[-1].strip() if hits else "?"


q = Qmp(QMP)
print("QMP up; waiting for the guest REPL...", flush=True)
r = Repl()
print("net REPL up", flush=True)
r.ev('(ignore-errors (with-open-file (o "/proc/sys/kernel/printk" :direction :output '
     ':if-exists :append) (write-string "1" o) (terpri o)) :quieted)')
time.sleep(6)

# --- enter the framebuffer browser (cooked read-line needs Enter) ---
# 'l' — the FLAT CL-USER package view, not 'b' (the module index). The module index
# is nineteen rows and fits entirely on an 800-line screen, so its viewport CANNOT
# move and a wheel test there would prove nothing. A few hundred definitions can.
q.key('l'); time.sleep(0.2); q.key('ret')
time.sleep(9)
print("browser kind:", r.probe("(view-kind (pane-view (current-pane)))"), flush=True)

def state(label):
    kind = r.probe("(view-kind (pane-view (current-pane)))")
    scroll = r.probe("(if (pane-tv (current-pane)) "
                     "(lol.textview::tv-scroll (pane-tv (current-pane))) :no-tv)")
    sel = r.probe("(pane-sel (current-pane))")
    top = r.probe("(pane-top (current-pane))")
    pt = r.probe("(if (pane-tv (current-pane)) "
                 "(lol.textview:tv-point-line (pane-tv (current-pane))) :no-tv)")
    print("  %-22s kind=%s scroll=%s sel=%s top=%s point=%s"
          % (label, kind, scroll, sel, top, pt), flush=True)
    return scroll, sel, pt, top

results = []

# ============ A. LIST PANE: PageDown / PageUp / wheel ============
print("\n== A. list pane (the flat CL-USER package view) ==", flush=True)
a0 = state("fresh")
q.key('pgdn'); time.sleep(1.5)
a1 = state("after PageDown")
q.key('pgup'); time.sleep(1.5)
a2 = state("after PageUp")
q.wheel("down", 2); time.sleep(0.5)
a3 = state("after wheel-down x2")
q.wheel("up", 1); time.sleep(0.5)
a4 = state("after wheel-up x1")
def num(v):
    try:
        return int(v)
    except (TypeError, ValueError):
        return None

results.append(("list PageDown moves the selection down",
                num(a0[1]) is not None and num(a1[1]) is not None and num(a1[1]) > num(a0[1])))
results.append(("list PageUp returns it", num(a2[1]) == num(a0[1])))
# Since the list/scrollbar widget landed, a list has a VIEWPORT of its own: the wheel
# moves pane-top and leaves the selection alone. That is the property to pin now.
results.append(("list wheel-down x2 moves the VIEWPORT +6",
                num(a3[3]) is not None and num(a2[3]) is not None and num(a3[3]) - num(a2[3]) == 6))
results.append(("list wheel leaves the SELECTION alone", num(a3[1]) == num(a2[1])))
results.append(("list wheel-up x1 moves the viewport -3",
                num(a4[3]) is not None and num(a3[3]) is not None and num(a3[3]) - num(a4[3]) == 3))

q.key('down'); time.sleep(1.5)
a5 = state("after down (re-attach)")
# Re-attaching does NOT mean "jump back to the top": the widget scrolls only far
# enough to bring the selected row into view, which from a viewport at 3 with the
# selection at 1 means a viewport of 1. The property is that the selection is
# visible again and the viewport moved to make it so.
results.append(("an arrow key re-attaches the viewport to the selection",
                num(a5[1]) == num(a4[1]) + 1
                and num(a5[3]) <= num(a5[1])
                and num(a5[3]) != num(a4[3])))

# ============ B. SOURCE PANE: wheel scrolls viewport, point stays ============
# Reach a LONG definition on purpose, through the Spotter. Drilling whatever happens
# to be under the cursor lands on a three-line defparameter, and a buffer shorter
# than its viewport cannot scroll — the test would pass by being vacuous.
print("\n== B. source pane (a long definition, via the Spotter) ==", flush=True)
q.hold("meta_l", "spc"); time.sleep(2.0)
q.type_str("draw-brow"); time.sleep(1.5)
q.key('ret'); time.sleep(3.5)
b0 = state("fresh source")
q.wheel("down", 3); time.sleep(0.5)
b1 = state("after wheel-down x3")
q.wheel("up", 1); time.sleep(0.5)
b2 = state("after wheel-up x1")
q.key('pgdn'); time.sleep(1.5)
b3 = state("after PageDown")

s0, s1, s2, s3 = num(b0[0]), num(b1[0]), num(b2[0]), num(b3[0])
p0, p1 = num(b0[2]), num(b1[2])
results.append(("source wheel-down scrolls the viewport DOWN",
                s0 is not None and s1 is not None and s1 > s0))
results.append(("wheel-down x3 == 9 lines (3 x +wheel-lines+)",
                s0 is not None and s1 is not None and s1 - s0 == 9))
results.append(("source wheel-up scrolls back UP",
                s2 is not None and s1 is not None and s2 < s1))
results.append(("wheel-up x1 == 3 lines", s1 is not None and s2 is not None and s1 - s2 == 3))
results.append(("the wheel leaves point alone", p0 is not None and p0 == p1))
results.append(("source PageDown moves point", num(b3[2]) is not None and num(b3[2]) != p1))

q.cmd("screendump", filename=os.path.join(WORK, "final.ppm"))
time.sleep(1)

print("\n===== RESULTS =====", flush=True)
ok = True
for label, passed in results:
    print("  %-48s %s" % (label, "PASS" if passed else "FAIL"))
    ok = ok and passed
print("=====", "ALL PASS" if ok else "FAILURES", "=====")

qemu.terminate()
sys.exit(0 if ok else 1)
