# AGENTS.md — working notes for AI agents in `~/lisp-over-linux`

This is a **learning project**: a very minimal EFISTUB Linux that a UEFI laptop
boots directly from a USB stick (no GRUB/bootloader), now running **SBCL Common
Lisp as PID 1**. The user is here to *understand* the system, so prefer
**didactic, well-commented** code and explanations over production hardening.

## What lives here

| File | Role |
|------|------|
| `README.org` | **Entry point.** What the project is + how to reconstruct the env (the `./linux`/`./sbcl` symlinks, `deps.sh`, build). Read first. The only `.org` in root. |
| `doc/` | All other `.org` documentation lives here (keeps the root clean). |
| `doc/micro-distro.org` | Base EFISTUB micro-linux: boot chain, USB creation, first-build diagnosis. |
| `doc/sbcl-init.org` | SBCL as PID 1: preinit shim, supervisor, separate-initrd, build. Extends `micro-distro.org`. |
| `doc/kernel-config.org` | **Inventory of every kernel option we enable and why.** Single source of truth for `.config`. |
| `doc/framebuffer.org` | Deep-dive: framebuffer → efifb → fbcon, `/dev/fb0`, font size, drawing images. Explains the Display options. |
| `doc/fonts.org` | Orientation guide: how our pure-Lisp renderer turns a character into pixels (PSF bitmap → `draw-string` → screen), and how to change the font/size. Companion to `framebuffer.org`. |
| `doc/line-editing.org` | Deep-dive: readline-class input in pure Lisp (raw-mode termios via FFI, fbcon ANSI). |
| `doc/input.org` | Deep-dive: **input devices** — evdev (`/dev/input/eventN`, the 24-byte `input_event`), finding the mouse without udev, `poll(2)` over three fds, the wheel, and why a bare Ctrl needs the keyboard's own evdev. The input sibling of `framebuffer.org`. |
| `doc/networking.org` | Networking: kernel `NET`/`INET`/`virtio-net` + userland `sb-bsd-sockets`, static IP/DHCP, the TCP REPL. Wired path working; DNS/Wi-Fi ahead. |
| `doc/code-browser.org` | **The active major effort.** Full design + phase tracker for the Smalltalk/Pharo-style code browser: the `present` drill-down protocol, the accordion shell, trails/tabs/Spotter, the debugger. The longest doc here and the best maintained — treat its phase list as truth. |
| `doc/fonts.org` | How our pure-Lisp renderer turns a character into pixels (PSF/`.lolf` bitmap → `draw-string`), and how to change the font or its size. |
| `doc/inspector-history.org` | **Design doc that became code (2026-09-20).** What an inspector could do on Interlisp-D, the Lisp Machines, Smalltalk, CLIM and SLIME — a 13-power catalogue — then which four are cheap here, each *prototyped against `./sbcl` before being written down* (`all-instances` via the heap walker, `search-roots` for "why is this alive", Smalltalk-style eval-in-the-object's-context, settable `sb-di` frame locals), with measured numbers and effort. Four of them now ship; §6 records what they actually cost and four ways the estimate was wrong. Still owed: settable debugger frame locals (§5.4). |
| `doc/agent.org` | **PLAN.** An LLM client written in Lisp and living in the image: HTTP/JSON/SSE over the existing sockets, `registry.lisp`/`model.lisp`/`present.lisp` as its tool surface, doorways from the REPL / a browser tab / the debugger, and TLS 1.3 from scratch as a late sub-project. Also records why we are *not* running the `claude` CLI in the guest. |
| `doc/x11.org` | The pure-Lisp X11 **client** (`poc/x11.lisp`): the wire format, MIT-MAGIC-COOKIE auth, `PutImage` + banding, keycodes vs keysyms. The host dev backend — deliberately NOT in `LISP_SOURCES`, so it never ships in the image. |
| `doc/processes.org` | **Concurrent components that share graphical resources.** Why exactly one process may write `/dev/fb0`, the split between a peer that sends a *view* (cheap — `present` is not a pixel protocol) and one that sends *pixels* (a compositor, declined with reasons), what two processes would do to one mouse, and fork/exec vs copy-on-write. §6 is the **`sb-posix` audit**, which concluded by changing nothing: its inline entry points cannot be RECOMPILED in the guest, so using them would cost live editing — a rule that binds every future dependency. §7 is **built** (`initramfs/worker.lisp`).|
| `doc/background/background.org` | General firmware/CPU/hardware theory (UEFI handoff, x86 modes, USB-HID, multicore). "Not project rationale." |
| `doc/background/learn-networking.org` | From-scratch networking tutorial with a progress tracker + deep links into the code. |
| `doc/background/learn-clos.org` | From-scratch CLOS + MOP tutorial (same shape as `learn-networking.org`). The Lisp background the browser assumes: multiple dispatch, specificity, method combination, conditions/restarts, and the `sb-mop` accessors `model.lisp` is built from. |
| `build.sh` | One-step rebuild of userland (+ `--kernel`, `--run`). Location-independent; uses the `./linux`/`./sbcl` symlinks. |
| `deps.sh` | Fetch / link / update the external `./linux` (kernel) and `./sbcl` trees; create the gitignored symlinks. |
| `check-links.sh` | Verify every `[[file:...]]` link in the `.org` docs resolves, anchors included. Run before committing doc changes. |
| `check-scroll.lisp` | Boot the image under QEMU and **assert the browser's scrolling behaviour** — drives it over QMP (keys, and real evdev `REL_WHEEL` via QEMU's wheel buttons) while reading `pane-top` / `pane-sel` / `tv-scroll` back over the net REPL. Exits non-zero on any failure. |
| `refresh-screenshot.sh` | Re-capture the screenshots by booting the image headless under QEMU and driving it over QMP: `--mode repl`, `--mode browser`, and `--scene NAME` for the `code-browser.org` illustrations (`--scenes` lists them). |
| `initramfs/preinit.c` | The C PID-1 shim: mounts /proc /sys /dev /tmp, then `execv`s the Lisp. |
| `initramfs/supervisor.lisp` | The Lisp supervisor (PID 1): REPL / worker / power-off menu. |
| `initramfs/registry.lisp` | The definition registry: `load-recording` captures every module's source INTO the image, so the running system can show its own code (`source-of`, `show-source`, `senders`). Phase 0 of the code browser — see `doc/code-browser.org`. |
| `initramfs/model.lisp` | The code browser's MODEL layer (Phase 1): CLOS introspection via `sb-mop` — class DAG (`show-class`), GF/method graph (`show-gf`, `applicable-methods`), `categorize`, object inspector (`show-inspect`). Also the **inspector powers** (`doc/inspector-history.org`): a closure's captures as places (`closure-captures` — settable only when SBCL boxed them), `all-instances` (heap walk), `retention-path` (`sb-ext:search-roots`), `eval-in-context` (Smalltalk's `self` pane), `apply-function`. Headless/REPL-driven. |
| `initramfs/present.lisp` | The drill-down `present`/`commands-for` protocol + affordance model (code browser §4½). `(present subject)`→a view of drillable items; `(commands-for subject)`→declarative commands (menu/halo/palette/key). Includes the **function view** (a closure's captures as fields, source/senders/disassembly doors) and the instances / retention / disassembly subjects. Headless; `(show-view subject)` demos it. The Workspace primer ships a runnable `COMPILE`-a-closure example. |
| `initramfs/fb-browser.lisp` | The code browser **on the bare framebuffer** as PID 1 (menu `b`): `poc/shell.lisp`'s shell rendered via `present-fb` under `KD_GRAPHICS`, keyboard + evdev mouse multiplexed with `poll(2)`, the Spotter overlay, and the live debugger (`run-debugger-fb`). |
| `initramfs/framebuffer.lisp` | `/dev/fb0` basics: read the live geometry from sysfs (`read-fb-geometry`) and blit the Land-of-Lisp alien (`draw-alien`). See `doc/framebuffer.org`. |
| `initramfs/line-editor.lisp` | "Poor man's readline" in pure Lisp: raw-mode termios via FFI, history, completion. See `doc/line-editing.org`. |
| `initramfs/repl.lisp` | The `r)` menu action: the read-eval-print loop driven by the line editor, with `<Tab>` symbol completion. Every read/eval guarded so a bad form cannot kill PID 1. |
| `initramfs/ansi.lisp` | ANSI SGR colour, restricted to the 16 classic colours — the kernel VT renders no more than that. Used by the REPL and line editor. |
| `initramfs/net.lisp` | Bring `eth0` up from Lisp (`SIOCSIFADDR`/`SIOCSIFFLAGS` ioctls via FFI) and serve the REPL over TCP on :4005. |
| `initramfs/dhcp.lisp` | A real DHCP handshake (DISCOVER/OFFER/REQUEST/ACK) in pure Lisp, then configure `eth0` from the lease. |
| `initramfs/process.lisp` | Process lifecycle for the supervisor: spawn a throwaway worker, and power off via `reboot(2)` through libc FFI. |
| `initramfs/meminfo.lisp` | The `m)` menu action: RAM accounting in honest layers — Lisp heap, process RSS, the kernel's own resident image, the whole machine. |
| `initramfs/examples.lisp` | A gallery of CLOS specimens that exist purely to be *looked at* in the browser — chiefly `COLLIDE`, a genuinely 2-argument generic whose dispatch matrix is a real grid with real gaps. |
| `initramfs/worker.lisp` | **A second PROCESS that contributes a view** (`doc/processes.org` §2, §7). `run-program "/sbin/lisp" ("job" …)` re-execs the image in a `job` role; the child computes and `prin1`s a plain `(:title :rows)` plist down a pipe; the parent reads it with `*read-eval*` nil and rebuilds real subjects around the names, so a row computed elsewhere drills into a local view. `subj-worker` + one `present` method. Synchronous — liveness is still owed. Falls back to in-process on the host (no `/sbin/lisp`) and says so in the view. |
| `initramfs/initramfs.sbcl.list` | `gen_init_cpio` description of the rootfs. |
| `poc/` | Prototype for the code browser's pixel UI (pure-Lisp X11 client + framebuffer, a text view). Host-runnable; see `poc/README.org` and `doc/code-browser.org`. |
| `host-client/` | **Host-side** tools (NOT shipped in the image, never run in the guest): the network-REPL raw-forwarding client (C), plus the Lisp tooling the build and screenshot scripts run — `qmp.lisp` (drive a running QEMU), `png.lisp` (write a PNG, DEFLATE included), and the three CLIs over them. These replaced all the project's Python; see its README for the compression trade. The deliberate opposite of `initramfs/`. |

External source trees are reached via **gitignored symlinks** in the project root,
created by `deps.sh` (never hardcode a path):
- `./linux` → a Linux kernel tree (`.config`, `arch/x86/boot/bzImage`, `usr/gen_init_cpio`).
- `./sbcl` → a built SBCL tree (`src/runtime/sbcl`, `output/sbcl.core`).

On this machine they currently point at `./sources/linux-7.2.5` and `./sources/sbcl-2.6.8`
(both source trees live under the project's gitignored `sources/` dir).

## Keep the docs in sync with reality — REQUIRED

The `.org` files are the deliverable, not an afterthought. When you change the
system, update the matching doc **in the same turn**:

- **Changed the kernel `.config`** (enabled/disabled any option) →
  update **`doc/kernel-config.org`**: fix the relevant table, bump the kernel tag,
  and add a line to its "Change log". Re-run the audit `grep`s documented in
  that file's "How to regenerate / audit" section and reconcile differences.
  **Also refresh the tracked snapshot** in the same turn (run from the project
  root): `cp linux/.config kernel/config-7.2.5` (the live `.config` lives outside
  the repo behind the `./linux` symlink; `kernel/config-7.2.5` is our copy).
- **Changed the boot chain / USB / EFISTUB** → update `doc/micro-distro.org`.
- **Changed `preinit.c`, the Lisp modules, the initramfs list, or `build.sh`** →
  update `doc/sbcl-init.org` (and its STATUS line).
- **Bumped the kernel build** → bump the tag (`#NN`) consistently across
  `doc/kernel-config.org`, `doc/sbcl-init.org`, and `doc/micro-distro.org`.

If a doc and the code disagree, the **code/`.config` is truth** — fix the doc.

### Verify, don't assert

**Every factual claim in a doc gets checked against the source before it is
written** — our code, `./linux`, `./sbcl`, or a running SBCL. This is not
pedantry; it routinely catches real errors. Recent examples: `ship`'s class
precedence list includes SBCL's own `SLOT-OBJECT` (the ANSI-only answer was
wrong), `COLLIDE` has 13 methods where a `grep` suggested 12, `fbcon.c` lives
under `drivers/video/fbdev/core/` and not `drivers/video/console/`, and
`DRM_FBDEV_EMULATION` is defined in `drivers/gpu/drm/clients/Kconfig`.

Cheap checks worth running: `grep` the kernel headers for an ioctl constant's
value, `grep -n "^config FOO"` for a config symbol, `wc -l` for a line count,
`stat`/`ls` for a size, and a throwaway `sbcl --non-interactive --eval` for
anything about CLOS/the MOP.

**Check every `[[file:...]]` link resolves** — run **`./check-links.sh`** (add
`--quiet` for problems only; it exits non-zero if anything is broken). It
resolves each link relative to the file it lives in, because depth differs —
`../` from `doc/`, `../../` from `doc/background/`, the commonest way to get one
wrong — and it verifies the `::search` anchor is really present, so a renamed
function surfaces as a broken link instead of a silent one. Links into the
gitignored `./linux` and `./sbcl` trees are reported as SKIPPED when those
symlinks are absent. Run it before committing any doc change.

### Drift shapes to check first

The rule above has existed for a while and drift still accumulated, because these
failures are *quiet* — nothing breaks, a doc merely stops being true. The audit on
2026-08-09 found all of these; check them by name when touching a doc:

- **Facts that were true once.** `AGENTS.md` claimed "Networking is entirely OFF
  (`CONFIG_NET` unset)" — wrong since tag #21 — and `line-editing.org` ruled out a
  network REPL on the same dead premise.
- **Tag numbers diverging** across `AGENTS.md` / `kernel-config.org` /
  `sbcl-init.org`.
- **Roadmaps and phase lists** left on an old "Next:" after the work shipped.
  `doc/code-browser.org`'s phase list is the truth for browser status — but even
  it listed trail tabs and the Spotter as owed long after both landed.
- **Measured numbers that grow**: image/core/cpio sizes, line counts. State the
  tag they were measured at.
- **Paths drifting absolute.** A doc showed `/home/everton/...` where the real
  `initramfs.sbcl.list` uses a relative path — that breaks the repo's
  relocatable-for-anyone property, which is a stated goal in `README.org`.
- **Plans described as current** after the implementation went elsewhere
  (`code-browser.org` §6.1 had the X11 client living in `host-client/`; it landed
  in `poc/`).

### Explaining concepts

When a doc leans on a term the reader may not have (`ioctl`, character device,
major/minor, "a frozen heap", sysfs, endianness), explain it **where the reader
first meets it**, not in a glossary — and keep the surrounding didactic register.
Prefer a few well-placed sections over one-liners scattered about.

## Conventions

- Every `.org` file starts with `#+TITLE` / `#+STARTUP`.
- The docs live under **`doc/`** (and theory/tutorials under `doc/background/`);
  only `README.org` stays in the repo root. Cross-link with **relative** paths:
  within `doc/` it's `[[file:other.org]]`; from `doc/background/` up to a sibling
  doc it's `[[file:../other.org]]`; to code it's `[[file:../../initramfs/foo.lisp]]`.
- Kernel build tags are `#NN` (currently **#24**). Always say which tag a claim
  refers to.
- The external trees are **gitignored symlinks** (`./linux`, `./sbcl`); reference
  them via those names, never an absolute/`$HOME` path. `build.sh` derives its own
  location and the cpio source paths are relative — keep it that way.
- Don't rebuild the kernel (`build.sh --kernel`, minutes) unless a *kernel*
  option actually changed; userland-only changes are `build.sh` (seconds).

## Next things to do (roadmap)

Done: the **line editor** (`line-editor.lisp`/`repl.lisp`) and **networking**
(`net.lisp`/`dhcp.lisp`, the TCP REPL) — both shipped.

The active major effort is a **Smalltalk/Pharo-style code browser** for the Lisp
machine. Full plan: **`doc/code-browser.org`**; design mock-up:
`media/browser-views-design.html` (open locally). Pixel-native (framebuffer +
a pure-Lisp X11 client for host dev), built on a single `present` drill-down
protocol in a tiled, keyboard-first shell. Progress:

1. **Phase 0 — the definition registry** — *DONE* (`initramfs/registry.lisp`,
   wired into `build.sh` via `load-recording`). The running image shows its own
   source: `(show-source 'draw-alien)`, `(senders 'read-fb-geometry)`.
2. **Phase 1 — the CLOS/model layer** — *DONE* (`initramfs/model.lisp`): class DAG,
   GF/method graph, `applicable-methods`, `categorize`, object inspector. Headless.
3. **Phase 2/3 UI toolkit** — *DONE* in `poc/` (pure-Lisp X11 client, `/dev/fb0`
   backend, an editable syntax-colouring text view, and `poc/widgets.lisp` —
   `lol.widgets`: panel, bar, list (sections + selection + viewport + scrollbar),
   scrollbar, grid, card, plus the palette and row metrics). Host-runnable. A widget
   knows nothing about the browser: it takes geometry and `ROW`s and returns a hit
   map, so the same list draws a package, the Spotter and the debugger's restarts.
4. **Phase 3a — the Workspace** — *DONE* (`initramfs/present.lisp` +
   `poc/shell.lisp`, supervisor menu `p`): Do it / Print it / Inspect it /
   Debug it, with the printed transcript woven back into the buffer.
5. **Phase 4 — the `present` spine + the accordion shell** — *DONE*
   (`initramfs/present.lisp` + `poc/shell.lisp`): drill-down accordion, breadcrumbs,
   list/source/matrix views, and **live editing** (`C-c C-c` compiles a definition
   into the running image and updates the registry). Trail **tabs** and the
   **Spotter** (§4¾) have landed too — `poc/shell.lisp:draw-tabs`,
   `poc/shell.lisp:draw-spotter-card` (its drawing; only the modal input loop is in
   `initramfs/fb-browser.lisp`).
6. **Phase 4a — the Lisp-native views** — *DONE*. Generics: dispatch matrix,
   clickable cells, effective-method onion. Conditions: the *static* face
   (`subj-condition` in `present.lisp`, `condition-info` in `model.lisp`) — anatomy,
   **Signallers**, **Handlers** and nearby restarts, derived by reading each
   definition's captured source back out of the registry and matching forms. The
   analogue of Senders/Implementors for control flow, which CLOS cannot show.
   Honestly labelled a static approximation; the live debugger is the authority.
7. **Phase 5 — into the machine** — *DONE* (`initramfs/fb-browser.lisp`): the same
   shell full-screen on `/dev/fb0` as PID 1 (menu `b`), keyboard **and** evdev
   mouse via `poll(2)`, `M-.`/Ctrl-click jump-to-definition, and **scrolling**
   (PageUp/Down + the wheel) on both backends — evdev `REL_WHEEL` in the guest,
   buttons 4/5 under X11; `+wheel-lines+` lives in `poc/shell.lisp` because both
   need it. Verified in the guest over QMP against the browser's own state (ten
   assertions). The input path end to end: `doc/input.org`.
8. **Phase 6 — the live system** — *DONE*: the framebuffer debugger (restarts +
   `sb-di` backtrace + frame locals + `M-.` on a frame), and the **change set**
   (menu `c`). *File-out is deferred* to its own future session — it needs a
   storage-target decision first (the rootfs is tmpfs).
9. **The module index** (§4⅔) — *DONE, and now the browser's default root*
   (menu `b`): every definition grouped by the file it came from.
   `subj-modules` / `subj-module` in `present.lisp`, over `model.lisp`'s
   `categorize :file`, which keys on `directory/name` so the `initramfs/` and
   `poc/` trees cannot collide. It is keyed by *provenance*, so it shows the
   `lol.*` toolkit modules (`canvas`, `textview`, `fb`, `shell`) that a CL-USER
   root cannot list at all — which is why it replaced CL-USER as the default.
   The flat package view is still menu `l`.
10. **The design mock-up** (`media/browser-views-design.html`) has been walked
    section by section and the gaps that carry meaning are closed: the Spotter is
    banded with the matched characters lit, a match count and a footer; every tab
    has a close box; the dispatch matrix says what a cell does; the onion shows its
    `:around` return leg and its combine count; `M-,` opens Senders (the key
    `commands-for` had always advertised while the shell bound it elsewhere).
    One divergence is deliberate and recorded in §4¾: we kept a *contextual* status
    line where the mock-up shows none. Phase 7 (Morphic) is open horizon. See the
    Phases section of `doc/code-browser.org` — that doc is the truth here.
11. **Verifying a visual change:** `sbcl --script poc/render-frame.lisp SUBJECT`
    renders one real browser frame to a PPM headless in about a second — no QEMU,
    no X, no `/dev/fb0`. `-h` lists the subject forms; `-k` replays keys and clicks
    to reach a drilled state. Use it instead of booting for anything cosmetic; see
    `poc/README.org`, "Verifying a visual change".

## Known design facts (don't re-derive these)

- The bzImage **is** the EFI app; `\EFI\BOOT\BOOTX64.EFI` must be a literal copy of it.
- Separate initrd: `CONFIG_INITRAMFS_SOURCE=""` + `initrd=initramfs.cpio` on the cmdline.
- `CONFIG_PCI` is the keystone for real hardware: USB keyboard **and** ACPI power-off both depend on it.
- Real UEFI has no VGA text mode → need `FB_EFI` + `FRAMEBUFFER_CONSOLE`, or the screen is blank.
- PID 1 must never return, or the kernel panics.
- A saved SBCL image is a **frozen heap**: install libraries *before* `save-lisp-and-die`, not at runtime. No Quicklisp/`require` at runtime (no contrib fasls, no network).
- **Networking is ON** since tag #21: `CONFIG_NET=y` + `INET` + `virtio-net`, and
  the userland ships DHCP (`dhcp.lisp`) and a TCP REPL on :4005 (`net.lisp`).
  Older notes claiming `CONFIG_NET` is unset are stale — see `kernel-config.org`
  and `doc/networking.org`.
