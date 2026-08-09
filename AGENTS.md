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
| `doc/networking.org` | Networking: kernel `NET`/`INET`/`virtio-net` + userland `sb-bsd-sockets`, static IP/DHCP, the TCP REPL. Wired path working; DNS/Wi-Fi ahead. |
| `doc/code-browser.org` | **The active major effort.** Full design + phase tracker for the Smalltalk/Pharo-style code browser: the `present` drill-down protocol, the accordion shell, trails/tabs/Spotter, the debugger. The longest doc here and the best maintained — treat its phase list as truth. |
| `doc/fonts.org` | How our pure-Lisp renderer turns a character into pixels (PSF/`.lolf` bitmap → `draw-string`), and how to change the font or its size. |
| `doc/agent.org` | **PLAN.** An LLM client written in Lisp and living in the image: HTTP/JSON/SSE over the existing sockets, `registry.lisp`/`model.lisp`/`present.lisp` as its tool surface, doorways from the REPL / a browser tab / the debugger, and TLS 1.3 from scratch as a late sub-project. Also records why we are *not* running the `claude` CLI in the guest. |
| `doc/background/background.org` | General firmware/CPU/hardware theory (UEFI handoff, x86 modes, USB-HID, multicore). "Not project rationale." |
| `doc/background/learn-networking.org` | From-scratch networking tutorial with a progress tracker + deep links into the code. |
| `build.sh` | One-step rebuild of userland (+ `--kernel`, `--run`). Location-independent; uses the `./linux`/`./sbcl` symlinks. |
| `deps.sh` | Fetch / link / update the external `./linux` (kernel) and `./sbcl` trees; create the gitignored symlinks. |
| `initramfs/preinit.c` | The C PID-1 shim: mounts /proc /sys /dev /tmp, then `execv`s the Lisp. |
| `initramfs/supervisor.lisp` | The Lisp supervisor (PID 1): REPL / worker / power-off menu. |
| `initramfs/registry.lisp` | The definition registry: `load-recording` captures every module's source INTO the image, so the running system can show its own code (`source-of`, `show-source`, `senders`). Phase 0 of the code browser — see `doc/code-browser.org`. |
| `initramfs/model.lisp` | The code browser's MODEL layer (Phase 1): CLOS introspection via `sb-mop` — class DAG (`show-class`), GF/method graph (`show-gf`, `applicable-methods`), `categorize`, object inspector (`show-inspect`). Headless/REPL-driven. |
| `initramfs/present.lisp` | The drill-down `present`/`commands-for` protocol + affordance model (code browser §4½). `(present subject)`→a view of drillable items; `(commands-for subject)`→declarative commands (menu/halo/palette/key). Headless; `(show-view subject)` demos it. |
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
| `initramfs/initramfs.sbcl.list` | `gen_init_cpio` description of the rootfs. |
| `poc/` | Prototype for the code browser's pixel UI (pure-Lisp X11 client + framebuffer, a text view). Host-runnable; see `poc/README.org` and `doc/code-browser.org`. |
| `host-client/` | **Host-side** tools (NOT shipped in the image, never run in the guest): the network-REPL raw-forwarding client. The deliberate opposite of `initramfs/`. |

External source trees are reached via **gitignored symlinks** in the project root,
created by `deps.sh` (never hardcode a path):
- `./linux` → a Linux kernel tree (`.config`, `arch/x86/boot/bzImage`, `usr/gen_init_cpio`).
- `./sbcl` → a built SBCL tree (`src/runtime/sbcl`, `output/sbcl.core`).

On this machine they currently point at `./sources/linux-7.1.3` and `./sources/sbcl`
(both source trees live under the project's gitignored `sources/` dir).

## Keep the docs in sync with reality — REQUIRED

The `.org` files are the deliverable, not an afterthought. When you change the
system, update the matching doc **in the same turn**:

- **Changed the kernel `.config`** (enabled/disabled any option) →
  update **`doc/kernel-config.org`**: fix the relevant table, bump the kernel tag,
  and add a line to its "Change log". Re-run the audit `grep`s documented in
  that file's "How to regenerate / audit" section and reconcile differences.
  **Also refresh the tracked snapshot** in the same turn (run from the project
  root): `cp linux/.config kernel/config-7.1.3` (the live `.config` lives outside
  the repo behind the `./linux` symlink; `kernel/config-7.1.3` is our copy).
- **Changed the boot chain / USB / EFISTUB** → update `doc/micro-distro.org`.
- **Changed `preinit.c`, the Lisp modules, the initramfs list, or `build.sh`** →
  update `doc/sbcl-init.org` (and its STATUS line).
- **Bumped the kernel build** → bump the tag (`#NN`) consistently across
  `doc/kernel-config.org`, `doc/sbcl-init.org`, and `doc/micro-distro.org`.

If a doc and the code disagree, the **code/`.config` is truth** — fix the doc.

## Conventions

- Every `.org` file starts with `#+TITLE` / `#+STARTUP`.
- The docs live under **`doc/`** (and theory/tutorials under `doc/background/`);
  only `README.org` stays in the repo root. Cross-link with **relative** paths:
  within `doc/` it's `[[file:other.org]]`; from `doc/background/` up to a sibling
  doc it's `[[file:../other.org]]`; to code it's `[[file:../../initramfs/foo.lisp]]`.
- Kernel build tags are `#NN` (currently **#23**). Always say which tag a claim
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
   backend, an editable syntax-colouring text view). Host-runnable. Still owed
   from phase 3: generic panes/lists/scrollbars/menus as reusable widgets.
4. **Phase 3a — the Workspace** — *DONE* (`initramfs/present.lisp` +
   `poc/shell.lisp`, supervisor menu `p`): Do it / Print it / Inspect it /
   Debug it, with the printed transcript woven back into the buffer.
5. **Phase 4 — the `present` spine + the accordion shell** — *DONE*
   (`initramfs/present.lisp` + `poc/shell.lisp`): drill-down accordion, breadcrumbs,
   list/source/matrix views, and **live editing** (`C-c C-c` compiles a definition
   into the running image and updates the registry). Trail **tabs** and the
   **Spotter** (§4¾) have landed too — `poc/shell.lisp:draw-tabs`,
   `initramfs/fb-browser.lisp:draw-spotter`.
6. **Phase 4a — the Lisp-native views** — *generics DONE* (dispatch matrix,
   clickable cells, effective-method onion). Still owed: the condition subject's
   *static* face as a browsable view (the live debugger already exists).
7. **Phase 5 — into the machine** — *DONE* (`initramfs/fb-browser.lisp`): the same
   shell full-screen on `/dev/fb0` as PID 1 (menu `b`), keyboard **and** evdev
   mouse via `poll(2)`, `M-.`/Ctrl-click jump-to-definition. Still owed:
   PageUp/Down + wheel scroll.
8. **Phase 6 — the live system** — *DONE*: the framebuffer debugger (restarts +
   `sb-di` backtrace + frame locals + `M-.` on a frame), and the **change set**
   (menu `c`). *File-out is deferred* to its own future session — it needs a
   storage-target decision first (the rootfs is tmpfs).
9. **Next:** phase 3's remaining widgets, phase 4a's condition view, and the
   phase 5 scrolling gaps. Phase 7 (Morphic) is open horizon.
   See the Phases section of `doc/code-browser.org` — that doc is the truth here.

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
