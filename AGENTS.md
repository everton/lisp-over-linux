# AGENTS.md — working notes for AI agents in `~/lisp-over-linux`

This is a **learning project**: a very minimal EFISTUB Linux that a UEFI laptop
boots directly from a USB stick (no GRUB/bootloader), now running **SBCL Common
Lisp as PID 1**. The user is here to *understand* the system, so prefer
**didactic, well-commented** code and explanations over production hardening.

## What lives here

| File | Role |
|------|------|
| `README.org` | **Entry point.** What the project is + how to reconstruct the env (the `./linux`/`./sbcl` symlinks, `deps.sh`, build). Read first. |
| `micro-distro.org` | Base EFISTUB micro-linux: boot chain, USB creation, first-build diagnosis. |
| `sbcl-init.org` | SBCL as PID 1: preinit shim, supervisor, separate-initrd, build. Extends `micro-distro.org`. |
| `kernel-config.org` | **Inventory of every kernel option we enable and why.** Single source of truth for `.config`. |
| `framebuffer.org` | Deep-dive: framebuffer → efifb → fbcon, `/dev/fb0`, font size, drawing images. Explains the Display options. |
| `line-editing.org` | Deep-dive: readline-class input in pure Lisp (raw-mode termios via FFI, fbcon ANSI), linedit's features + native-dep caveats, hand-rolled vs library. |
| `networking.org` | **Roadmap (PLANNING ONLY):** bring a network layer — kernel `NET`/`INET`/`virtio-net` + userland `sb-bsd-sockets` baked into the image. Not started. |
| `build.sh` | One-step rebuild of userland (+ `--kernel`, `--run`). Location-independent; uses the `./linux`/`./sbcl` symlinks. |
| `deps.sh` | Fetch / link / update the external `./linux` (kernel) and `./sbcl` trees; create the gitignored symlinks. |
| `initramfs/preinit.c` | The C PID-1 shim: mounts /proc /sys /dev /tmp, then `execv`s the Lisp. |
| `initramfs/supervisor.lisp` | The Lisp supervisor (PID 1): REPL / worker / power-off menu. |
| `initramfs/initramfs.sbcl.list` | `gen_init_cpio` description of the rootfs. |

External source trees are reached via **gitignored symlinks** in the project root,
created by `deps.sh` (never hardcode a path):
- `./linux` → a Linux kernel tree (`.config`, `arch/x86/boot/bzImage`, `usr/gen_init_cpio`).
- `./sbcl` → a built SBCL tree (`src/runtime/sbcl`, `output/sbcl.core`).

On this machine they currently point at `~/linux/linux-6.18.3` and `~/sbcl`.

## Keep the docs in sync with reality — REQUIRED

The `.org` files are the deliverable, not an afterthought. When you change the
system, update the matching doc **in the same turn**:

- **Changed the kernel `.config`** (enabled/disabled any option) →
  update **`kernel-config.org`**: fix the relevant table, bump the kernel tag,
  and add a line to its "Change log". Re-run the audit `grep`s documented in
  that file's "How to regenerate / audit" section and reconcile differences.
  **Also refresh the tracked snapshot** in the same turn (run from the project
  root): `cp linux/.config kernel/config-6.18.3` (the live `.config` lives outside
  the repo behind the `./linux` symlink; `kernel/config-6.18.3` is our copy).
- **Changed the boot chain / USB / EFISTUB** → update `micro-distro.org`.
- **Changed `preinit.c`, `supervisor.lisp`, the initramfs list, or `build.sh`** →
  update `sbcl-init.org` (and its STATUS line).
- **Bumped the kernel build** → bump the tag (`#NN`) consistently across
  `kernel-config.org`, `sbcl-init.org`, and `micro-distro.org`.

If a doc and the code disagree, the **code/`.config` is truth** — fix the doc.

## Conventions

- Every `.org` file starts with the **resume-session comment block** (the
  `cd … && claude --resume <session-id>` header) and `#+TITLE` / `#+STARTUP`.
- Cross-link docs with `[[file:other.org][other.org]]`.
- Kernel build tags are `#NN` (currently **#20**). Always say which tag a claim
  refers to.
- The external trees are **gitignored symlinks** (`./linux`, `./sbcl`); reference
  them via those names, never an absolute/`$HOME` path. `build.sh` derives its own
  location and the cpio source paths are relative — keep it that way.
- Don't rebuild the kernel (`build.sh --kernel`, minutes) unless a *kernel*
  option actually changed; userland-only changes are `build.sh` (seconds).

## Next things to do (roadmap)

In order (agreed with the user). Neither is started:

1. **Built-in line editor** — hand-roll Path 2 from `line-editing.org` straight
   into `supervisor.lisp`: one `sb-alien` termios call (raw mode) + a `read-char`
   loop that parses arrow/Home/End escapes and redraws via ANSI. Pure userland,
   **no kernel rebuild**, fast iteration with `build.sh`. Small and self-contained.
2. **Networking** — the big, two-sided subsystem. Follow `networking.org`: kernel
   `NET`→`INET`→`virtio-net` (QEMU first) **and** userland `sb-bsd-sockets` baked
   into the frozen image + an IP (static ioctl first, DHCP later). Do a planning
   pass before touching `.config`; give it its own changelog/tag discipline.

## Known design facts (don't re-derive these)

- The bzImage **is** the EFI app; `\EFI\BOOT\BOOTX64.EFI` must be a literal copy of it.
- Separate initrd: `CONFIG_INITRAMFS_SOURCE=""` + `initrd=initramfs.cpio` on the cmdline.
- `CONFIG_PCI` is the keystone for real hardware: USB keyboard **and** ACPI power-off both depend on it.
- Real UEFI has no VGA text mode → need `FB_EFI` + `FRAMEBUFFER_CONSOLE`, or the screen is blank.
- PID 1 must never return, or the kernel panics.
- A saved SBCL image is a **frozen heap**: install libraries *before* `save-lisp-and-die`, not at runtime. No Quicklisp/`require` at runtime (no contrib fasls, no network).
- **Networking is entirely OFF** (`CONFIG_NET` unset) — see `kernel-config.org`.
