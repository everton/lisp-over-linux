# kernel/ — tracked snapshot of the kernel build config

The live kernel tree (`~/linux/linux-6.18.3/`) is **outside** this repo: it is
unmodified upstream Linux 6.18.3 source (re-downloadable) plus one precious file,
the `.config`. This directory keeps a tracked **copy** of that `.config` so the
kernel side of the project can't be lost with the rest of `micro/`.

- `config-6.18.3` — verbatim snapshot of `~/linux/linux-6.18.3/.config`.

**This is a snapshot, not the live file.** The live `.config` is still the truth
(see [`../kernel-config.org`](../kernel-config.org)). After any kernel config
change, refresh the snapshot in the same commit:

```sh
cp ~/linux/linux-6.18.3/.config ~/linux/micro/kernel/config-6.18.3
```

To rebuild the kernel from scratch on a fresh machine: fetch linux-6.18.3, drop
this file in as `.config`, `make olddefconfig`, then `make -j$(nproc) bzImage`.
