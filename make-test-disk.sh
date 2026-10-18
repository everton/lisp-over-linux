#!/usr/bin/env bash
#
# make-test-disk.sh — build test-disk.img, a QEMU stand-in for the laptop's disk.
#
# The image boots, mounts "the disk whose ext4 UUID is in /etc/lol.conf" and
# becomes the user named there (doc/storage.org). Under QEMU we must never hand
# the guest the REAL NVMe (the host has it mounted read-write — two kernels
# writing one ext4 is how filesystems die). So this script makes a small ext4
# image that LOOKS like the laptop's root to preinit:
#
#   - the same filesystem UUID as the host's / (so lol.conf matches it),
#   - an /etc/passwd + /etc/group naming the build user with the host's uid/gid,
#   - /home/<user> with a few things worth browsing: text, Lisp source, images,
#     a symlink (and a broken one), dotfiles, nested folders.
#   - the ./sbcl tree's .lisp sources at their real path, so a builtin's source
#     (initramfs/sbcl-source.lisp) reads from the disk as on the laptop.
#
# No root needed: `mke2fs -d DIR` populates the new filesystem from a directory,
# copying each file's owner from the source — which is us, uid 1000 — so the
# files in the image are owned by the user exactly as on the real disk.
#
#   ./make-test-disk.sh            (re)create test-disk.img (256 MB, sparse)
#   SIZE=1G ./make-test-disk.sh    a bigger one
#
# build.sh --run / --interactive attach test-disk.img automatically when present.

set -euo pipefail
MICRO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT="${1:-$MICRO/test-disk.img}"
SIZE="${SIZE:-256M}"
USER_NAME="$(id -un)"; USER_UID="$(id -u)"; USER_GID="$(id -g)"
UUID="$(findmnt -no UUID -T "$HOME")"
MKE2FS="$(command -v mke2fs || echo /usr/sbin/mke2fs)"

STAGE="$(mktemp -d)"
trap 'rm -rf "$STAGE"' EXIT

mkdir -p "$STAGE/etc" "$STAGE/home/$USER_NAME"
printf 'root:x:0:0:root:/root:/bin/sh\n%s:x:%s:%s:%s:/home/%s:/bin/sh\n' \
  "$USER_NAME" "$USER_UID" "$USER_GID" "$USER_NAME" "$USER_NAME" > "$STAGE/etc/passwd"
printf 'root:x:0:\n%s:x:%s:\nvideo:x:44:%s\n' "$USER_NAME" "$USER_GID" "$USER_NAME" > "$STAGE/etc/group"

H="$STAGE/home/$USER_NAME"
mkdir -p "$H/Documents/notes" "$H/Pictures" "$H/src" "$H/.config/lol"
printf 'Hello from the test disk.\nThis file lives on ext4, owned by %s.\n' "$USER_NAME" > "$H/README.txt"
printf '# a note\n- one\n- two\n' > "$H/Documents/notes/todo.org"
cp "$MICRO/initramfs/examples.lisp" "$H/src/examples.lisp"
# at the PRE-2026-10-17 place on purpose: every boot with this disk moves it into
# ~/.lol (initramfs/lol-home.lisp), and check-files.lisp asserts the move
printf 'secret=1\n' > "$H/.config/lol/settings"
printf 'hidden\n' > "$H/.hidden-file"
ln -s Documents/notes "$H/notes"                 # a symlink to a directory
ln -s does-not-exist "$H/broken-link"           # a dangling one
# Pictures: whatever ImageMagick can make, if it is installed (previews).
if command -v convert >/dev/null; then
  convert -size 320x200 gradient:'#1e3c72-#2a5298' "$H/Pictures/gradient.png"
  convert -size 320x200 plasma:fractal -quality 85 "$H/Pictures/plasma.jpg"
  convert -size 320x200 plasma:fractal -interlace JPEG "$H/Pictures/progressive.jpg"
  convert -size 64x64 xc:orange "$H/Pictures/orange.gif"
fi
cp -L "$MICRO/media/browser.png" "$H/Pictures/browser.png" 2>/dev/null || true
# SBCL's own sources, at the SAME absolute path as on the laptop: the image bakes
# the ./sbcl tree's real path (initramfs/sbcl-source.lisp) and reads a builtin's
# source from it on demand. Only the .lisp files of src/ and contrib/ (~25 MB),
# and only when that tree lives under $HOME — anywhere else the real /home would
# not have it either, and the guest says "not on this disk".
SBCL_REAL="$(readlink -f "$MICRO/sbcl" || true)"
case "$SBCL_REAL" in
  "$HOME"/*)
    ( cd "$SBCL_REAL" && find src contrib -name '*.lisp' -print0 |
        cpio -0pdm --quiet "$STAGE$SBCL_REAL" ) ;;
esac
tar -C "$H" -czf "$H/Documents/backup.tar.gz" README.txt src
gzip -c "$H/README.txt" > "$H/Documents/README.txt.gz"

rm -f "$OUT"
truncate -s "$SIZE" "$OUT"
"$MKE2FS" -q -t ext4 -U "$UUID" -L lol-test -d "$STAGE" -E root_owner=0:0 "$OUT"
echo "test disk: $OUT ($SIZE, ext4 UUID=$UUID, user $USER_NAME uid $USER_UID)"
