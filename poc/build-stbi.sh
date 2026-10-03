#!/usr/bin/env bash
#
# build-stbi.sh — compile poc/stbi-shim.c (+ the vendored stb_image.h) into
# libstbi.so, the one shared object behind JPEG/PSD/HDR/PIC previews.
#
# Usage:  poc/build-stbi.sh [OUTDIR]     (default: poc/out)
#
# The result links against libc and libm only — the two libraries the SBCL
# runtime already pulls into the initramfs — and the script checks that with
# `ldd`, failing loudly if anything else sneaks in. poc/image.lisp loads it
# lazily (first JPEG), from /usr/lib/lol/libstbi.so in the guest, or from
# poc/out/ (or $LOL_STBI) on the host. See doc/previews.org.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
out="${1:-$here/out}"
mkdir -p "$out"
gcc -O2 -shared -fPIC -fvisibility=hidden -Wall -Wno-unused-function \
    -o "$out/libstbi.so" "$here/stbi-shim.c" -lm
strip --strip-unneeded "$out/libstbi.so"
# Only libc, libm, the vdso and the loader are allowed.
extra="$(ldd "$out/libstbi.so" | grep -vE 'linux-vdso|libc\.so|libm\.so|ld-linux' || true)"
if [ -n "$extra" ]; then
    echo "build-stbi.sh: unexpected dependency:" >&2; echo "$extra" >&2; exit 1
fi
echo "$out/libstbi.so ($(stat -c %s "$out/libstbi.so") bytes)"
