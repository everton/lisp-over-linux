#!/usr/bin/env bash
#
# deps.sh — materialize the external source trees this project builds against.
#
# The repo deliberately does NOT contain the Linux kernel source or the SBCL
# source (huge, version-specific, per-machine). build.sh instead references two
# *gitignored symlinks* in the project root:
#
#     ./linux  ->  a Linux kernel source tree
#     ./sbcl   ->  an SBCL source tree
#
# This script creates/updates those symlinks. Two ways to use it:
#
#   Already have the trees somewhere:
#     ./deps.sh link linux  /path/to/linux-6.18.3
#     ./deps.sh link sbcl   /path/to/sbcl
#
#   Start from nothing (downloads into ./sources/, then symlinks):
#     ./deps.sh kernel [VERSION]   # default VERSION below; applies our pinned .config
#     ./deps.sh sbcl               # clones SBCL (build it yourself — see README.org)
#
#   ./deps.sh status               # show where the symlinks currently point
#
# After this, build with ./build.sh (userland) or ./build.sh --kernel.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC="${LOL_SRC:-$HERE/sources}"     # where downloaded trees live (gitignored)
KVER_DEFAULT="6.18.3"               # kept in step with kernel/config-<ver>

say()  { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }
die()  { echo "ERROR: $*" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

# pick whatever downloader is present
fetch() { # fetch <url> <out>
  if   have curl; then curl -L --fail -o "$2" "$1"
  elif have wget; then wget -O "$2" "$1"
  else die "need curl or wget to download"; fi
}

cmd_link() { # link <linux|sbcl> <path>
  local name="${1:-}" target="${2:-}"
  case "$name" in linux|sbcl) ;; *) die "link: name must be 'linux' or 'sbcl'";; esac
  [ -n "$target" ] || die "link: usage: ./deps.sh link $name /path/to/tree"
  [ -d "$target" ] || die "link: not a directory: $target"
  ln -sfn "$(cd "$target" && pwd)" "$HERE/$name"
  say "./$name -> $(readlink "$HERE/$name")"
}

cmd_kernel() { # kernel [version]
  local ver="${1:-$KVER_DEFAULT}"
  local tarball="linux-$ver.tar.xz"
  local url="https://cdn.kernel.org/pub/linux/kernel/v${ver%%.*}.x/$tarball"
  mkdir -p "$SRC"
  [ -f "$SRC/$tarball" ]      || { say "Downloading $tarball"; fetch "$url" "$SRC/$tarball"; }
  [ -d "$SRC/linux-$ver" ]    || { say "Extracting $tarball";  tar -C "$SRC" -xf "$SRC/$tarball"; }
  ln -sfn "$SRC/linux-$ver" "$HERE/linux"
  say "./linux -> $(readlink "$HERE/linux")"
  # seed it with our pinned config if we have one for this version
  local cfg="$HERE/kernel/config-$ver"
  if [ -f "$cfg" ]; then
    say "Applying pinned config kernel/config-$ver"
    cp "$cfg" "$HERE/linux/.config"
    make -C "$HERE/linux" olddefconfig
  else
    echo "  (no kernel/config-$ver snapshot — configure the kernel yourself,"
    echo "   then: cp linux/.config kernel/config-$ver)"
  fi
  echo "  next: ./build.sh --kernel"
}

cmd_sbcl() { # sbcl  (clone or update; build is manual — see README.org)
  mkdir -p "$SRC"
  if [ -d "$SRC/sbcl/.git" ]; then
    say "Updating SBCL (git pull)"; git -C "$SRC/sbcl" pull --ff-only
  else
    have git || die "need git to clone SBCL"
    say "Cloning SBCL"; git clone https://github.com/sbcl/sbcl.git "$SRC/sbcl"
  fi
  ln -sfn "$SRC/sbcl" "$HERE/sbcl"
  say "./sbcl -> $(readlink "$HERE/sbcl")"
  echo "  SBCL must be BUILT before build.sh can use it:"
  echo "    cd sbcl && sh make.sh        # needs an existing lisp (sbcl/ccl) to bootstrap"
  echo "  build.sh expects: sbcl/src/runtime/sbcl and sbcl/output/sbcl.core"
}

cmd_status() {
  for name in linux sbcl; do
    if [ -L "$HERE/$name" ]; then
      printf '  ./%-6s -> %s%s\n' "$name" "$(readlink "$HERE/$name")" \
        "$([ -e "$HERE/$name" ] || echo '   (BROKEN: target missing)')"
    else
      printf '  ./%-6s   (not linked — run ./deps.sh)\n' "$name"
    fi
  done
}

case "${1:-}" in
  link)   shift; cmd_link "$@" ;;
  kernel) shift; cmd_kernel "$@" ;;
  sbcl)   shift; cmd_sbcl "$@" ;;
  status) cmd_status ;;
  ""|-h|--help)
    sed -n '3,28p' "$0" | sed 's/^# \{0,1\}//' ;;
  *) die "unknown command: $1  (try: link | kernel | sbcl | status)";;
esac
