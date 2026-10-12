/* lol-ct: run one command inside a container whose root is the laptop's own Debian.
 *
 *   lol-ct [-n NAME] [-s STATEDIR] [-r HOSTROOT] [-C DIR] [-e VAR=VALUE]... [-N] [-v]
 *          [--] PROGRAM [ARG...]
 *   lol-ct --probe-root [-r HOSTROOT]      (the doc/containers.org §3.3 question)
 *
 * The design is doc/containers.org; this is §5.1 of it, built. In one sentence:
 * make /mnt/host look like / to one process tree and to nobody else, with
 * writable overlays on top, WITHOUT any privilege the user does not already have.
 *
 * Why C, and why a separate program? Two reasons, both checked:
 *   - unshare(CLONE_NEWUSER) refuses a caller with more than one thread
 *     (linux/kernel/fork.c, check_unshare_flags), and our Lisp PID 1 has several
 *     (the net REPL is an sb-thread). The namespace work needs a fresh process.
 *   - It is the same shape as preinit.c: a short sequence of mount(2) calls with
 *     an errno check after each. C says that most directly.
 *
 * The four namespaces we create, and the one we deliberately share:
 *   USER   our own uid table. We map our uid to ITSELF (1000 -> 1000), so inside
 *          you are still you — files you create in /home are yours. Creating it
 *          also hands us a full set of capabilities, valid ONLY over things this
 *          namespace owns: that is what lets an ordinary user mount below.
 *   MOUNT  our own mount table: the overlays, binds and the new root are invisible
 *          to the Lisp and to every other process.
 *   PID    our own process numbering: the first process in it is PID 1 of the
 *          container, and when it exits the kernel kills everything else in it.
 *   UTS    our own hostname ("lol-default"), so a shell prompt says where you are.
 *   (NET is NOT created: the container shares our network, so `rails s` on :3000
 *   is reachable from the Lisp and the resolver works the same inside and out.)
 *
 * The process tree:
 *
 *     the Lisp (sb-ext:run-program)
 *       └─ lol-ct          OUTER: unshares user+mount+uts, writes the id maps,
 *            │                   unshares pid, forks; forwards signals; relays
 *            │                   the exit status. Lives in the parent pid ns.
 *            └─ lol-ct     INIT:  PID 1 of the new pid ns. Builds the root, pivots,
 *                 │               forks the command, reaps orphans, forwards
 *                 │               signals, exits with the command's status.
 *                 └─ PROGRAM      PID 2: execve'd as you, with no capabilities.
 *
 * Exit status: the command's own (128+N if signal N killed it), 125 when lol-ct
 * itself could not build the container, 127 when the program was not found —
 * the conventions of env(1), docker and the shell.
 *
 * Static, musl, no libraries — like preinit, so it runs on our initramfs.
 * All original code, under this project's MIT licence.
 */
#define _GNU_SOURCE
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pwd.h>
#include <sched.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define EXIT_SETUP    125   /* lol-ct itself failed */
#define EXIT_NOTFOUND 127   /* the program could not be executed */

/* ---- options ------------------------------------------------------------ */

static const char *opt_name = "default";
static char opt_state[PATH_MAX] = "";      /* empty = $HOME/.lol/ct/<name> */
static const char *opt_hostroot = "/mnt/host";
static const char *opt_cwd = NULL;         /* NULL = $HOME */
static const char *opt_env[64];
static int n_opt_env = 0;
static int opt_verbose = 0;
static int opt_no_stdin = 0;               /* -N: stdin from /dev/null */
static int opt_probe_root = 0;

/* ---- saying things ------------------------------------------------------ */
/* Errors always go to stderr, which the Lisp shows in the run pane. With -v each
 * layout step is narrated too. Every step is ALSO written to <state>/layout, so the
 * container's view in the environment can say what this run actually got (an
 * overlay, or the read-only fallback) without anyone passing -v. */

static FILE *layout_f = NULL;

static void note(const char *fmt, ...) {
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof buf, fmt, ap);
    va_end(ap);
    if (layout_f) fprintf(layout_f, "%s\n", buf);
    if (opt_verbose) fprintf(stderr, "lol-ct: %s\n", buf);
}

static void die(const char *fmt, ...) {
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof buf, fmt, ap);
    va_end(ap);
    fprintf(stderr, "lol-ct: %s\n", buf);
    if (layout_f) { fprintf(layout_f, "FAILED: %s\n", buf); fclose(layout_f); }
    _exit(EXIT_SETUP);
}

/* ---- small file helpers ------------------------------------------------- */

static int write_file(const char *path, const char *text) {
    int fd = open(path, O_WRONLY | O_CLOEXEC);
    if (fd < 0) return -1;
    ssize_t n = write(fd, text, strlen(text));
    close(fd);
    return n == (ssize_t)strlen(text) ? 0 : -1;
}

/* mkdir -p, as the current user. Returns 0 or -1 (errno set). */
static int mkdirs(const char *path, mode_t mode) {
    char tmp[PATH_MAX];
    snprintf(tmp, sizeof tmp, "%s", path);
    for (char *p = tmp + 1; *p; p++)
        if (*p == '/') {
            *p = 0;
            if (mkdir(tmp, mode) != 0 && errno != EEXIST) return -1;
            *p = '/';
        }
    if (mkdir(tmp, mode) != 0 && errno != EEXIST) return -1;
    return 0;
}

static int is_dir(const char *p)  { struct stat st; return stat(p, &st) == 0 && S_ISDIR(st.st_mode); }
static int exists(const char *p)  { struct stat st; return lstat(p, &st) == 0; }

/* ---- step 1 (OUTER): the user namespace and its id maps ----------------- */
/*
 * A fresh user namespace maps NO ids at all; until we write the maps, every file
 * looks owned by "nobody" and we could not even create one. A process may write
 * its own maps, but an unprivileged one only ONE line: its own id, onto anything.
 *   uid_map "1000 1000 1"   inside uid 1000 = outside uid 1000, a range of one.
 * The gid map has one extra rule: /proc/self/setgroups must say "deny" first, or
 * we could use setgroups(2) to DROP a group and gain access a group-deny ACL had
 * refused us (CVE-2014-8989 is why the kernel insists).
 */
static void enter_user_namespace(uid_t uid, gid_t gid) {
    if (unshare(CLONE_NEWUSER | CLONE_NEWNS | CLONE_NEWUTS) != 0)
        die("unshare(user+mount+uts) failed: %s%s", strerror(errno),
            errno == EINVAL ? " (is the caller multithreaded?)" :
            errno == EPERM  ? " (CONFIG_USER_NS off, or max_user_namespaces is 0?)" : "");
    char map[64];
    if (write_file("/proc/self/setgroups", "deny") != 0)
        die("writing /proc/self/setgroups: %s", strerror(errno));
    snprintf(map, sizeof map, "%u %u 1\n", (unsigned)uid, (unsigned)uid);
    if (write_file("/proc/self/uid_map", map) != 0)
        die("writing uid_map: %s", strerror(errno));
    snprintf(map, sizeof map, "%u %u 1\n", (unsigned)gid, (unsigned)gid);
    if (write_file("/proc/self/gid_map", map) != 0)
        die("writing gid_map: %s", strerror(errno));
}

/* ---- step 2 (INIT): the layered root ------------------------------------ */
/*
 * Overlayfs shows a read-only LOWER directory (the host's /usr) merged with a
 * writable UPPER one; the first write to a lower file copies it up, and the lower
 * is never touched. WORK is overlay's scratch space, on the upper's filesystem.
 *
 * A container's uppers live in <state>/<dir>/upper, on /home, so what you install
 * into /usr/local or write into /etc persists across runs and boots; deleting the
 * state directory resets the container.
 *
 * Tried in order, and the first that mounts wins (the outcome goes to the layout
 * file, so a fallback is never silent):
 *   1. overlay, upper in the state dir                 — the design
 *   2. the same with "userxattr"                       — overlay keeps its own
 *      bookkeeping in xattrs; trusted.* ones need root in the INITIAL namespace,
 *      user.* ones do not
 *   3. overlay, upper in a tmpfs (changes vanish)      — when /home cannot be an
 *      upper (QEMU's 9p test root puts /home itself on an overlay)
 *   4. a plain read-only bind of the host directory    — when the LOWER refuses
 *      (a filesystem overlay cannot stack on)
 */
static char R[PATH_MAX];       /* the new root, while we build it: <state>/root */

static int try_overlay(const char *lower, const char *upper, const char *work,
                       const char *target, int userxattr) {
    char opts[PATH_MAX * 3 + 32];
    if (mkdirs(upper, 0755) != 0 || mkdirs(work, 0755) != 0) return -1;
    snprintf(opts, sizeof opts, "lowerdir=%s,upperdir=%s,workdir=%s%s",
             lower, upper, work, userxattr ? ",userxattr" : "");
    return mount("overlay", target, "overlay", 0, opts);
}

/* Remount a bind read-only. In a user namespace the flags the parent namespace set
 * (nosuid, nodev, noexec) are LOCKED: a remount that omits one is refused with
 * EPERM, because dropping nosuid would be an escalation. So read them back from
 * statvfs and keep them. */
static int remount_ro(const char *target) {
    struct statvfs sv;
    unsigned long keep = 0;
    if (statvfs(target, &sv) == 0) {
        if (sv.f_flag & ST_NOSUID) keep |= MS_NOSUID;
        if (sv.f_flag & ST_NODEV)  keep |= MS_NODEV;
        if (sv.f_flag & ST_NOEXEC) keep |= MS_NOEXEC;
    }
    return mount(NULL, target, NULL, MS_REMOUNT | MS_BIND | MS_RDONLY | keep, NULL);
}

static void layer_dir(const char *state, const char *d) {
    char lower[PATH_MAX], upper[PATH_MAX], work[PATH_MAX], target[PATH_MAX];
    snprintf(lower, sizeof lower, "%s/%s", opt_hostroot, d);
    snprintf(target, sizeof target, "%s/%s", R, d);
    if (!is_dir(lower)) { note("/%s: not in the host root, skipped", d); return; }
    if (mkdir(target, 0755) != 0 && errno != EEXIST) die("mkdir %s: %s", target, strerror(errno));

    snprintf(upper, sizeof upper, "%s/%s/upper", state, d);
    snprintf(work, sizeof work, "%s/%s/work", state, d);
    if (try_overlay(lower, upper, work, target, 0) == 0) {
        note("/%s: overlay, upper %s", d, upper); return;
    }
    int e1 = errno;
    if (try_overlay(lower, upper, work, target, 1) == 0) {
        note("/%s: overlay (userxattr), upper %s [plain overlay: %s]", d, upper, strerror(e1)); return;
    }
    int e2 = errno;
    /* The scratch tmpfs is mounted on <R>/.scratch (build_root does it), so it ends
     * up as /.scratch inside: ephemeral, gone with the container. */
    snprintf(upper, sizeof upper, "%s/.scratch/%s/upper", R, d);
    snprintf(work, sizeof work, "%s/.scratch/%s/work", R, d);
    if (try_overlay(lower, upper, work, target, 0) == 0) {
        note("/%s: overlay, upper in tmpfs (changes VANISH) [state upper: %s / %s]",
             d, strerror(e1), strerror(e2)); return;
    }
    int e3 = errno;
    if (mount(lower, target, NULL, MS_BIND | MS_REC, NULL) == 0) {
        remount_ro(target);
        note("/%s: read-only bind of %s (overlay refused: %s; userxattr: %s; tmpfs upper: %s)",
             d, lower, strerror(e1), strerror(e2), strerror(e3));
        return;
    }
    die("/%s: neither an overlay (%s) nor a bind (%s) of %s", d, strerror(e3), strerror(errno), lower);
}

/* Bind SRC (a path in OUR namespace) onto <R>/DST. Recursive when asked: /dev must
 * bring /dev/pts and /dev/shm along, and in a user namespace a mount's children
 * are LOCKED to it — a non-recursive bind that would uncover them is refused. */
static void bind_in(const char *src, const char *dst, int rec, int ro) {
    char target[PATH_MAX];
    snprintf(target, sizeof target, "%s%s", R, dst);
    if (mkdir(target, 0755) != 0 && errno != EEXIST) die("mkdir %s: %s", target, strerror(errno));
    if (mount(src, target, NULL, MS_BIND | (rec ? MS_REC : 0), NULL) != 0) {
        /* non-recursive refused (locked children): fall back to recursive */
        if (!rec && errno == EINVAL && mount(src, target, NULL, MS_BIND | MS_REC, NULL) == 0)
            note("%s: recursive bind of %s (a plain one was refused)", dst, src);
        else
            die("bind %s -> %s: %s", src, dst, strerror(errno));
    } else
        note("%s: bind of %s%s", dst, src, rec ? " (recursive)" : "");
    if (ro && remount_ro(target) != 0) note("%s: read-only remount failed: %s", dst, strerror(errno));
}

/* Put OUR copy of a small /etc file over the container's. The networking side of
 * the environment writes /etc/resolv.conf in our root at boot, and a program in
 * the container should use the same resolver we do. If ours does not exist, the
 * host's (already in the /etc overlay) stays.
 *
 * Debian often makes /etc/resolv.conf a SYMLINK (to ../run/systemd/resolve/…).
 * mount(2) follows symlinks, and that target lives in our fresh /run where it does
 * not exist — so we follow the link ourselves, inside R, and create the target. */
static void etc_file(const char *name) {
    char ours[PATH_MAX], target[PATH_MAX], link[PATH_MAX];
    snprintf(ours, sizeof ours, "/etc/%s", name);
    if (!exists(ours)) { note("/etc/%s: none of ours, the host's stays", name); return; }
    snprintf(target, sizeof target, "%s/etc/%s", R, name);
    for (int hops = 0; hops < 8; hops++) {
        ssize_t n = readlink(target, link, sizeof link - 1);
        if (n < 0) break;
        link[n] = 0;
        char next[PATH_MAX];
        if (link[0] == '/') snprintf(next, sizeof next, "%s%s", R, link);
        else {
            char *slash = strrchr(target, '/');
            *slash = 0;
            snprintf(next, sizeof next, "%s/%s", target, link);
        }
        snprintf(target, sizeof target, "%s", next);
    }
    if (!exists(target)) {
        char dir[PATH_MAX];
        snprintf(dir, sizeof dir, "%s", target);
        *strrchr(dir, '/') = 0;
        int fd = (mkdirs(dir, 0755) == 0) ? open(target, O_CREAT | O_WRONLY | O_CLOEXEC, 0644) : -1;
        if (fd < 0) { note("/etc/%s: cannot create %s (%s), the host's stays", name, target, strerror(errno)); return; }
        close(fd);
    }
    if (mount(ours, target, NULL, MS_BIND, NULL) == 0) {
        remount_ro(target);
        note("/etc/%s: ours, bound over %s", name, target + strlen(R));
    } else
        note("/etc/%s: bind failed (%s), the host's stays", name, strerror(errno));
}

static void build_root(const char *state) {
    snprintf(R, sizeof R, "%s/root", state);
    if (mkdirs(R, 0755) != 0) die("mkdir %s: %s", R, strerror(errno));

    /* Nothing we mount may leak back to the Lisp's namespace. A fresh mount
     * namespace starts as a COPY of ours, and mounts marked "shared" would
     * propagate; marking the whole tree private cuts that. */
    if (mount(NULL, "/", NULL, MS_REC | MS_PRIVATE, NULL) != 0)
        die("making / private: %s", strerror(errno));

    /* The new root is a fresh tmpfs: only what we put in it exists. No host /boot,
     * no /root, no lost+found — and the host's /home is never reached through it. */
    if (mount("tmpfs", R, "tmpfs", MS_NOSUID | MS_NODEV, "mode=0755,size=16m") != 0)
        die("tmpfs on %s: %s", R, strerror(errno));
    char p[PATH_MAX];
    snprintf(p, sizeof p, "%s/.scratch", R);
    mkdir(p, 0755);
    mount("tmpfs", p, "tmpfs", MS_NOSUID | MS_NODEV, "mode=0755");  /* only used by fallback 3 */

    /* Debian is "merged-/usr": /bin, /lib, /lib64, /sbin are symlinks into usr/.
     * Copy every top-level symlink that points into usr, so the layout follows the
     * host's own (lib32, libx32 …) instead of a list written here. */
    DIR *d = opendir(opt_hostroot);
    struct dirent *e;
    while (d && (e = readdir(d))) {
        char src[PATH_MAX], link[PATH_MAX];
        snprintf(src, sizeof src, "%s/%s", opt_hostroot, e->d_name);
        ssize_t n = readlink(src, link, sizeof link - 1);
        if (n <= 0) continue;
        link[n] = 0;
        if (strncmp(link, "usr/", 4) && strncmp(link, "/usr/", 5)) continue;
        snprintf(p, sizeof p, "%s/%s", R, e->d_name);
        if (symlink(link, p) == 0) note("/%s -> %s", e->d_name, link);
    }
    if (d) closedir(d);

    static const char *layered[] = { "usr", "etc", "var", "opt" };
    for (unsigned i = 0; i < sizeof layered / sizeof *layered; i++) layer_dir(state, layered[i]);

    bind_in("/home", "/home", 0, 0);   /* the shared workspace, read-write          */
    bind_in("/tmp",  "/tmp",  1, 0);   /* shared with us: sockets, scratch          */
    bind_in("/dev",  "/dev",  1, 0);   /* devices, plus /dev/pts and /dev/shm       */
    bind_in("/sys",  "/sys",  1, 1);   /* read-only: sysfs cannot be mounted fresh
                                          without our own NETWORK namespace         */
    /* A fresh /proc, which shows only the container's processes: proc is mounted
     * for the pid namespace of the process that mounts it — us, PID 1 in it. */
    snprintf(p, sizeof p, "%s/proc", R);
    mkdir(p, 0555);
    if (mount("proc", p, "proc", MS_NOSUID | MS_NODEV | MS_NOEXEC, NULL) != 0)
        die("mounting a fresh /proc: %s", strerror(errno));
    note("/proc: fresh, this pid namespace only");
    snprintf(p, sizeof p, "%s/run", R);
    mkdir(p, 0755);
    if (mount("tmpfs", p, "tmpfs", MS_NOSUID | MS_NODEV, "mode=0755") == 0)
        note("/run: fresh tmpfs");
    static const char *empty[] = { "mnt", "root", "srv", "media" };
    for (unsigned i = 0; i < sizeof empty / sizeof *empty; i++) {
        snprintf(p, sizeof p, "%s/%s", R, empty[i]);
        mkdir(p, 0755);
    }
    etc_file("resolv.conf");
    etc_file("hosts");
}

/* ---- step 3 (INIT): pivot_root ------------------------------------------ */
/*
 * pivot_root(new, put_old) swaps the process's root for NEW and parks the old root
 * on PUT_OLD (a directory under NEW), which we then detach. Unlike chroot(2) there
 * is no way back out: the old tree is no longer mounted anywhere we can reach.
 * (Our "/" is the initramfs. Before Linux 6.x pivot_root refused to move rootfs;
 * kernel 7.2 layers rootfs over an immutable "nullfs", which is exactly what makes
 * this work from an initramfs — see the comment above pivot_root in
 * linux/fs/namespace.c. If it is ever refused, MS_MOVE + chroot is the fallback.)
 */
static void enter_root(void) {
    char old[PATH_MAX];
    snprintf(old, sizeof old, "%s/.oldroot", R);
    mkdir(old, 0700);
    if (chdir(R) != 0) die("chdir %s: %s", R, strerror(errno));
    if (syscall(SYS_pivot_root, ".", ".oldroot") == 0) {
        if (chdir("/") != 0) die("chdir /: %s", strerror(errno));
        if (umount2("/.oldroot", MNT_DETACH) != 0) die("detaching the old root: %s", strerror(errno));
        rmdir("/.oldroot");
        note("root: pivot_root, old root detached");
    } else {
        int e = errno;
        if (mount(R, "/", NULL, MS_MOVE, NULL) != 0 || chroot(".") != 0 || chdir("/") != 0)
            die("pivot_root (%s) and MS_MOVE+chroot (%s) both failed", strerror(e), strerror(errno));
        rmdir("/.oldroot");
        note("root: MS_MOVE + chroot (pivot_root refused: %s)", strerror(e));
    }
}

/* ---- step 4 (INIT -> PROGRAM): exec as you, with nothing extra ----------- */
/*
 * We are uid 1000 inside already (the identity map), but this process still holds
 * the namespace's full capability set — that is how it could mount. execve(2)
 * clears them: a non-root uid keeps across exec only AMBIENT capabilities, and a
 * new user namespace starts with an empty ambient set (linux/kernel/
 * user_namespace.c, set_cred_user_ns). We also clear it explicitly and set
 * NO_NEW_PRIVS, so not even a setuid binary could hand them back (/mnt/host is
 * nosuid anyway). The program's environment is built fresh: the Lisp's own
 * variables (LOL_ROOTD_PID …) mean nothing in here.
 */
static char *envp_buf[96];

static void build_env(const char *home, const char *user) {
    static char e_home[PATH_MAX + 8], e_user[128], e_logname[128], e_shell[PATH_MAX + 8],
                e_term[128], e_ct[160];
    int n = 0;
    struct passwd *pw = getpwuid(getuid());            /* the CONTAINER's /etc/passwd */
    snprintf(e_home, sizeof e_home, "HOME=%s", home);
    snprintf(e_user, sizeof e_user, "USER=%s", user);
    snprintf(e_logname, sizeof e_logname, "LOGNAME=%s", user);
    snprintf(e_shell, sizeof e_shell, "SHELL=%s", pw && pw->pw_shell[0] ? pw->pw_shell : "/bin/sh");
    snprintf(e_term, sizeof e_term, "TERM=%s", getenv("TERM") ? getenv("TERM") : "dumb");
    snprintf(e_ct, sizeof e_ct, "LOL_CONTAINER=%s", opt_name);
    envp_buf[n++] = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
    envp_buf[n++] = e_home; envp_buf[n++] = e_user; envp_buf[n++] = e_logname;
    envp_buf[n++] = e_shell; envp_buf[n++] = e_term; envp_buf[n++] = "LANG=C.UTF-8";
    envp_buf[n++] = e_ct;
    /* -e VAR=VALUE: replace a default with the same name, or add */
    for (int i = 0; i < n_opt_env; i++) {
        size_t klen = strcspn(opt_env[i], "=");
        int j;
        for (j = 0; j < n; j++)
            if (!strncmp(envp_buf[j], opt_env[i], klen + 1)) break;
        if (j == n && n < 94) n++;
        envp_buf[j] = (char *)opt_env[i];
    }
    envp_buf[n] = NULL;
    environ = envp_buf;
}

static void exec_program(char **argv, const char *home, const char *user) {
    build_env(home, user);
    const char *cwd = opt_cwd ? opt_cwd : home;
    if (chdir(cwd) != 0) {
        fprintf(stderr, "lol-ct: cannot cd to %s (%s), staying in /\n", cwd, strerror(errno));
        chdir("/");
    }
    prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_CLEAR_ALL, 0, 0, 0);
    prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0);
    signal(SIGPIPE, SIG_DFL);
    /* -N: nothing to type into. The environment's run pane is read-only, yet its
     * stdout is a pty (so programs colour and line-buffer as on a terminal) — and a
     * pty is also an stdin something could block reading forever: a passphrase
     * prompt in a login script, say. /dev/null answers every read with end-of-file. */
    if (opt_no_stdin) {
        int fd = open("/dev/null", O_RDONLY);
        if (fd >= 0) { dup2(fd, 0); if (fd != 0) close(fd); }
    }
    execvp(argv[0], argv);                               /* searches the PATH above */
    fprintf(stderr, "lol-ct: %s: %s\n", argv[0], strerror(errno));
    _exit(EXIT_NOTFOUND);
}

/* ---- signals: two forwarders -------------------------------------------- */
/*
 * A signal to PID 1 of a pid namespace is IGNORED unless PID 1 has a handler for
 * it (pid_namespaces(7)) — the kernel protects every namespace's init the way it
 * protects the real one. So the container's init must catch the polite signals and
 * pass them on, or "Stop" would do nothing to a program that runs as PID 1. The
 * OUTER process forwards too, so the Lisp can simply signal the process it started.
 * If the outer one is SIGKILLed, PR_SET_PDEATHSIG takes the init down with it, and
 * the kernel then kills the whole namespace.
 */
static volatile pid_t forward_to = 0;
static void forward(int sig) { if (forward_to > 0) kill(forward_to, sig); }

static void install_forwarders(void) {
    static const int sigs[] = { SIGTERM, SIGINT, SIGHUP, SIGQUIT, SIGUSR1, SIGUSR2, SIGWINCH };
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = forward;
    sa.sa_flags = SA_RESTART;
    for (unsigned i = 0; i < sizeof sigs / sizeof *sigs; i++) sigaction(sigs[i], &sa, NULL);
}

static int status_to_exit(int status) {
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return EXIT_SETUP;
}

/* The init's loop: reap EVERY child (orphans are reparented to us — a daemon that
 * double-forked, say), and leave when the program itself has gone. Leaving kills
 * whatever else is still running in the namespace: one command, one lifetime. A
 * server you want to keep (postgres) is run as its OWN command, in the foreground. */
static int reap_until(pid_t main_pid) {
    for (;;) {
        int status;
        pid_t w = waitpid(-1, &status, 0);
        if (w == main_pid) return status_to_exit(status);
        if (w < 0 && errno != EINTR) return EXIT_SETUP;
    }
}

/* ---- the §3.3 question: can the WHOLE root be the lower layer? ----------- */

static int probe_root(void) {
    char base[] = "/tmp/lol-ct-probe.XXXXXX";
    if (!mkdtemp(base)) die("mkdtemp: %s", strerror(errno));
    enter_user_namespace(getuid(), getgid());
    if (mount(NULL, "/", NULL, MS_REC | MS_PRIVATE, NULL) != 0) die("private /: %s", strerror(errno));
    if (mount("tmpfs", base, "tmpfs", 0, "mode=0755") != 0) die("tmpfs: %s", strerror(errno));
    char upper[PATH_MAX], work[PATH_MAX], target[PATH_MAX];
    snprintf(upper, sizeof upper, "%s/u", base);
    snprintf(work, sizeof work, "%s/w", base);
    snprintf(target, sizeof target, "%s/m", base);
    mkdir(target, 0755);
    int rc = 0;
    for (int ux = 0; ux < 2; ux++) {
        int r = try_overlay(opt_hostroot, upper, work, target, ux);
        printf("overlay lowerdir=%s%s: %s\n", opt_hostroot, ux ? ",userxattr" : "",
               r == 0 ? "MOUNTED" : strerror(errno));
        if (r == 0) { umount2(target, MNT_DETACH); rc = 1; }
    }
    /* and one directory below it, which the design depends on */
    char usr[PATH_MAX];
    snprintf(usr, sizeof usr, "%s/usr", opt_hostroot);
    int r = try_overlay(usr, upper, work, target, 0);
    printf("overlay lowerdir=%s: %s\n", usr, r == 0 ? "MOUNTED" : strerror(errno));
    if (r == 0) umount2(target, MNT_DETACH);
    umount2(base, MNT_DETACH);
    rmdir(base);
    return rc ? 0 : 1;          /* 0 = the whole root CAN be a lower layer */
}

/* ---- main ------------------------------------------------------------------ */

static void usage(void) {
    fputs("usage: lol-ct [-n NAME] [-s STATEDIR] [-r HOSTROOT] [-C DIR] [-e VAR=VALUE]... [-N] [-v]\n"
          "              [--] PROGRAM [ARG...]\n"
          "       lol-ct --probe-root [-r HOSTROOT]\n"
          "Runs PROGRAM in a container whose root is HOSTROOT (default /mnt/host) with\n"
          "overlays whose changes live in STATEDIR (default $HOME/.lol/ct/NAME).\n"
          "See doc/containers.org.\n", stderr);
    _exit(EXIT_SETUP);
}

int main(int argc, char **argv) {
    int i = 1;
    for (; i < argc && argv[i][0] == '-'; i++) {
        const char *a = argv[i];
        if (!strcmp(a, "--")) { i++; break; }
        else if (!strcmp(a, "--probe-root")) opt_probe_root = 1;
        else if (!strcmp(a, "-v")) opt_verbose = 1;
        else if (!strcmp(a, "-N")) opt_no_stdin = 1;
        else if (i + 1 >= argc) usage();
        else if (!strcmp(a, "-n")) opt_name = argv[++i];
        else if (!strcmp(a, "-s")) snprintf(opt_state, sizeof opt_state, "%s", argv[++i]);
        else if (!strcmp(a, "-r")) opt_hostroot = argv[++i];
        else if (!strcmp(a, "-C")) opt_cwd = argv[++i];
        else if (!strcmp(a, "-e")) { if (n_opt_env < 64) opt_env[n_opt_env++] = argv[++i]; }
        else usage();
    }
    if (opt_probe_root) return probe_root();
    if (i >= argc) usage();
    char **prog = argv + i;
    if (strchr(opt_name, '/') || !opt_name[0] || opt_name[0] == '.')
        die("bad container name '%s'", opt_name);

    /* Who we are, OUTSIDE. Our uid stays our uid inside (the identity map). */
    uid_t uid = getuid();
    gid_t gid = getgid();
    const char *home = getenv("HOME");
    const char *user = getenv("USER");
    struct passwd *pw = getpwuid(uid);   /* our initramfs has no passwd: may be NULL */
    if (!home || !home[0]) home = pw ? pw->pw_dir : "/";
    if (!user || !user[0]) user = pw ? pw->pw_name : "user";
    if (!opt_state[0]) snprintf(opt_state, sizeof opt_state, "%s/.lol/ct/%s", home, opt_name);
    if (mkdirs(opt_state, 0755) != 0) die("cannot create the state directory %s: %s", opt_state, strerror(errno));
    if (!is_dir(opt_hostroot)) die("no host root at %s (is the laptop's disk mounted?)", opt_hostroot);

    enter_user_namespace(uid, gid);
    char hostname[80];
    snprintf(hostname, sizeof hostname, "lol-%s", opt_name);
    sethostname(hostname, strlen(hostname));

    /* CLONE_NEWPID moves only our CHILDREN into the new pid namespace — the caller
     * keeps its own pid — so the container's init is the process we fork next. */
    if (unshare(CLONE_NEWPID) != 0) die("unshare(pid): %s", strerror(errno));
    pid_t init = fork();
    if (init < 0) die("fork: %s", strerror(errno));
    if (init > 0) {                                   /* ---- OUTER ---- */
        forward_to = init;
        install_forwarders();
        int status;
        while (waitpid(init, &status, 0) < 0)
            if (errno != EINTR) return EXIT_SETUP;
        return status_to_exit(status);
    }

    /* ---- INIT: PID 1 of the container ---- */
    /* (No "is the parent still there?" check after this: getppid() is 0 here by
     * definition — our parent lives in the ANCESTOR pid namespace, where we have no
     * number for it. The first version checked, and exited silently every time.) */
    prctl(PR_SET_PDEATHSIG, SIGKILL);
    char lpath[PATH_MAX];
    snprintf(lpath, sizeof lpath, "%s/layout", opt_state);
    layout_f = fopen(lpath, "we");
    note("container %s, host root %s, state %s", opt_name, opt_hostroot, opt_state);
    build_root(opt_state);
    enter_root();
    if (layout_f) { fclose(layout_f); layout_f = NULL; }

    install_forwarders();
    pid_t prog_pid = fork();
    if (prog_pid < 0) die("fork: %s", strerror(errno));
    if (prog_pid == 0) {
        /* the program must not inherit the init's forwarding handlers */
        signal(SIGTERM, SIG_DFL); signal(SIGINT, SIG_DFL); signal(SIGHUP, SIG_DFL);
        signal(SIGQUIT, SIG_DFL); signal(SIGUSR1, SIG_DFL); signal(SIGUSR2, SIG_DFL);
        signal(SIGWINCH, SIG_DFL);
        exec_program(prog, home, user);
    }
    forward_to = prog_pid;
    return reap_until(prog_pid);
}
