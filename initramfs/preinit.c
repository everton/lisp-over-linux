/* preinit: the real PID 1 entry point.
 *
 * Job: set up the minimal filesystem environment that SBCL needs, then
 * hand the process over to the Lisp supervisor via execv (the process keeps
 * PID 1 — execv replaces the program image, not the process).
 *
 * Why a C shim instead of doing this in Lisp?  An SBCL :executable image
 * locates its embedded core by reading /proc/self/exe, so /proc must already
 * be mounted *before* SBCL starts. The C preinit guarantees that ordering.
 *
 * Since storage landed (doc/storage.org) it does three more things, all of
 * them PRIVILEGED and all of them done once, here, while we are still root:
 *
 *   1. find the laptop's own disk and mount it PRIVATELY at /mnt/host
 *      (read-only), and bind-mount ONLY its /home onto our /home (read-write).
 *      Our / stays the tiny initramfs; the host's / is never our /.
 *   2. fork a tiny root helper, "lol-rootd", that keeps the two privileged
 *      jobs the Lisp cannot do as a user: chown hotplugged devices to the
 *      user, and remount the disk read-only before power-off.
 *   3. become the USER (e.g. everton, uid 1000) before exec'ing the Lisp,
 *      keeping only five named capabilities. Everything the environment
 *      creates on the real disk is then owned by the user — and it cannot
 *      write anywhere the user could not.
 *
 * Every step is non-fatal: no disk / no user means "run as root on the
 * initramfs alone", exactly the system as it was before storage (and what
 * QEMU without a disk image boots into).
 */
#define _GNU_SOURCE
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* Capability numbers and the capset(2) ABI, copied from the kernel's
 * include/uapi/linux/capability.h (musl ships no kernel headers). */
#define CAP_NET_BIND_SERVICE 10
#define CAP_NET_ADMIN        12
#define CAP_NET_RAW          13
#define CAP_SYS_BOOT         22
#define CAP_SYS_TTY_CONFIG   26
#define CAP_LAST_CAP         40   /* CAP_CHECKPOINT_RESTORE, as of 7.2 */
#define _LINUX_CAPABILITY_VERSION_3 0x20080522
struct __user_cap_header_struct { unsigned version; int pid; };
struct __user_cap_data_struct   { unsigned effective, permitted, inheritable; };

/* ---- small helpers ------------------------------------------------------ */

/* Everything we say goes into the KERNEL LOG via /dev/kmsg, so it is printed on
 * every console= (tty0 AND the serial line, where serial.log catches it) and
 * sits in dmesg next to the kernel's own lines — fd 2 alone reaches only the
 * last console= (tty0). "<6>" is the KERN_INFO priority prefix. */
static int kmsg_fd = -1;
static void say_line(const char *msg) {
    char line[600];
    int n = snprintf(line, sizeof line, "<6>preinit: %s\n", msg);
    /* Opened once, while still root, and kept: /dev/kmsg is root-only for
     * writing, and an fd already open stays usable after we become the user.
     * O_CLOEXEC so the Lisp does not inherit it. */
    if (kmsg_fd < 0) kmsg_fd = open("/dev/kmsg", O_WRONLY | O_CLOEXEC);
    if (kmsg_fd < 0 || write(kmsg_fd, line, (size_t)n) < 0) fputs(line + 3, stderr);
}
#define say(...) do { char m_[500]; snprintf(m_, sizeof m_, __VA_ARGS__); say_line(m_); } while (0)

static void msleep(long ms) {
    struct timespec ts = { ms / 1000, (ms % 1000) * 1000000L };
    nanosleep(&ts, NULL);
}

/* Read a whole small file into BUF (NUL-terminated). Returns length or -1. */
static int slurp(const char *path, char *buf, size_t size) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return -1;
    ssize_t n = read(fd, buf, size - 1);
    close(fd);
    if (n < 0) return -1;
    buf[n] = 0;
    return (int)n;
}

/* ---- configuration: /etc/lol.conf, overridden by the kernel cmdline ----- */
/*
 * build.sh writes /etc/lol.conf into the initramfs from the BUILD host, which
 * is the laptop the image boots on:  user=everton  disk=UUID=<fs of $HOME>.
 * Any `lol.user=` / `lol.disk=` on the kernel command line wins, so a GRUB
 * entry (or a different machine) can say otherwise without a rebuild.
 * lol.disk=none disables storage entirely.
 */
static char cfg_user[64] = "";
static char cfg_disk[128] = "";

static void set_key(const char *key, const char *val) {
    if (!strcmp(key, "user")) snprintf(cfg_user, sizeof cfg_user, "%s", val);
    if (!strcmp(key, "disk")) snprintf(cfg_disk, sizeof cfg_disk, "%s", val);
}

static void read_config(void) {
    char buf[4096], *line, *save;
    if (slurp("/etc/lol.conf", buf, sizeof buf) > 0)
        for (line = strtok_r(buf, "\n", &save); line; line = strtok_r(NULL, "\n", &save)) {
            char *eq = strchr(line, '=');
            if (line[0] == '#' || !eq) continue;
            *eq = 0;
            set_key(line, eq + 1);
        }
    /* /proc/cmdline is one line of space-separated words. */
    if (slurp("/proc/cmdline", buf, sizeof buf) > 0)
        for (line = strtok_r(buf, " \n", &save); line; line = strtok_r(NULL, " \n", &save)) {
            char *eq = strchr(line, '=');
            if (strncmp(line, "lol.", 4) || !eq) continue;
            *eq = 0;
            set_key(line + 4, eq + 1);
        }
    /* QEMU cannot pass a cmdline to an EFISTUB kernel booted from OVMF (ours is
     * built in), but it CAN hand the guest named blobs through its fw_cfg device:
     * -fw_cfg name=opt/lol/disk,string=9p:hostroot shows up, with
     * CONFIG_FW_CFG_SYSFS (tag #26), as a file under /sys/firmware/qemu_fw_cfg.
     * Real hardware has no fw_cfg, so these are test-only switches by construction. */
    {
        static const char *keys[] = { "user", "disk" };
        for (unsigned i = 0; i < sizeof keys / sizeof *keys; i++) {
            char path[128];
            snprintf(path, sizeof path, "/sys/firmware/qemu_fw_cfg/by_name/opt/lol/%s/raw", keys[i]);
            int n = slurp(path, buf, sizeof buf);
            if (n > 0) { buf[strcspn(buf, "\n")] = 0; set_key(keys[i], buf); }
        }
    }
}

/* ---- finding the disk: read ext4 superblocks ourselves ------------------ */
/*
 * There is no udev and no blkid here, so "UUID=..." has nobody to resolve it.
 * We do what blkid does for ext2/3/4: the superblock sits 1024 bytes into the
 * partition; at +0x38 is the magic 0xEF53 (little-endian), at +0x68 the
 * 16-byte UUID and at +0x78 the 16-byte volume label. /sys/class/block lists
 * every block device the kernel found (disks AND partitions), and devtmpfs
 * has made /dev/<name> for each.
 */
struct ext_id { int ok; char uuid[37]; char label[17]; };

static struct ext_id read_ext_id(const char *dev) {
    struct ext_id id = { 0 };
    unsigned char sb[256];
    int fd = open(dev, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return id;
    if (pread(fd, sb, sizeof sb, 1024) == (ssize_t)sizeof sb &&
        sb[0x38] == 0x53 && sb[0x39] == 0xEF) {
        const unsigned char *u = sb + 0x68;
        snprintf(id.uuid, sizeof id.uuid,
                 "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
                 u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7],
                 u[8], u[9], u[10], u[11], u[12], u[13], u[14], u[15]);
        memcpy(id.label, sb + 0x78, 16);
        id.ok = 1;
    }
    close(fd);
    return id;
}

/* Is there a controller whose disk we could be waiting for? Read each PCI
 * device's class/vendor/device from sysfs: class 0x010802 is "mass storage /
 * non-volatile memory / NVMe"; vendor 0x1af4 device 0x1001 or 0x1042 is a
 * virtio-blk disk (legacy / modern). Without one there is nothing to wait for —
 * a diskless QEMU boot must not stall. (QEMU's q35 always has an AHCI
 * controller, class 0x0106, which we have no driver for: deliberately ignored.) */
static int storage_controller_present(void) {
    DIR *d = opendir("/sys/bus/pci/devices");
    struct dirent *e;
    int found = 0;
    while (d && !found && (e = readdir(d))) {
        char p[300], v[32];
        if (e->d_name[0] == '.') continue;
        snprintf(p, sizeof p, "/sys/bus/pci/devices/%s/class", e->d_name);
        if (slurp(p, v, sizeof v) > 0 && !strncmp(v, "0x010802", 8)) found = 1;
        snprintf(p, sizeof p, "/sys/bus/pci/devices/%s/vendor", e->d_name);
        if (slurp(p, v, sizeof v) > 0 && !strncmp(v, "0x1af4", 6)) {
            snprintf(p, sizeof p, "/sys/bus/pci/devices/%s/device", e->d_name);
            if (slurp(p, v, sizeof v) > 0 && (!strncmp(v, "0x1001", 6) || !strncmp(v, "0x1042", 6)))
                found = 1;
        }
    }
    if (d) closedir(d);
    return found;
}

/* Resolve cfg_disk (UUID=…, LABEL=…, or a /dev path) to a device path in OUT.
 * NVMe and USB controllers probe asynchronously, so the partition may not
 * exist yet when we get here: retry for up to ~8 seconds — but only if a
 * controller that could carry it exists at all. */
static int find_disk(char *out, size_t size) {
    int attempts = storage_controller_present() ? 40 : 1;
    for (int attempt = 0; attempt < attempts; attempt++) {
        if (cfg_disk[0] == '/') {
            if (access(cfg_disk, F_OK) == 0) { snprintf(out, size, "%s", cfg_disk); return 1; }
        } else {
            DIR *d = opendir("/sys/class/block");
            struct dirent *e;
            while (d && (e = readdir(d))) {
                if (e->d_name[0] == '.') continue;
                char dev[300];
                snprintf(dev, sizeof dev, "/dev/%s", e->d_name);
                struct ext_id id = read_ext_id(dev);
                if (!id.ok) continue;
                if ((!strncmp(cfg_disk, "UUID=", 5) && !strcasecmp(cfg_disk + 5, id.uuid)) ||
                    (!strncmp(cfg_disk, "LABEL=", 6) && !strcmp(cfg_disk + 6, id.label))) {
                    snprintf(out, size, "%s", dev);
                    closedir(d);
                    return 1;
                }
            }
            if (d) closedir(d);
        }
        msleep(200);
    }
    return 0;
}

/* ---- QEMU only: the build host's REAL root over 9p ---------------------- */
/*
 * test-disk.img (make-test-disk.sh) has a passwd and a /home but no Debian, so
 * anything that runs the host's programs — a container (doc/containers.org),
 * Chrome (doc/web-browser.org), wpa_supplicant — cannot be tried against it.
 * For those, QEMU shares the build host's whole / READ-ONLY with
 *   -virtfs local,path=/,mount_tag=hostroot,security_model=none,readonly=on
 *   -fw_cfg name=opt/lol/disk,string=9p:hostroot
 * and we mount it where the laptop's disk would go. virtio-9p is a file-sharing
 * protocol over a virtio queue (CONFIG_NET_9P_VIRTIO + CONFIG_9P_FS, tag #26).
 * The host's /home must not be written from a test, so /home becomes an
 * OVERLAY: the host's /home as the read-only lower layer, a tmpfs as the upper
 * one that takes every write and vanishes at power-off. If overlay refuses the
 * 9p lower layer, an empty tmpfs /home is the fallback.
 */
static int mount_storage_9p(const char *tag) {
    mkdir("/mnt", 0755);
    mkdir("/mnt/host", 0755);
    if (mount(tag, "/mnt/host", "9p", MS_RDONLY | MS_NOSUID | MS_NODEV,
              "trans=virtio,version=9p2000.L,access=any,cache=loose,msize=262144") != 0) {
        say("storage: 9p mount of tag '%s' failed: %s", tag, strerror(errno));
        return 0;
    }
    mkdir("/run", 0755);
    mount("tmpfs", "/run", "tmpfs", 0, "size=256m,mode=0755");
    mkdir("/run/home-upper", 0755);
    mkdir("/run/home-work", 0755);
    if (mount("overlay", "/home", "overlay", 0,
              "lowerdir=/mnt/host/home,upperdir=/run/home-upper,workdir=/run/home-work") == 0)
        say("storage: 9p '%s' -> /mnt/host (ro); /home = host /home + tmpfs overlay", tag);
    else {
        say("storage: overlay on /home failed (%s) — empty tmpfs /home instead", strerror(errno));
        mount("tmpfs", "/home", "tmpfs", 0, "size=256m,mode=0755");
    }
    return 1;
}

/* ---- mounting: the host's / stays apart; only /home is shared ----------- */
static int mount_storage(void) {
    char dev[300];
    if (!cfg_disk[0] || !strcmp(cfg_disk, "none")) { say("storage: off (no disk configured)"); return 0; }
    if (!strncmp(cfg_disk, "9p:", 3)) return mount_storage_9p(cfg_disk + 3);
    if (!find_disk(dev, sizeof dev)) { say("storage: %s not found — running without a disk", cfg_disk); return 0; }

    mkdir("/mnt", 0755);
    mkdir("/mnt/host", 0755);
    /* nosuid+nodev: nothing on the host disk can become root here or be a
     * device node we honour. The filesystem itself is mounted read-WRITE —
     * the superblock has one rw/ro state, and /home needs to write. */
    if (mount(dev, "/mnt/host", "ext4", MS_NOSUID | MS_NODEV, NULL) != 0) {
        say("storage: mount %s on /mnt/host failed: %s", dev, strerror(errno));
        return 0;
    }
    /* A BIND mount makes the same directory appear in a second place. Only the
     * host's /home crosses over into our tree. */
    if (mount("/mnt/host/home", "/home", NULL, MS_BIND, NULL) != 0)
        say("storage: bind /home failed: %s", strerror(errno));
    /* Then make the /mnt/host VIEW read-only. A bind-remount changes the flags
     * of one mount point, not of the filesystem, so /home stays writable while
     * the host's /etc, /usr … can be read (fonts, pictures, docs) but never
     * written from here — even before we stop being root. */
    if (mount(NULL, "/mnt/host", NULL, MS_REMOUNT | MS_BIND | MS_RDONLY | MS_NOSUID | MS_NODEV, NULL) != 0)
        say("storage: read-only remount of /mnt/host failed: %s", strerror(errno));
    say("storage: %s (%s) -> /mnt/host (ro), /mnt/host/home -> /home (rw)", dev, cfg_disk);
    return 1;
}

/* ---- the user: looked up in the HOST's /etc/passwd ---------------------- */
/*
 * Our initramfs has no /etc/passwd (nobody logs in to it). The uid that owns
 * the user's files is whatever the host says it is, so we read the host's
 * passwd/group through /mnt/host. passwd lines: name:x:uid:gid:gecos:home:shell
 */
struct user { int ok; uid_t uid; gid_t gid; char home[256]; gid_t groups[64]; int ngroups; };

static struct user lookup_user(const char *name) {
    struct user u = { 0 };
    static char buf[1 << 16];
    char *line, *save;
    if (!name[0] || slurp("/mnt/host/etc/passwd", buf, sizeof buf) <= 0) return u;
    for (line = strtok_r(buf, "\n", &save); line; line = strtok_r(NULL, "\n", &save)) {
        char *f[7] = { 0 }; int n = 0;
        for (char *p = line; n < 7; n++) { f[n] = p; p = strchr(p, ':'); if (!p) { n++; break; } *p++ = 0; }
        if (n >= 6 && !strcmp(f[0], name)) {
            u.uid = (uid_t)atoi(f[2]); u.gid = (gid_t)atoi(f[3]);
            snprintf(u.home, sizeof u.home, "%s", f[5]);
            u.ok = 1;
            break;
        }
    }
    if (!u.ok) return u;
    /* Supplementary groups: group lines are name:x:gid:member,member,… */
    u.groups[u.ngroups++] = u.gid;
    if (slurp("/mnt/host/etc/group", buf, sizeof buf) > 0)
        for (line = strtok_r(buf, "\n", &save); line && u.ngroups < 64; line = strtok_r(NULL, "\n", &save)) {
            char *c1 = strchr(line, ':'), *c2 = c1 ? strchr(c1 + 1, ':') : NULL, *c3 = c2 ? strchr(c2 + 1, ':') : NULL;
            if (!c3) continue;
            gid_t g = (gid_t)atoi(c2 + 1);
            for (char *m = strtok(c3 + 1, ","); m; m = strtok(NULL, ","))
                if (!strcmp(m, name) && g != u.gid) { u.groups[u.ngroups++] = g; break; }
        }
    return u;
}

/* ---- devices: the user must be able to open what the Lisp opens --------- */
/*
 * devtmpfs creates nodes root-owned (fb0 0600, input/event* 0600 …). The Lisp
 * opens /dev/fb0 and /dev/tty0 read-write and /dev/input/event* read-only, so
 * hand those to the user. Run once now, then again by lol-rootd whenever a
 * device appears later (a USB mouse plugged in after boot).
 */
static void chown_dir_matching(const char *dir, const char *prefix, uid_t uid, gid_t gid) {
    DIR *d = opendir(dir);
    struct dirent *e;
    while (d && (e = readdir(d))) {
        if (strncmp(e->d_name, prefix, strlen(prefix))) continue;
        char p[300]; struct stat st;
        snprintf(p, sizeof p, "%s/%s", dir, e->d_name);
        if (lstat(p, &st) == 0 && (S_ISCHR(st.st_mode)) && st.st_uid != uid)
            chown(p, uid, gid);
    }
    if (d) closedir(d);
}

static void give_devices(uid_t uid, gid_t gid) {
    chown_dir_matching("/dev", "fb", uid, gid);
    chown_dir_matching("/dev", "tty", uid, gid);   /* tty, tty0, tty1 … (not ttyS*: harmless too) */
    chown_dir_matching("/dev/input", "event", uid, gid);
}

/* ---- lol-rootd: the one process that stays root ------------------------- */
/*
 * Privilege separation in miniature. The Lisp runs as the user; this child of
 * ours keeps root for exactly two jobs:
 *   - every second, give newly appeared device nodes to the user (a poor
 *     man's udev: polling is ~nothing at this scale and needs no netlink);
 *   - on SIGUSR1 (sent by the Lisp's power-off), sync and remount the disk
 *     read-only, so the host finds a clean filesystem next boot. A user
 *     cannot unmount (that needs CAP_SYS_ADMIN, which is nearly root).
 * The Lisp finds it through $LOL_ROOTD_PID.
 */
static volatile sig_atomic_t want_shutdown = 0;
static void on_usr1(int sig) { (void)sig; want_shutdown = 1; }

static pid_t start_rootd(uid_t uid, gid_t gid, int have_disk) {
    pid_t pid = fork();
    if (pid != 0) return pid;                   /* parent (or fork failed: -1) */
    prctl(PR_SET_NAME, "lol-rootd");
    /* Real uid = the user, effective + saved uid = root. kill(2) lets a sender
     * signal a process whose REAL (or saved) uid equals the sender's, so the
     * user's Lisp may send us SIGUSR1 — yet with euid 0 we keep root's powers. */
    setresuid(uid, 0, 0);
    signal(SIGUSR1, on_usr1);
    for (;;) {
        if (want_shutdown) {
            sync();
            if (have_disk) {
                /* Remounting the SUPERBLOCK read-only (no MS_BIND) affects both
                 * /home and /mnt/host — they are one filesystem. */
                if (mount(NULL, "/home", NULL, MS_REMOUNT | MS_RDONLY, NULL) != 0)
                    say("lol-rootd: read-only remount failed: %s (synced anyway)", strerror(errno));
                else
                    say("lol-rootd: disk remounted read-only — safe to power off");
            }
            want_shutdown = 0;
        }
        give_devices(uid, gid);
        sleep(1);                               /* a signal cuts this short */
    }
}

/* ---- becoming the user, keeping five capabilities ----------------------- */
/*
 * A capability is one slice of root's power. The Lisp still needs a few:
 *   CAP_SYS_BOOT          reboot(2) — the power-off menu entry
 *   CAP_NET_ADMIN         SIOCSIFADDR/SIOCSIFFLAGS — bring eth0 up (net.lisp)
 *   CAP_NET_RAW           SO_BINDTODEVICE — DHCP on eth0 (dhcp.lisp)
 *   CAP_NET_BIND_SERVICE  bind UDP port 68 (< 1024) — the DHCP client port
 *   CAP_SYS_TTY_CONFIG    KDSETMODE on /dev/tty0 — KD_GRAPHICS (poc/fb.lisp)
 * Notably absent: CAP_DAC_OVERRIDE (read/write any file) and CAP_SYS_ADMIN
 * (mount …). So file access is checked exactly as for the user.
 *
 * Capabilities normally do not survive execve() for a non-root program. The
 * AMBIENT set (Linux 4.3+) is the mechanism that makes them survive: a cap that
 * is permitted + inheritable + ambient is handed to the new program as
 * permitted + effective. Sequence:
 *   PR_SET_KEEPCAPS   so setuid() away from 0 does not wipe the permitted set
 *   setgroups/setresgid/setresuid   become the user (gid first: after the uid
 *                     change we could no longer change gids)
 *   capset            permitted = inheritable = effective = the five
 *   PR_CAP_AMBIENT_RAISE  each of the five
 */
static const int keep_caps[] = { CAP_SYS_BOOT, CAP_NET_ADMIN, CAP_NET_RAW,
                                 CAP_NET_BIND_SERVICE, CAP_SYS_TTY_CONFIG };
#define NKEEP (int)(sizeof keep_caps / sizeof keep_caps[0])

static int become_user(const struct user *u) {
    /* First shrink the BOUNDING set — the ceiling no later exec can exceed —
     * to the five, while we still hold CAP_SETPCAP to do so. */
    for (int c = 0; c <= CAP_LAST_CAP; c++) {
        int keep = 0;
        for (int i = 0; i < NKEEP; i++) keep |= (keep_caps[i] == c);
        if (!keep) prctl(PR_CAPBSET_DROP, c, 0, 0, 0);
    }
    if (prctl(PR_SET_KEEPCAPS, 1, 0, 0, 0) != 0 ||
        setgroups((size_t)u->ngroups, u->groups) != 0 ||
        setresgid(u->gid, u->gid, u->gid) != 0 ||
        setresuid(u->uid, u->uid, u->uid) != 0) {
        say("user: switching to uid %d failed: %s", (int)u->uid, strerror(errno));
        return 0;
    }
    /* musl has no libcap; the raw syscall takes a header + two 32-bit words. */
    struct __user_cap_header_struct hdr = { _LINUX_CAPABILITY_VERSION_3, 0 };
    struct __user_cap_data_struct data[2];
    memset(data, 0, sizeof data);
    for (int i = 0; i < NKEEP; i++) {
        int c = keep_caps[i];
        data[c / 32].permitted   |= 1u << (c % 32);
        data[c / 32].inheritable |= 1u << (c % 32);
        data[c / 32].effective   |= 1u << (c % 32);
    }
    if (syscall(SYS_capset, &hdr, data) != 0) say("user: capset failed: %s", strerror(errno));
    for (int i = 0; i < NKEEP; i++)
        if (prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_RAISE, keep_caps[i], 0, 0) != 0)
            say("user: ambient cap %d failed: %s", keep_caps[i], strerror(errno));
    return 1;
}

int main(void) {
    /* Pseudo-filesystems. MS_NOSUID|MS_NODEV|MS_NOEXEC are good hygiene but
     * omitted here for didactic clarity. Errors are non-fatal. */
    mount("proc",     "/proc", "proc",     0, NULL);          /* /proc/self/exe etc. */
    mount("sysfs",    "/sys",  "sysfs",    0, NULL);
    mount("devtmpfs", "/dev",  "devtmpfs", 0, NULL);          /* populated /dev      */

    /* A real, size-limited scratch filesystem on /tmp (needs CONFIG_TMPFS=y).
     * The 5th arg is tmpfs-specific options: cap it at 64 MB, mode 1777. */
    mount("tmpfs",    "/tmp",  "tmpfs",    0, "size=64m,mode=1777");

    /* Pseudo-terminals (tag #26 onward): a terminal pane, a container's shell,
     * anything that opens /dev/ptmx gets its slave end under /dev/pts — which
     * exists only once devpts is mounted. ptmxmode=666 so the user can open it. */
    mkdir("/dev/pts", 0755);
    mount("devpts",   "/dev/pts", "devpts", MS_NOSUID | MS_NOEXEC, "mode=620,ptmxmode=666");
    /* POSIX shared memory (shm_open) lives in a tmpfs at /dev/shm; Chrome and
     * PostgreSQL both need it. */
    mkdir("/dev/shm", 01777);
    mount("tmpfs",    "/dev/shm", "tmpfs", MS_NOSUID | MS_NODEV, "mode=1777");

    /* SBCL's generational GC uses write-protected pages + a SIGSEGV handler as
     * its write barrier; it handles each fault and resumes. Otherwise the kernel
     * logs every one as "lisp[1]: segfault ..." onto the console. Silence those
     * unhandled-signal messages so the console stays readable. */
    {
        FILE *f = fopen("/proc/sys/debug/exception-trace", "w");
        if (f) { fputs("0\n", f); fclose(f); }
    }

    /* Storage and identity (doc/storage.org). */
    read_config();
    int have_disk = mount_storage();
    struct user u = { 0 };
    if (have_disk) {
        u = lookup_user(cfg_user);
        if (!u.ok) say("user: '%s' not in the host's /etc/passwd — staying root", cfg_user);
    }

    /* The environment the Lisp sees. HOME matters most: (user-homedir-pathname)
     * and the file manager both start there. */
    static char env_home[300], env_user[100], env_rootd[40];
    snprintf(env_home, sizeof env_home, "HOME=%s", u.ok ? u.home : "/");
    snprintf(env_user, sizeof env_user, "USER=%s", u.ok ? cfg_user : "root");
    /* Fixed slots first; optional ones are appended at N and the array stays
     * NULL-terminated because it is zero-initialised past them. */
    char *envp[8] = { env_home, env_user, "PATH=/sbin", "TERM=linux" };
    int n_env = 4;
    env_rootd[0] = 0;

    /* QEMU's net-REPL switch (net.lisp, NET-REPL-WANTED-P). The fw_cfg files are
     * readable by root only (0400), and the Lisp may run as the user, so we read
     * it here, while still root, and hand it down as LOL_NETREPL=1. */
    {
        char sw[8];
        if (slurp("/sys/firmware/qemu_fw_cfg/by_name/opt/lol/netrepl/raw", sw, sizeof sw) > 0
            && sw[0] == '1')
            envp[n_env++] = "LOL_NETREPL=1";
    }

    if (u.ok) {
        give_devices(u.uid, u.gid);
        pid_t rootd = start_rootd(u.uid, u.gid, have_disk);
        if (rootd > 0) {
            snprintf(env_rootd, sizeof env_rootd, "LOL_ROOTD_PID=%d", (int)rootd);
            envp[n_env++] = env_rootd;
        }
        if (become_user(&u)) {
            say("user: now %s (uid %d gid %d, %d groups), home %s",
                cfg_user, (int)u.uid, (int)u.gid, u.ngroups, u.home);
            if (chdir(u.home) != 0) chdir("/");
        }
    }

    /* Become the Lisp supervisor. argv[0] is conventional; no "worker"
     * argument means "run as the supervisor" (see the Lisp toplevel). */
    char *argv[] = { "/sbin/lisp", NULL };
    execve("/sbin/lisp", argv, envp);

    /* Only reached if execv failed — keep PID 1 alive so the kernel doesn't panic. */
    perror("preinit: execv /sbin/lisp failed");
    for (;;) pause();
    return 1;
}
