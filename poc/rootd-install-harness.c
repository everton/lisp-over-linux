/* rootd-install-harness.c — lol-rootd's install request (ROOTD_SIG_INSTALL, 42),
 * the REAL code from initramfs/preinit.c, run on the host without root for
 * poc/update-test.lisp.
 *
 * Usage (always under `unshare -rm`, which makes us root of a private user and
 * mount namespace — nothing here touches the host's real /mnt):
 *     rootd-install-harness HOME HOSTDIR
 * Mounts a tmpfs on /mnt, binds HOSTDIR at /mnt/host and makes that read-only —
 * the shape preinit leaves (/mnt/host a read-only bind view) — then waits for
 * signal 42 and runs do_install(uid 0, gid 0, HOME), which copies HOME/.lol/stage/
 * into /mnt/host/boot/lisp/, i.e. HOSTDIR/boot/lisp/, where the test reads it.
 * SIGTERM ends it. (uid 0: the namespace maps only one uid, so setfsuid to any
 * other fails; the as-the-user half is the one thing this cannot show.)
 */
#define main preinit_main
#include "../initramfs/preinit.c"
#undef main
#include <sys/statvfs.h>

static volatile sig_atomic_t done = 0;
static void on_term(int sig) { (void)sig; done = 1; }

int main(int argc, char **argv) {
    if (argc != 3) { fprintf(stderr, "usage: %s HOME HOSTDIR\n", argv[0]); return 2; }
    if (mount("tmpfs", "/mnt", "tmpfs", 0, "mode=0755") != 0) { perror("tmpfs /mnt"); return 1; }
    if (mkdir("/mnt/host", 0755) != 0) { perror("mkdir /mnt/host"); return 1; }
    if (mount(argv[2], "/mnt/host", NULL, MS_BIND, NULL) != 0) { perror("bind"); return 1; }
    if (mount(NULL, "/mnt/host", NULL, MS_REMOUNT | MS_BIND | MS_RDONLY | MS_NOSUID | MS_NODEV, NULL) != 0) {
        perror("ro"); return 1;
    }
    /* (nosuid + nodev as preinit's /mnt/host has them — and a bind of /home must
     * keep them inside a user namespace: they are LOCKED, lol-ct.c explains) */
    signal(ROOTD_SIG_INSTALL, on_install);
    signal(SIGTERM, on_term);
    printf("ready\n"); fflush(stdout);
    while (!done) {
        if (want_install) {
            want_install = 0;
            do_install(0, 0, argv[1]);
            /* the view must be read-only again, as do_install promises */
            struct statvfs sv;
            if (statvfs("/mnt/host", &sv) == 0 && !(sv.f_flag & ST_RDONLY))
                install_status(0, 0, "error: harness: /mnt/host left writable");
        }
        msleep(20);                             /* pause() could miss a signal sent just before it */
    }
    return 0;
}
