/* preinit: the real PID 1 entry point.
 *
 * Job: set up the minimal filesystem environment that SBCL needs, then
 * hand the process over to the Lisp supervisor via execv (the process keeps
 * PID 1 — execv replaces the program image, not the process).
 *
 * Why a C shim instead of doing this in Lisp?  An SBCL :executable image
 * locates its embedded core by reading /proc/self/exe, so /proc must already
 * be mounted *before* SBCL starts. The C preinit guarantees that ordering.
 */
#include <sys/mount.h>
#include <unistd.h>
#include <stdio.h>

int main(void) {
    /* Pseudo-filesystems. MS_NOSUID|MS_NODEV|MS_NOEXEC are good hygiene but
     * omitted here for didactic clarity. Errors are non-fatal. */
    mount("proc",     "/proc", "proc",     0, NULL);          /* /proc/self/exe etc. */
    mount("sysfs",    "/sys",  "sysfs",    0, NULL);
    mount("devtmpfs", "/dev",  "devtmpfs", 0, NULL);          /* populated /dev      */

    /* A real, size-limited scratch filesystem on /tmp (needs CONFIG_TMPFS=y).
     * The 5th arg is tmpfs-specific options: cap it at 64 MB, mode 1777. */
    mount("tmpfs",    "/tmp",  "tmpfs",    0, "size=64m,mode=1777");

    /* SBCL's generational GC uses write-protected pages + a SIGSEGV handler as
     * its write barrier; it handles each fault and resumes. Otherwise the kernel
     * logs every one as "lisp[1]: segfault ..." onto the console. Silence those
     * unhandled-signal messages so the console stays readable. */
    {
        FILE *f = fopen("/proc/sys/debug/exception-trace", "w");
        if (f) { fputs("0\n", f); fclose(f); }
    }

    /* Become the Lisp supervisor. argv[0] is conventional; no "worker"
     * argument means "run as the supervisor" (see the Lisp toplevel). */
    char *argv[] = { "/sbin/lisp", NULL };
    execv("/sbin/lisp", argv);

    /* Only reached if execv failed — keep PID 1 alive so the kernel doesn't panic. */
    perror("preinit: execv /sbin/lisp failed");
    for (;;) pause();
    return 1;
}
