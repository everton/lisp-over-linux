/* =====================================================================
 *  *** THIS RUNS ON YOUR HOST MACHINE — NOT INSIDE THE LISP MACHINE. ***
 * =====================================================================
 *
 * lol-repl-client — a host-side terminal for the lisp-over-linux network REPL.
 *
 * This is plain HOST tooling. It is NOT shipped in the initramfs, never runs as
 * the guest's PID 1, and has nothing to do with SBCL — that is why it lives in
 * host-client/ and not in initramfs/. Its only job is to be the "terminal" for
 * the guest's REPL:
 *
 *   - it puts YOUR terminal into raw mode, so every keystroke (arrows, Ctrl-C,
 *     Ctrl-D, ...) becomes a byte instead of being line-buffered/echoed locally;
 *   - it shuttles bytes both ways over TCP.
 *
 * Because the keystrokes arrive raw, the line editor running INSIDE the guest
 * (read-line-edited) receives the arrow escape sequences and drives history /
 * editing remotely; its redraw escapes come back and we paint them. So we get
 * full editing over a bare socket with no PTY on the guest side. (networking.org
 * §6a — the raw-forwarding client.)
 *
 * Build (host, any libc):
 *     cc -O2 -o lol-repl-client lol-repl-client.c
 * Usage:
 *     ./lol-repl-client [host] [port]      # defaults: 127.0.0.1 4005
 *
 * Keys (the guest interprets these; we just forward them raw):
 *     Ctrl-C  abort the current input line — you STAY connected
 *     Ctrl-D  on an empty line: the guest ends the session, so we disconnect
 *
 * The local terminal is ALWAYS restored on exit (normal, error, or signal).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <termios.h>
#include <signal.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <netdb.h>

static struct termios saved_tio;
static int            raw_active = 0;

static void restore_tty(void) {
    if (raw_active) {
        tcsetattr(STDIN_FILENO, TCSADRAIN, &saved_tio);
        raw_active = 0;
        fprintf(stderr, "\r\n[disconnected]\r\n");
    }
}

static void on_signal(int sig) {            /* SIGTERM/HUP/etc: restore, then die */
    (void)sig;
    restore_tty();
    _exit(1);
}

/* write all N bytes (loopback rarely short-writes, but be correct anyway) */
static int write_all(int fd, const char *p, ssize_t n) {
    while (n > 0) {
        ssize_t w = write(fd, p, n);
        if (w < 0) { if (errno == EINTR) continue; return -1; }
        p += w; n -= w;
    }
    return 0;
}

int main(int argc, char **argv) {
    const char *host = argc > 1 ? argv[1] : "127.0.0.1";
    const char *port = argc > 2 ? argv[2] : "4005";

    struct addrinfo hints, *res, *rp;
    memset(&hints, 0, sizeof hints);
    hints.ai_family   = AF_INET;            /* IPv4, matching the guest */
    hints.ai_socktype = SOCK_STREAM;
    int err = getaddrinfo(host, port, &hints, &res);
    if (err) {
        fprintf(stderr, "cannot resolve %s:%s: %s\n", host, port, gai_strerror(err));
        return 1;
    }
    int sock = -1;
    for (rp = res; rp; rp = rp->ai_next) {
        sock = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock < 0) continue;
        if (connect(sock, rp->ai_addr, rp->ai_addrlen) == 0) break;
        close(sock); sock = -1;
    }
    freeaddrinfo(res);
    if (sock < 0) {
        fprintf(stderr, "could not connect to %s:%s: %s\n", host, port, strerror(errno));
        fprintf(stderr, "is the guest up, and is the network REPL enabled "
                        "('t' in the supervisor menu)?\n");
        return 1;
    }

    if (!isatty(STDIN_FILENO)) {
        fprintf(stderr, "stdin is not a terminal; this client needs a real tty.\n");
        return 1;
    }

    /* Raw mode: disables canonical buffering, local echo AND signal keys (ISIG),
     * so Ctrl-C/Ctrl-D reach us as bytes 0x03/0x04 and we forward them to the
     * guest, which decides what they mean. Always restored on exit. */
    if (tcgetattr(STDIN_FILENO, &saved_tio) == 0) {
        struct termios raw = saved_tio;
        cfmakeraw(&raw);
        /* cfmakeraw() also clears OPOST (output post-processing). Put it back:
         * the guest sends a bare '\n' after each line and tracks the cursor
         * column assuming '\n' returns to column 0 (the normal ONLCR -> CR+LF
         * mapping). Without OPOST|ONLCR here, '\n' moves down but NOT to column
         * 0, so each "=> result" line starts wherever the cursor happened to be.
         * We only want raw INPUT (keystrokes forwarded); output stays cooked. */
        raw.c_oflag |= OPOST | ONLCR;
        tcsetattr(STDIN_FILENO, TCSANOW, &raw);
        raw_active = 1;
        atexit(restore_tty);
        signal(SIGTERM, on_signal);
        signal(SIGHUP,  on_signal);
        signal(SIGQUIT, on_signal);
    }

    char buf[4096];
    for (;;) {
        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(STDIN_FILENO, &rfds);
        FD_SET(sock, &rfds);
        int maxfd = sock > STDIN_FILENO ? sock : STDIN_FILENO;
        if (select(maxfd + 1, &rfds, NULL, NULL, NULL) < 0) {
            if (errno == EINTR) continue;
            break;
        }
        if (FD_ISSET(STDIN_FILENO, &rfds)) {            /* keystrokes -> guest */
            ssize_t n = read(STDIN_FILENO, buf, sizeof buf);
            if (n <= 0) break;
            if (write_all(sock, buf, n) < 0) break;
        }
        if (FD_ISSET(sock, &rfds)) {                    /* guest output -> screen */
            ssize_t n = read(sock, buf, sizeof buf);
            if (n <= 0) break;                          /* guest closed (Ctrl-D/:quit) */
            if (write_all(STDOUT_FILENO, buf, n) < 0) break;
        }
    }

    restore_tty();
    close(sock);
    return 0;
}
