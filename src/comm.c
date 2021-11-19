#include <unistd.h>
#include <errno.h>

#include <R.h>
#include <Rinternals.h>

#define MAX_RECV_SIZE (1024*1024*128)
#define MAX_SEND_SIZE (1024*1024*128)

SEXP C_send(SEXP fd, SEXP value);
SEXP C_fetch(SEXP fd);
SEXP C_pipe_fd(void);
SEXP C_close_fd(int fd);

SEXP C_send(SEXP fd, SEXP value) {
    int n, fdi, need;
    unsigned int len = 0, i = 0;
    unsigned char *payload;

    fdi = *INTEGER(fd);
    len = LENGTH(value);
    payload = RAW(value);
#ifdef PROM_DEBUG
    Rprintf("Sending %d bytes to descriptor %d...\n", len, fdi);
#endif
    if (write(fdi, &len, sizeof len) != sizeof len) {
        close(fdi);
        fdi = -1;
        Rf_error("Failed to write header (n=%d, descriptor=%d) %s", n, fdi,
                    (n == -1 && errno) ? strerror(errno) : "");
    }
    while (i < len) {
        need = (len - i > MAX_SEND_SIZE) ? MAX_SEND_SIZE : (len - i);
        n = write(fdi, payload + i, need);
        if (n < 1) {
            close(fdi);
            fdi = -1;
            Rf_error("Failed to write (n=%d of %d, descriptor=%d) %s",
                    n, need, fdi, (n == -1 && errno) ? strerror(errno) : "");
        }
        i += n;
    }
    return ScalarLogical(1);
}

SEXP C_fetch(SEXP fd) {
    unsigned int len = 0, i = 0;
    unsigned char *payload;
    int n, fdi, need;
    SEXP out;

    fdi = *INTEGER(fd);
    n = read(fdi, &len, sizeof len);
#ifdef PROM_DEBUG
    Rprintf("Receiving %d bytes from descriptor %d...\n", len, fdi);
#endif
    if (n != sizeof(len) || len == 0) {
        close(fdi);
        fdi = -1;
        Rf_error("Header read error on descriptor %d", fdi);
    } else {
        out = PROTECT(allocVector(RAWSXP, len));
        payload = RAW(out);
        while (i < len) {
            need = (len - i > MAX_RECV_SIZE) ? MAX_RECV_SIZE : (len - i);
            n = read(fdi, payload + i, need);
            if (n < 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK) {
                    R_CheckUserInterrupt();
                    continue;
                }
                close(fdi);
                fdi = -1;
                Rf_error("Read error on descriptor %d: %s", fdi, strerror(errno));
            } else if (n == 0) {
                close(fdi);
                fdi = -1;
                Rf_error("Connection closed on descriptor %d before all data was received", fdi);
            }
            i += n;

        }
    }
    UNPROTECT(1);
    return out;
}

SEXP C_pipe_fd(void) {
    SEXP fd;

    fd = allocVector(INTSXP, 2);
    pipe(INTEGER(fd));
    return fd;
}

SEXP C_close_fd(int fd) {
    close(fd);
    return ScalarLogical(1);
}
