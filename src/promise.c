#include <unistd.h>
#include <stdio.h>

#include <R.h>
#include <Rinternals.h>
// #include <R_ext/eventloop.h>

#define MAX_THENS 10

typedef struct Promise {
    int fd;
    char *state;
    SEXP value;
    SEXP then[MAX_THENS];
} Promise;

SEXP C_pipe_fd(void) {
    SEXP fd;

    fd = PROTECT(allocVector(INTSXP, 2));
    pipe(INTEGER(fd));
    UNPROTECT(1);
    return fd;
}

SEXP C_close_fd(int fd) {
    close(fd);
    return ScalarLogical(1);
}

SEXP C_send(int fd, SEXP value, int length) {
    write(fd, RAW(value), length);
    return ScalarLogical(1);
}
