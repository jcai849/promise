#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <R.h>
#include <Rinternals.h>
// #include <R_ext/eventloop.h>

#define MAX_THENS 10 /* All inflexible for now */
#define STATE_SIZE 3
#define MAX_PROMISES 50
#define HASH(x) x % MAX_PROMISES
#define INSTALL(tab, id, val) tab[HASH(id)] = val
#define LOOKUP(tab, id) tab[HASH(id)]

typedef struct Promise {
    int fd;
    char state[STATE_SIZE+1];
    SEXP value;

    struct Promise *then[MAX_THENS]; /* private */
    int then_i;		             /* private */

    SEXP onFulfilled; /* used only for then */
    SEXP onRejected;  /* used only for then */
} Promise;

static int fds[MAX_PROMISES];
static Promise *fd_promise_map[MAX_PROMISES];

SEXP C_make_promise(int fd, SEXP state, SEXP value) {
    Promise *promise;
    SEXP r_promise;

    promise = (Promise*) calloc(1, sizeof(Promise));
    promise->fd = fd;
    strncpy(promise->state, CHAR(STRING_ELT(state, 0)), STATE_SIZE);
    promise->value = value;
    promise->then_i = 0;
    r_promise = R_MakeExternalPtr(promise, install("Promise"), value);
    return r_promise;
}

SEXP C_make_then(SEXP promise, SEXP onFulfilled, SEXP onRejected) {
    Promise *ppromise;
    ppromise = R_ExternalPtrAddr(promise);
    ppromise->onFulfilled = onFulfilled;
    ppromise->onRejected = onRejected;
    return promise;
}

SEXP C_register_promise(SEXP promise) {
    Promise *ppromise;

    ppromise = R_ExternalPtrAddr(promise);
    INSTALL(fds, ppromise->fd, ppromise->fd);
    INSTALL(fd_promise_map, ppromise->fd, ppromise);

    return ScalarLogical(1);
}

SEXP C_register_then(SEXP promise, SEXP then) {
    Promise *ppromise, *pthen;

    ppromise = R_ExternalPtrAddr(promise);
    pthen = R_ExternalPtrAddr(then);
    ppromise->then[ppromise->then_i++] = pthen;
    return ScalarLogical(1);
}

SEXP C_promise_state(SEXP promise) {
    return mkString(((Promise *) R_ExternalPtrAddr(promise))->state);
}

SEXP C_send(int fd, SEXP value) {
    unsigned int len;

    len = 0;
    len = LENGTH(value);
    write(fd, &len, sizeof len);
    write(fd, RAW(value), len);
    return ScalarLogical(1);
}

SEXP C_fetch(int fd) {
    unsigned int len;
    SEXP out;

    len = 0;

     /* I will neaten this up w/ checks etc., *
      * once it is all working...	      */
    read(fd, &len, sizeof len);
    out = PROTECT(allocVector(RAWSXP, len));
    read(fd, RAW(out), len);
    UNPROTECT(1);
    return out;
}

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

