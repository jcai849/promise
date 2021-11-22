#include <string.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include "comm.h"

/* To be made more robust upon attaining functionality */
#define MAX_THENS 10
#define STATE_SIZE 3
#define MAX_PROMISES 50
#define HASH(x) x % MAX_PROMISES
#define INSTALL(tab, id, val) tab[HASH(id)] = val
#define LOOKUP(tab, id) tab[HASH(id)]
/*                                                     */

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

SEXP C_make_promise(SEXP fd, SEXP state, SEXP value);
SEXP C_make_then(SEXP promise, SEXP onFulfilled, SEXP onRejected);
SEXP C_register_promise(SEXP promise);
SEXP C_register_then(SEXP promise, SEXP then);
SEXP C_promise_state(SEXP promise);
SEXP C_promise_value(SEXP promise);
SEXP C_settle_promise(SEXP promise, SEXP unserialize, SEXP rho); 

SEXP C_make_promise(SEXP fd, SEXP state, SEXP value) {
    Promise *promise;
    SEXP r_promise;

    promise = (Promise*) calloc(1, sizeof(Promise));
    promise->fd = *INTEGER(fd);
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

SEXP C_settle_promise(SEXP promise, SEXP unserialize, SEXP rho) {
    Promise *ppromise;
    SEXP value;

    ppromise = R_ExternalPtrAddr(promise);
    value = fetch(ppromise->fd);
    ppromise->value = eval(lang2(unserialize, value), rho);
    strncpy(ppromise->state, "RES", STATE_SIZE);
    close(ppromise->fd);
    ppromise->fd = -1;
    return ScalarLogical(1);
}

SEXP C_promise_state(SEXP promise) {
    return mkString(((Promise *) R_ExternalPtrAddr(promise))->state);
}

SEXP C_promise_value(SEXP promise) {
    return (((Promise *) R_ExternalPtrAddr(promise))->value);
}
