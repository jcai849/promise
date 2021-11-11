#include <unistd.h>
#include <stdio.h>

#include <R.h>
#include <Rinternals.h>
// #include <R_ext/eventloop.h>
 
int pfd[2];

typedef struct Prms {
    int pfd;
    SEXP then; // should be list of then's; one for now to test working
    SEXP thenContext; // environment in which to evaluate 'then'
} Promise;

SEXP C_promise(SEXP sExecutor, SEXP sResolve, SEXP rho) {
    pid_t cpid;
    Promise *promise;
    SEXP ptr, R_fcall;

    pipe(pfd);
    cpid = fork();
    if (cpid == 0) {
        close(pfd[1]);
        R_fcall = PROTECT(lang2(sExecutor, sResolve));
        eval(R_fcall, rho);
        exit(0);
    }
    close(pfd[0]);
    promise->pfd = pfd[1];
    promise->then = NULL;
    promise->thenContext = NULL;
    ptr = R_MakeExternalPtr(promise, install("promise"), R_NilValue);

    return ptr;
}

SEXP C_resolve(SEXP sValue) {
    // write(pfd[0], RAW(sValue), sValue->truelength); // not truelength?
    close(pfd[0]);
    exit(0);
}

SEXP C_then(SEXP sPromise, SEXP sOnFulfilled, SEXP rho) {
    Promise *ppromise;

    ppromise = R_ExternalPtrAddr(sPromise);
    ppromise->then = sOnFulfilled;
    ppromise->thenContext = rho;
    return sPromise;
}

SEXP run_then(SEXP sPromise) {
    Promise *ppromise;
    char buf[BUFSIZ];
    pid_t cpid;
    int n;
    SEXP ans, R_fcall, value;

    ppromise = R_ExternalPtrAddr(sPromise);
    while((n = read(ppromise->pfd, buf, sizeof buf)) > 0) {
        // how to malloc & fill RAWSEXPs?; save as unserialized variable 'value'
    }
    close(ppromise->pfd);

    pipe(pfd);
    cpid = fork();
    if (cpid == 0) {
        close(pfd[1]);
        R_fcall = PROTECT(lang2(ppromise->then, value));
        ans = PROTECT(eval(R_fcall, ppromise->thenContext));
        C_resolve(ans);
    }
    close(pfd[0]);
    ppromise->pfd = pfd[1];
    ppromise->then = NULL;
    ppromise->thenContext = NULL;
    return sPromise;
}
