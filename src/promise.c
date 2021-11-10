#include <R.h>
#include <Rinternals.h>
#include <R_ext/eventloop.h>

#include <unistd.h>

int pfd[2];

SEXP C_promise(SEXP sExecutorExpression, SEXP rho) {
    pid_t cpid;

    pipe(pfd);
    cpid = fork();
    if (cpid == 0) {
        close(pfd[1]);
        eval(sExecutorExpression, rho)
        exit(0)
    } else {
        close(pfd[0]);
        return(pfd[1]);
    }
}

SEXP C_resolve(SEXP sValue) {
    write(pfd[0], sValue, SIZEOF(sValue))
    close(pfd[0])
    exit(0)
}
