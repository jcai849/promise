#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#define HAVE_SYS_SELECT_H
#include <R_ext/eventloop.h>

static int ifd, ofd;
static int fired = 0, active = 1;
static unsigned long intersleep = 300;

SEXP aih(SEXP tout, SEXP ap);
static void *thread(void *pass);
static void uih(void *data);
SEXP promise_set_sleep(SEXP tout);
static void millisleep(unsigned long tout);
void stopt(void);

SEXP aih(SEXP tout, SEXP ap) {
    int fds[2];
    pthread_t t;
    pthread_attr_t ta;

    pipe(fds);
    ifd = fds[0];
    ofd = fds[1];

    promise_set_sleep(tout);

    addInputHandler(R_InputHandlers, ifd, &uih, 32);

    pthread_attr_init(&ta);
    pthread_attr_setdetachstate(&ta, PTHREAD_CREATE_DETACHED);
    pthread_create(&t, &ta, thread, 0);

    return ScalarLogical(1);
}

static void *thread(void *pass) {
    char buf[16];
    while (active) {
        millisleep(intersleep);
        if (!fired) {
            fired = 1; *buf = 0;
            write(ofd, buf, 1);
        }
    }
    return 0;
}

static void uih(void *data) {
    char buf[16];

    read(ifd, buf, 16);

    // select on fd list (fds from promise.c)?

    fired = 0;
}

SEXP promise_set_sleep(SEXP tout) {
    intersleep = (unsigned long) (asReal(tout) * 1000.0 + 0.5);
    if (intersleep < 0) intersleep = 300;
    return ScalarLogical(1);
}

static void millisleep(unsigned long tout) {
    struct timeval tv;
    tv.tv_usec = (tout%1000)*1000;
    tv.tv_sec = tout/1000;
    select(0, 0, 0, 0, &tv);
}

void stopt(void) {
    active=0;
}
