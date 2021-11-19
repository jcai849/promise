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

SEXP C_start_monitoring(void);
static void *fire_loop(void *pass);
static void monitor(void *data);
SEXP C_set_sleep(SEXP timeout);
static void millisleep(unsigned long timeout);
SEXP C_stop_monitoring(void);

SEXP C_start_monitoring(void) {
    int fds[2];
    pthread_t *t;
    pthread_attr_t *ta;

    pipe(fds);
    ifd = fds[0];
    ofd = fds[1];

    addInputHandler(R_InputHandlers, ifd, &monitor, 32);

    pthread_attr_init(ta);
    pthread_attr_setdetachstate(ta, PTHREAD_CREATE_DETACHED);
    pthread_create(t, ta, fire_loop, 0);

    return ScalarLogical(1);
}

static void *fire_loop(void *pass) {
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

static void monitor(void *data) {
    char buf[16];

    read(ifd, buf, 16);

    // select on fd list (fds from promise.c)?
    // settle_promise

    fired = 0;
}

SEXP C_set_sleep(SEXP timeout) {
    intersleep = (unsigned long) (asReal(timeout) * 1000.0 + 0.5);
    if (intersleep < 0) intersleep = 300;
    return ScalarLogical(1);
}

static void millisleep(unsigned long timeout) {
    struct timeval tv;
    tv.tv_usec = (timeout%1000)*1000;
    tv.tv_sec = timeout/1000;
    select(0, 0, 0, 0, &tv);
}

SEXP C_stop_monitoring(void) {
    active=0;
    return ScalarLogical(1);
}
