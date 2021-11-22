promise <- function(executor) {
    pfd <- pipe()
    callCC(function(k) {
        make_tail <- function(state) function(value) {
            result <- make_result(state, value)
            send(pfd[2], result)
            k(NULL)
        }
        resolve <- make_tail("RES")
        reject <- make_tail("REJ")
        executor(resolve, reject)
    })
    close(pfd[2])
    promise <- make_promise(fd=pfd[1], state="PND", value=NULL)
    register_promise(promise)
    promise
}

then <- function(promise, onFulfilled, onRejected) {
    then <- make_then(onFulfilled, onRejected)
    register_then(promise, then)
    then
}

make_promise <- function(fd, state, value) {
    stopifnot(is.integer(fd) && length(fd) == 1,
              valid_state(state),
              !missing(value))
    promise_struct <- .Call(C_make_promise, fd, state, value)
    structure(promise_struct, class=c("Promise", class(promise_struct)))
}

make_then <- function(onFulfilled, onRejected) {
    stopifnot(is.function(onFulfilled), is.function(onRejected))
    promise <- make_promise(0L, "PND", NULL)
    then_struct <- .Call(C_make_then, promise, onFulfilled, onRejected)
    structure(then_struct, class=c("ThenPromise", class(then_struct)))
}

register_then <- function(promise, then) {
    stopifnot(is.Promise(promise), is.ThenPromise(then))
    .Call(C_register_then, promise, then)
}

register_promise <- function(promise) {
    stopifnot(is.Promise(promise))
    .Call(C_register_promise, promise)
}

settle_promise <- function(promise) {
    stopifnot(is.Promise(promise))
    .Call(C_settle_promise, promise, unserialize, new.env())
}

valid_states <- function() c("RES", "REJ", "PND")
valid_state <- function(state)
    is.character(state) && (length(state) == 1L) && state %in% valid_states()
state <- function(x, ...) UseMethod("state")
state.Promise <- function(x, ...) .Call(C_promise_state, x)

value <- function(x, ...) UseMethod("value")
value.Promise <- function(x, ...) {
    if (pending(x)) { "Undefined"
    } else .Call(C_promise_value, x)
}

resolved <- function(x, ...) UseMethod("resolved")
resolved.Promise <- function(x, ...) identical(state(x), "RES")
rejected <- function(x, ...) UseMethod("rejected")
rejected.Promise <- function(x, ...) identical(state(x), "REJ")
pending <- function(x, ...) UseMethod("pending")
pending.Promise <- function(x, ...) identical(state(x), "PND")

is.Promise <- function(x) inherits(x, "Promise")
is.ThenPromise <- function(x) inherits(x, "ThenPromise")
format.Promise <- function(x, ...)
    c("Promise:",
    format_member("state", state(x)),
    format_member("value", if (pending(x)) "Undefined" else
                                capture.output(str(value(x)))))
format_member <- function(tag, value)
    paste0("\t", c(paste0(tag, ":"), paste0("\t", value)))
print.Promise <- str.Promise <- function(x, ...) cat(format(x), sep="\n")
