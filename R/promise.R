promise <- function(executor) {
    pfd <- pipe_fd()
    callCC(function(k){
        make_tail <- function(state) function(value) {
            result <- make_result(state, value)
            send(pfd[2], result)
            k(NULL)
        }
        resolve <- make_tail("RES")
        reject <- make_tail("REJ")
        executor(resolve, reject)
    })
    close_fd(pfd[2])
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

state <- function(x, ...) UseMethod("state")
state.Promise <- function(x, ...) .Call(C_promise_state, x)

value <- function(x, ...) UseMethod("value")

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
    format_member("state", state(x)))
format_member <- function(tag, value)
    paste0("\t", c(paste0(tag, ":"), paste0("\t", capture.output(str(value)))))
print.Promise <- str.Promise <- function(x, ...) cat(format(x), sep="\n")

make_result <- function(state, value) {
    stopifnot(valid_state(state), !missing(value))
    structure(list(state=state, value=value), class="Result")
}

state.Result <- function(x, ...) x$state
value.Result <- function(x, ...) x$value
resolved.Result <- resolved.Promise
rejected.Result <- rejected.Promise
pending.Result <- pending.Promise

format.Result <- function(x, ...)
    c("Result:",
    format_member("State", state(x)),
    format_member("Value", value(x)))
print.Result <- str.Result <- function(x, ...) cat(format(x), sep="\n")

valid_states <- function() c("RES", "REJ", "PND")
valid_state <- function(state)
    is.character(state) && (length(state) == 1L) && state %in% valid_states()
