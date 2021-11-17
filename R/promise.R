promise <- function(executor) {
    pfd <- pipe_fd()
    callCC(function(k){
        make_tail <- function(state) function(value) {
            result <- make_result(state, value)
            send(pfd[2], result)
            k(NULL)
        }
        resolve <- make_tail("RESOLVED")
        reject <- make_tail("REJECTED")
        executor(resolve, reject)
    })
    close_fd(pfd[2])
    promise <- make_promise(fd=pfd[1], state="PENDING", value=NULL)
    register_promise(promise)
    promise
}

then <- function(promise, onFulfilled, onRejected) {
    then <- make_then(onFulfilled, onRejected)
    register_then(promise, then)
    then
}

make_promise <- function(fd, state, value) {
    stopifnot(valid_state(state))
    promise <- structure(new.env(parent=emptyenv()), class="Promise")
    fd(promise) <- fd
    state(promise) <- state
    value(promise) <- value
    promise
}

make_then <- function(onFulfilled, onRejected) {
    then <- make_promise(NULL, "PENDING", NULL)
    class(then) <- c(class(then), "ThenPromise")
    onFulfilled(then) <- onFulfilled
    onRejected(then) <- onRejected
    then
}

fd <- function(x, ...) UseMethod("fd")
fd.Promise <- function(x, ...) x$fd
`fd<-` <- function(x, value) UseMethod("fd<-")
`fd<-.Promise` <- function(x, value) {x$fd <- value; x}

state <- function(x, ...) UseMethod("state")
state.Promise <- function(x, ...) x$state
`state<-` <- function(x, value) UseMethod("state<-")
`state<-.Promise` <- function(x, value) {x$state <- value; x}

value <- function(x, ...) UseMethod("value")
value.Promise <- function(x, ...) x$value
`value<-` <- function(x, value) UseMethod("value<-")
`value<-.Promise` <- function(x, value) {x$value <- value; x}

onFulfilled <- function(x, ...) UseMethod("onFulfilled")
onFulfilled.ThenPromise <- function(x, ...) x$onFulfilled
`onFulfilled<-` <- function(x, onFulfilled) UseMethod("onFulfilled<-")
`onFulfilled<-.ThenPromise` <- function(x, onFulfilled) {x$onFulfilled <- onFulfilled; x}

onRejected <- function(x, ...) UseMethod("onRejected")
onRejected.ThenPromise <- function(x, ...) x$onRejected
`onRejected<-` <- function(x, onRejected) UseMethod("onRejected<-")
`onRejected<-.ThenPromise` <- function(x, onRejected) {x$onRejected <- onRejected; x}

resolved <- function(x, ...) UseMethod("resolved")
resolved.Promise <- function(x, ...) identical(state(x), "RESOLVED")
rejected <- function(x, ...) UseMethod("rejected")
rejected.Promise <- function(x, ...) identical(state(x), "REJECTED")
pending <- function(x, ...) UseMethod("pending")
pending.Promise <- function(x, ...) identical(state(x), "PENDING")

is.Promise <- function(x) inherits(x, "Promise")
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

valid_states <- function() c("RESOLVED", "REJECTED", "PENDING")
valid_state <- function(state)
    is.character(state) && (length(state) == 1L) && state %in% valid_states()
