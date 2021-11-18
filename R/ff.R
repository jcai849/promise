pipe_fd <- function() {
    .Call(C_pipe_fd)
}
close_fd <- function(fd) {
    stopifnot(is.integer(fd), length(fd) == 1)
    .Call(C_close_fd, fd)
}
send <- function(fd, value) {
    stopifnot(is.integer(fd), length(fd) == 1)
    serialized <- serialize(value, NULL)
    .Call(C_send, fd, serialized)
}
fetch <- function(fd) {
    stopifnot(is.integer(fd), length(fd) == 1)
    msg <- .Call(C_fetch, fd)
    unserialize(msg)
}

register_then <- function(promise, then) {
    stopifnot(is.Promise(promise), is.ThenPromise(then))
    .Call(C_register_then, promise, then)
}
register_promise <- function(promise) {
    stopifnot(is.Promise(promise))
    .Call(C_register_promise, promise)
}
