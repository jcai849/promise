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

make_result <- function(state, value) {
    stopifnot(valid_state(state), !missing(value))
    structure(list(state=state, value=value), class="Result")
}

state.Result <- function(x, ...) x$state
value.Result <- function(x, ...) x$value
resolved.Result <- function(x, ...) identical(state(x), "RES")
rejected.Result <- function(x, ...) identical(state(x), "REJ")
pending.Result <-  function(x, ...) identical(state(x), "PND")

format.Result <- function(x, ...)
    c("Result:",
    format_member("State", state(x)),
    format_member("Value", value(x)))
print.Result <- str.Result <- function(x, ...) cat(format(x), sep="\n")
