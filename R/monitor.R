set_sleep <- function(timeout = 0.2) {
    stopifnot(is.numeric(timeout), length(timeout) == 1)
    .Call("C_set_sleep", timeout)
}

stop_monitoring <- function() {
    .Call("C_stop_monitoring")
}

start_monitoring <- function() {
    .Call("C_start_monitoring")
}

.onLoad <- function(libname, pkgname) {
    invisible(start_monitoring())
}
