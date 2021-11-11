promise <- function(executor) {
    resolve <- function(value) {
        sval <- serialize(value, NULL)
        .Call(C_resolve, sval)
    }
    .Call(C_promise, executor, resolve, new.env())
}

then <- function(promise, onFulfilled) {
    .Call(C_then, promise, onFulfilled, new.env())
}
