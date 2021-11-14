promise <- function(executor) {
    make_tail <- function(state) function(value) {
        result <- make_result(state, value)
        parallel:::sendMaster(result)
        q("no")
    }
    resolve <- make_tail("RESOLVED")
    reject <- make_tail("REJECTED")
    make_promise(job=mcparallel(executor(resolve, reject)),
                 state="PENDING", value=NULL, settled=FALSE)
}

then <- function(promise, onFulfilled, onRejected) {
    promise_result <- result(promise)
    fun <- if (resolved(promise_result)) { onFulfilled
              } else if (rejected(promise_result)) { onRejected
              } else stop("odd result")
    then_result <- fun(value(promise_result))
    if (!is.Promise(then_result)) {
        make_promise(job=NULL, state="RESOLVED",
                     value=then_result, settled=TRUE)
    } else then_result
}

make_promise <- function(job, state, value, settled) {
    stopifnot(valid_state(state))
    promise <- structure(new.env(parent=emptyenv()), class="Promise")
    job(promise) <- job
    state(promise) <- state
    value(promise) <- value
    settled(promise) <- settled
    promise
}

result <- function(x, ...) UseMethod("result")
result.Promise <- function(x, ...) {
    if (!settled(x)) {
        result <- mccollect(job(x))[[1]]
        settled(x) <- TRUE
        state(x) <- state(result)
        value(x) <- value(result)
    }
    x
}

job <- function(x, ...) UseMethod("job")
job.Promise <- function(x, ...) x$job
`job<-` <- function(x, value) UseMethod("job<-")
`job<-.Promise` <- function(x, value) {x$job <- value; x}

state <- function(x, ...) UseMethod("state")
state.Promise <- function(x, ...) x$state
`state<-` <- function(x, value) UseMethod("state<-")
`state<-.Promise` <- function(x, value) {x$state <- value; x}

value <- function(x, ...) UseMethod("value")
value.Promise <- function(x, ...) x$value
`value<-` <- function(x, value) UseMethod("value<-")
`value<-.Promise` <- function(x, value) {x$value <- value; x}

settled <- function(x, ...) UseMethod("settled")
settled.Promise <- function(x, ...) x$settled
`settled<-` <- function(x, value) UseMethod("settled<-")
`settled<-.Promise` <- function(x, value) {x$settled <- value; x}

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
