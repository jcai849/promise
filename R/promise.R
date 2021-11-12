promise <- function(executor) {
    returnType <- function(state) function(value) {
        result <- make_result(state, value)
        parallel:::sendMaster(result)
        q("no")
    }
    resolve <- returnType("RESOLVED")
    reject <- returnType("REJECTED")
    make_promise(job=mcparallel(executor(resolve, reject)),
                 state="PENDING",
                 fulfillmentCallbacks=list(),
                 rejectionCallbacks=list())

}

then <- function(promise, onFulfilled, onRejected) {
    make_promise(job(promise),
                 state="PENDING",
                 c(fulfillmentCallbacks(promise), list(onFulfilled)),
                 c(rejectionCallbacks(promise), list(onRejected)))
}

make_promise <- function(job, state, fulfillmentCallbacks, rejectionCallbacks) {
    stopifnot(inherits(job, "parallelJob"),
              valid_state(state),
                  is.list(fulfillmentCallbacks) &&
                      all(sapply(fulfillmentCallbacks, is.function)),
                  is.list(rejectionCallbacks) &&
                      all(sapply(rejectionCallbacks, is.function)))
    structure(list(job=job,
                   state=state,
                   fulfillmentCallbacks=fulfillmentCallbacks,
                   rejectionCallbacks=rejectionCallbacks),
              class="Promise")
}

job <- function(x, ...) UseMethod("job")
job.Promise <- function(x, ...) x$job
state <- function(x, ...) UseMethod("state")
state.Promise <- function(x, ...) x$state
fulfillmentCallbacks <- function(x, ...) UseMethod("fulfillmentCallbacks")
fulfillmentCallbacks.Promise <- function(x, ...) x$fulfillmentCallbacks
rejectionCallbacks <- function(x, ...) UseMethod("rejectionCallbacks")
rejectionCallbacks.Promise <- function(x, ...) x$rejectionCallbacks
format.Promise <- function(x, ...)
    c("Promise:",
    format_member("state", state(x)))
format_member <- function(tag, value)
    paste0("\t", c(paste0(tag, ":"), paste0("\t", capture.output(str(value)))))
print.Promise <- str.Promise <- function(x, ...) cat(format(x), sep="\n")

make_result <- function(state, value) {
    stopifnot(valid_state(state),
              !missing(value))
    structure(list(state=state, value=value), class="Result")
}

state.Result <- function(x, ...) x$state
value <- function(x, ...) UseMethod("value")
value.Result <- function(x, ...) x$value

format.Result <- function(x, ...)
    c("Result:",
    format_member("State", state(x)),
    format_member("Value", value(x)))
print.Result <- str.Result <- function(x, ...) cat(format(x), sep="\n")

valid_states <- function() c("RESOLVED", "REJECTED", "PENDING")
valid_state <- function(state)
    is.character(state) && (length(state) == 1L) && state %in% valid_states()
