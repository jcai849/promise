promise <- function(executor)
	.Call(C_promise, quote(executor(resolve=.Call(C_resolve, value))),
	      		 new.env())
