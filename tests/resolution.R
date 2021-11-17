library(promise)

working <- "Working!"
p <- promise(
        function(resolve, reject){
            parallel::mcparallel({
                    resolve(working)
                    reject("How did I get here?")
                }, detached = TRUE)
    })

print(p)

pt <- then(p,
         onFulfilled = print,
         onRejected = print)

ptt <- then(pt,
            onFulfilled = function(value)
                promise(function(reject, resolve)
                    resolve(print(rep(value, 3)))),
            onRejected = NULL)
