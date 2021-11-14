library(promise)

working <- "Working!"
p <- promise(
    function(resolve, reject){
        resolve(working)
        reject("How did I get here?")
})

print(p)

then(p,
     onFulfilled = print,
     onRejected = print)
