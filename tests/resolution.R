library(promise)

working <- "Working!"
p <- promise(
    function(resolve, reject){
        resolve(working)
        reject("How did I get here?")
})
print(p)
print(parallel::mccollect(p$job))
