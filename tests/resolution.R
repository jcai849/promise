library(promise)

path = tempfile("testfile")
promise(
    function(resolve, reject){
        value <- "working!"
        placement <- file(path, "w+")
        cat(value, "\n", file=x)
        close(placement)
        resolve(value)
})

Sys.sleep(2)
placement <- file(path, "r")
readLines(placement)
