#http://librestats.com/2012/03/15/a-no-bs-guide-to-the-basics-of-parallelization-in-r/

library(parallel)

inputs <- 1:100

processInput <- function(i) {
  i * i
}

#sysctl hw.ncpu


numCores <- detectCores()

print(paste("numCores =", numCores))
mclapply(inputs, processInput)

system.time(mclapply(inputs, processInput,mc.cores = numCores))
system.time(sapply(inputs,processInput))
system.time(lapply(inputs,processInput))

test <- lapply(1:100,function(x) rnorm(10000))
system.time(x <- lapply(test,function(x) loess.smooth(x,x)))
system.time(x <- mclapply(test,function(x) loess.smooth(x,x), mc.cores=numCores))
