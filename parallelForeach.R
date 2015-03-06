library(foreach)
library(doParallel)
#install.packages('doParallel')
#install.packages('doParallel')
library(parallel)

numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

inputs <- 1:100

processInput <- function(i) {
  i * i
}

results <- foreach(i=inputs) %dopar% {
  processInput(i)
}
system.time(foreach(i=inputs) %dopar% {
  processInput(i)
})
