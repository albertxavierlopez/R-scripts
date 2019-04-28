# Testing parallelizations 

library(doParallel)

getDoParWorkers() #testing how many cores are registered

n=100
t = Sys.time()
cl <- makeCluster(2)
registerDoParallel(cl)
foreach(i=1:n) %dopar% {
  sqrt(i) }
print(paste0(format(Sys.time()-t,units='hours')))


cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers() #testing how many cores are registered

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 100000
ptime <- system.time({
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime

