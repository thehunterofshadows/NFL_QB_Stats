#test going par
library(doSNOW)
library(foreach)
library(tictoc)

cl<-makeCluster(2) #change the 2 to your number of CPU cores
registerDoSNOW(cl)

tic("sleeping")
print("falling asleep...")
  myTest<- foreach(i=1:10000) %dopar% {
    
    25 ^ i
    
  }
print("waking up")
toc()

stopCluster(cl)