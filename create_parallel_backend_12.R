library(parallel)
library(foreach)

#n.cores <- parallel::detectCores() - 1
#n.cores <- 3
n.cores <- 12

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
  )
  
doParallel::registerDoParallel(cl = my.cluster)

