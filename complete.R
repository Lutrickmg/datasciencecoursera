complete <- function(directory, id = 1:332) {
  nobs <- c()
  for(i in id) {
    current <- read.csv(paste(c(directory, "/", sprintf("%03d", i), ".csv"), collapse=""))
    nobs <- c(nobs, sum(complete.cases(current)))
  }
  data.frame(id,nobs)
}