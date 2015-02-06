complete <- function(directory, id = 1:332) {
  holder <- c()
  for(i in id) {
    current <- read.csv(paste(c(directory, "/", sprintf("%03d", i), ".csv"), collapse=""))
    holder <- c(holder, sum(complete.cases(current)))
  }
  cbind(id,holder)
}