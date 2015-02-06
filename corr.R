corr <- function(directory, threshold = 0, id = 1:332) {
  holder <- c()
  corr <- numeric()
  for(i in id) {
    current <- read.csv(paste(c(directory, "/", sprintf("%03d", i), ".csv"), collapse=""))
    comcases <- sum(complete.cases(current))
    if(comcases > threshold) {
      cases <- current[complete.cases(current),]
      corr <- c(corr, cor(cases['nitrate'], cases['sulfate']))
    }
  }
  corr
}