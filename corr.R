corr <- function(directory, threshold = 0, id = 1:332) {
  corr <- numeric()
  for(i in id) {
    current <- read.csv(paste(c(directory, "/", sprintf("%03d", i), ".csv"), collapse=""))
    comcases <- complete.cases(current)
    if(sum(comcases) > threshold) {
      cases <- current[comcases, c('nitrate', 'sulfate')]
      corr <- c(corr, cor(cases['nitrate'], cases['sulfate']))
    }
  }
  corr
}