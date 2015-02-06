pollutantmean <- function(directory, pollutant, id = 1:332) {
  holder <- c()
  for(i in id) {
    current <- read.csv(paste(c(directory, "/", sprintf("%03d", i), ".csv"), collapse=""))
    holder <- c(holder, current[[pollutant]])
  }
  round(mean(holder, na.rm = T), digits = 3)
}