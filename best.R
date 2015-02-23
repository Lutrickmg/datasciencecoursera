best <- function(state, outcome) {
  bestest <- ''
  hospitals <- c()
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospitals <- data[data[,7] == state,]
  
  if (nrow(hospitals) == 0) stop("invalid state")
  
  if (outcome == 'heart attack') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    indexes <- !is.na(as.numeric(hospitals[,cause]))
    hospitals <- hospitals[indexes,]
  } else if (outcome == 'heart failure') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    indexes <- !is.na(as.numeric(hospitals[,cause]))
    hospitals <- hospitals[indexes,]
  } else if (outcome == 'pneumonia') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    indexes <- !is.na(as.numeric(hospitals[,cause]))
    hospitals <- hospitals[indexes,]
  } else stop("invalid outcome")
  
  minimum <- min(hospitals[,cause])
  
  hospitals <- hospitals[hospitals[,cause] == minimum,'Hospital.Name']
  hospitals <- sort(hospitals)[1]
  hospitals[1]
}