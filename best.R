
best <- function(state, outcome) {
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospitals <- hospitals[hospitals[,'State'] == state,]
  
  if (nrow(hospitals) == 0) stop("invalid state")
  
  if (outcome == 'heart attack') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == 'heart failure') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == 'pneumonia') {
    cause <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else stop("invalid outcome")
  
  hospitals[,cause] <- as.numeric(hospitals[,cause])
  hospitals <- hospitals[order(hospitals[,'Hospital.Name']),]  
  hospitals <- hospitals[hospitals[,cause] == min(hospitals[,cause], na.rm = TRUE),]
  sort(hospitals[,'Hospital.Name'])[1]
}