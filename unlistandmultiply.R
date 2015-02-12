getmoney <- function(list) {
  ((unlist(list[1])%*%unlist(list[2]))*(unlist(list[3])%*%unlist(list[4])))[1,1]
}
getrev <- function(list) {
  rev <- 0
  for(item in list) {rev <- rev + getmoney(item)}
  rev
}