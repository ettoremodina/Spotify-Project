normalize_numeric <- function(numeric) {
  numeric[[11]] <- numeric[[11]] / (1000 * 60)
  duration <- numeric[, 11]
  year <- numeric[, 1]
  numeric <- numeric[, -c(1, 11)]
  
  min_max_normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  numeric <- apply(numeric, 2, min_max_normalize)
  numeric <- round(numeric, 3)
  numeric <- as.data.frame(numeric)
  numeric$duration <- duration
  numeric$year <- year
  
  return(numeric)
}
