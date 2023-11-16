preprocess_numeric <- function(numeric,DataFrameID) {
 
  mean_data <- colMeans(numeric)
  variance_data <- apply(numeric, 2, sd)
  
  
  result_df <- data.frame(
    DataFrameID = rep(DataFrameID, each = length(mean_data) * 2),
    Statistic = rep(c("Mean", "Variance"), each = length(mean_data)),
    Variable = rep(colnames(numeric), times = 2),
    Value = c(mean_data, variance_data)
  )
  
  return(result_df)
}

