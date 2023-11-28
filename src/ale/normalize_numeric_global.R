normalize_numeric_global<- function(list_of_dataframes) {
  
  combined_df <- do.call(rbind, list_of_dataframes)
  
  # Calculate min and max values for each column across all dataframes
  min_values <- apply(combined_df, 2, min)
  max_values <- apply(combined_df, 2, max)
  
  # Function for min/max normalization
  min_max_normalize <- function(x, min_val, max_val) {
    return((x - min_val) / (max_val - min_val))
  }
  
  # Normalize each dataframe
  normalized_list <- lapply(list_of_dataframes, function(df) {
    #df[, 8] <- apply(df[8], 1, abs)
    df[[11]] <- df[[11]] / (1000 * 60)
    duration <- df[, 11]
    year <- df[, 1]
    df <-df[, -c(1, 11)]
    normalized_df <- df
    for (col in colnames(df)) {
      normalized_df[, col] <- min_max_normalize(df[, col], min_values[col], max_values[col])
    }
    normalized_df$duration <- duration
    normalized_df$year <- year
    
    return(normalized_df)
  })
  
  return(normalized_list)
}


