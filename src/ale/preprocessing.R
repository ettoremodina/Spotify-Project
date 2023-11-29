#load data and useful functions
source("preprocess_numeric.R")
source("normalize_numeric.R")
source("normalize_numeric_global.R")
dataset_list = readRDS("../data/data_list.RData")

#clean dataset
n_row <- numeric(length(dataset_list ))

for (i in seq_along(dataset_list )) {
  n_row[i] <- nrow(dataset_list [[i]])
}

# Function to filter out data frames with wrong nrow and crop the one too long
filter_nrow <- function(df) {
  nrow(df) != 3
}


filtered_list <- Filter(filter_nrow, dataset_list)
filtered_list[[26]] <- filtered_list[[26]][1:101, ]
dataset_list<- filtered_list

n_row <- numeric(length(dataset_list ))
for (i in seq_along(dataset_list )) {
  n_row[i] <- nrow(dataset_list [[i]])
}

# this chunk provides numeric data normalized in a local way
# option1

numeric_list <- list()
for (i in seq_along(dataset_list)) {
  data <- dataset_list[[i]]
  numeric_list[[i]] <- data[, c(5, 7:15, 18, 19)]
  numeric_list[[i]] <- normalize_numeric( numeric_list[[i]])
}

# this chunk provides numeric data normalized in a global way
# option2 
numeric_list <- list()
for (i in seq_along(dataset_list)) {
  data <- dataset_list[[i]]
  numeric_list[[i]] <- data[, c(5, 7:15, 18, 19)]
}
numeric_list <- normalize_numeric_global(numeric_list)

combined_numeric <- do.call(rbind, numeric_list)
combined_numeric <-round(combined_numeric,3)

temp <- rep(seq_along(n_row), times = n_row)
combined_numeric$id<-temp



#this chunk returns a dataframe with mean and variance for each variable
#of each dataframe contained in the input list

result_list <- list()
for (i in seq_along(dataset_list)) {
  result_list[[i]] <- preprocess_numeric( numeric_list[[i]], DataFrameID = i)
}



final_result_df <- do.call(rbind, result_list)
final_result_df$Value <- round(final_result_df$Value, 3)
view(final_result_df)

#use this to have a boxplot of the variables you want to visualize
#change this line with the number of the numeric dataset you want to open
#numeric<-numeric_list[[1]]

par(mfrow = c(2, 3))
numeric<-numeric_list[[45]]
col_names<-colnames(numeric)
boxplot(numeric[,1:2], col = "lightblue", main = "Boxplots", names = col_names[1:2], cex.axis = 0.5)
boxplot(numeric[,3:4], col = "lightblue", main = "Boxplots", names = col_names[3:4], cex.axis = 0.5)
boxplot(numeric[,5:6], col = "lightblue", main = "Boxplots", names = col_names[5:6], cex.axis = 0.5)
abline(h=c(0.5,0.8),col="red")
boxplot(numeric[,7:8], col = "lightblue", main = "Boxplots", names = col_names[7:8], cex.axis = 0.5)
boxplot(numeric[,9:10], col = "lightblue", main = "Boxplots", names = col_names[9:10], cex.axis = 0.5)
par(mfrow = c(1, 2))
boxplot(numeric[,11], col = "lightblue", main = "Duration", names = col_names[11], cex.axis = 0.5)
boxplot(numeric[,12], col = "lightblue", main = "Year", names = col_names[12], cex.axis = 0.5)



# let's try to have a clustering based on numeric variables only

cluster_subset <- combined_numeric[, -which(names(combined_numeric) == "id")]
scaled_data <- scale(cluster_subset )
k<-4
set.seed(123)  
kmeans_result <- kmeans(scaled_data, centers = k)
clustered <- cbind(combined_numeric, cluster = kmeans_result$cluster)
aggregate(cluster_subset, by = list(cluster = clustered$cluster), FUN = mean)





#factor list containing for each element a dataframe of factors
factor_list <- list()
for (i in seq_along(dataset_list)) {
  data <- dataset_list[[i]]
  factor_list[[i]] <- data[,c(2,3,4,6,16,17,20:ncol(data))]
}



#escluderei instrumentalness e liveness dal momento che quasi tutte le canzoni sono cantate e non da live

#per quanto riguarda factor
#-quante canzoni di stesso artista, 
#quante canzoni di stesso album,
#quante canzoni di stesso genere?
#key mode e tempo ???
