library(dplyr)
library(ggplot2)

data(longley)

dataset1 <- longley

# Normalizattion of the dataframe to range [-1,1]
variable_normalization <- function(dataset){
  mins <- apply(dataset, 2, max_cols)
  maxs <- apply(dataset, 2, min_cols)
  means <- apply(dataset, 2, mean_cols)
  
  
  for(j in 1:ncol(dataset)){
    for(i in 1:nrow(dataset)){
      dataset[i,j] = (dataset[i,j] - means[j]) / (maxs[j] - mins[j])
    }
  }
  
  dataset
  #apply(dataset, 2, normalize_cols, min=mins,  max = maxs, mean = means)
}

# Gets the maximun value of each column.
max_cols <- function(x){
  max(x)
}

# Gets the min value of each column.
min_cols <- function(x){
  min(x)
}

# Gets the mean of each column.
mean_cols <- function(x){
  mean(x)
}

# Transform 
normalize_cols <- function(x, min, max, mean){
  
}

dataset1 <- variable_normalization(dataset1)

