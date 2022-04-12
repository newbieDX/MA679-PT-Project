# functional pca

library(fda)
library(tidyverse)
library(plotly)

# Function to simulate data
fake_curves <- function(n_curves = 100, n_points = 80, max_time = 100){
  ID <- 1:n_curves
  x <- vector(mode = "list", length = n_curves)
  t <- vector(mode = "list", length = n_curves)
  
  for (i in 1:n_curves){
    t[i] <- list(sort(runif(n_points,0,max_time)))
    x[i] <- list(cumsum(rnorm(n_points)) / sqrt(n_points))
  }
  df <- tibble(ID,t,x)
  names(df) <- c("ID", "Time", "Curve")
  return(df)
}


set.seed(123)
n_curves <- 40
n_points <- 80
max_time <- 100
df <- fake_curves(n_curves = n_curves,n_points = n_points, max_time = max_time)
head(df)


# time series embedding
