# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(corpcor)
library(sparsebn)

# Import custom functions
source("common_functions.R")

# Import data
df <- read.table('./Data/Stock_prices/log_returns_all_ts.csv',sep=",", header=TRUE)

## Global parameters
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
N = dim(stock_df)[1]
window_length = 150
#final_date = as.Date("2020-06-30")
final_date = "30/06/2020"   #as.Date("2020-06-30")
final_date_loc = match(final_date,as.Date(df$Date))
final_date_loc = match(final_date,df$Date)

start_date_loc = final_date_loc - window_length #final_date - window_length

# Filter dataset based on the dates
#filtered_df = subset(df,as.Date(Date) >= start_date)
#filtered_df = subset(filtered_df, final_date as.Date(Date))
filtered_df = df[start_date_loc:(final_date_loc-1),]
filtered_df = filtered_df[var_cols]

# Inspect the results
plot(filtered_df$MS)

# Standardize final dataset
filtered_df <- scale(filtered_df)


## METHOD 1: Base case (without factors)

## Run the model

# SPARSEBN - DAG method
#################### estimate the partial correlation matrix with various methods

# Parameters
dat <- sparsebnData(filtered_df, type = "c")
dags = estimate.dag(dat)

dags.fit <- estimate.parameters(dags, data = dat)

# Get adjacency matrix of one particular solution from the solution path
#A <- as.matrix(get.adjacency.matrix(data_dag[[10]]))

A = dags.fit[[10]]$coefs
A = as.data.frame(summary(A))

## Export results
write.csv(A,'./Data/Estimated_networks/DAG_sparsebn.csv')
