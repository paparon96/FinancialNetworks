# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(corpcor)

# Import data
df <- read.table('./Data/Stock_prices/log_returns_all_ts.csv',sep=",", header=TRUE)

## Global parameters
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
N = dim(stock_df)[1]
window_length = 150
final_date = as.Date("2020-06-30")
final_date_loc = match(final_date,as.Date(df$Date))
start_date_loc = final_date_loc - window_length #final_date - window_length

# Filter dataset based on the dates
#filtered_df = subset(df,as.Date(Date) >= start_date)
#filtered_df = subset(filtered_df, final_date > as.Date(Date))
filtered_df = df[start_date_loc:(final_date_loc-1),]
filtered_df = filtered_df[var_cols]

# Inspect the results
plot(filtered_df$MS)

# Standardize final dataset
filtered_df <- scale(filtered_df)


## Run the model

# GLASSO method
#################### estimate the partial correlation matrix with various methods

# Parameters

# Get the sample covariance matrix of the data
s <- var(filtered_df)
rho_param <- .01

# Run the algorithm
result3 <-glasso(s, rho=rho_param)

# Obtain the estimated covariance matrix
estim_covmat = result3$w

## Transform the covariance matrix into a partial correlation matrix
estimated_partial_corr_matrix = cor2pcor(estim_covmat)

print(estimated_partial_corr_matrix)

# Hyperparameter tuning - penalty terms

## Export results
write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/GLASSO.csv')


