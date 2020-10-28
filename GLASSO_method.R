# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(corpcor)

# Import custom functions
source("common_functions.R")

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
#filtered_df = subset(filtered_df, final_date as.Date(Date))
filtered_df = df[start_date_loc:(final_date_loc-1),]
filtered_df = filtered_df[var_cols]

# Inspect the results
plot(filtered_df$MS)

# Standardize final dataset
filtered_df <- scale(filtered_df)


## METHOD 1: Base case (without factors)

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


## METHOD PART 2: Run combined model
  
  # Import factors
  five_factors_df <- read.table('./Data/Stock_prices/F-F_Research_Data_5_Factors_2x3_daily.CSV',sep=",", header=TRUE)

  ## parameters
  var_cols = c("Mkt.RF","SMB","HML","RMW","CMA","RF")
window_length = 150
final_date = "30/06/2020" 
final_date_loc = match(final_date,five_factors_df$Date)
start_date_loc = final_date_loc - window_length #final_date - window_length

  filtered_factor_df = five_factors_df[start_date_loc:(final_date_loc-1),]
filtered_factor_df = filtered_factor_df[var_cols]

  # Inspect the results
  plot(filtered_factor_df$SMB)
# Standardize final dataset
  filtered_factor_df <- scale(filtered_factor_df)

  # Combine arrays
  combined_filtered_df <- cbind(filtered_df,filtered_factor_df)

  # Get the sample covariance matrix of the data
  s <- var(combined_filtered_df)
rho_param <- 0.85

  # Run the algorithm
  result3 <-glasso(s, rho=rho_param)

  # Obtain the estimated inverse covariance matrix
  estim_inverse_partial_covmat = result3$wi

  # Get the partial correlation network from the inverse covariance matrix
  estimated_partial_corr_matrix = invcov2pcorr(estim_inverse_partial_covmat)

  
  print(estimated_partial_corr_matrix)

  # Hyperparameter tuning - penalty terms
  
  ## Export results
  write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/GLASSO_combined_factors.csv')
  
  
  ## METHOD PART 3: Model with residuals from factor regression
  
factor_residual_matrix <- get_residuals(filtered_df,filtered_factor_df)


factor_residual_matrix <- scale(factor_residual_matrix)
# Get the sample covariance matrix of the data
  s <- var(factor_residual_matrix)
rho_param <- 0.8

  # Run the algorithm
  result3 <-glasso(s, rho=rho_param)

  # Obtain the estimated inverse covariance matrix
  estim_inverse_partial_covmat = result3$wi

  # Get the partial correlation network from the inverse covariance matrix
  estimated_partial_corr_matrix = invcov2pcorr(estim_inverse_partial_covmat)

  print(estimated_partial_corr_matrix)


  # Hyperparameter tuning - penalty terms
  write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/GLASSO_factor_residuals.csv')


