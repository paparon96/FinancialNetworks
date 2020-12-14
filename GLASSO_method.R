# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(corpcor)

# Import custom functions
source("common_functions.R")

## Global parameters
method = "GLASSO"
type = "return"
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
window_length = 150
final_date = as.Date("2020-03-16")
rho_hyperparam = 0.25

###### Import data

# Returns
return_df <- read.table('./Data/Stock_prices/log_returns_all_ts.csv',sep=",", header=TRUE)

# Format date column
return_df$Date <- as.Date( format( as.Date( return_df$Date, format = "%d/%m/%Y" ), "%Y-%m-%d"))

# Volatilities #############
vol <- list()
vol$data <- list()
vol$data$high_low <- cbind( as.data.frame( read.csv( "Data/Stock_prices/high_price.csv", header = TRUE, sep = "," ) ),
                            as.data.frame( read.csv( "Data/Stock_prices/low_price.csv", header = TRUE, sep = "," ) )[ , -1 ] )
colnames( vol$data$high_low )[ 1 ] <- "Date"
vol$data$high_low$Date <- as.Date( format( as.Date( vol$data$high_low$Date, format = "%d/%m/%Y" ), "%Y-%m-%d"))
risk_factors_5 <- as.data.frame( read.csv( "Data/Stock_prices/F-F_Research_Data_5_Factors_2x3_daily.CSV", header = TRUE, sep = "," ))

# Calculating the annualized daily percent standard deviation
vol$data$volatility <- as.data.frame( matrix( nrow = nrow( vol$data$high_low ), ncol = ncol( vol$data$high_low[ , -1 ] ) / 2 + 1 ) )
vol$data$volatility[ , 1 ] <- vol$data$high_low$Date
for( i in 1:10 ){
  vol$data$volatility[ , i + 1 ] <- log( 100 * sqrt( 365 * 0.361 * ( log( vol$data$high_low[ , i + 1 ] ) -  log( vol$data$high_low[ , i + 11 ] ) )^2 ) )
}

colnames( vol$data$volatility ) <- colnames( vol$data$high_low )[ 1:11 ]

volatility_df = vol$data$volatility
#################

# Get modelling dataframe based on the current modelling type
if (type=="return"){
  
  df = return_df
} else if (type=="volatility"){
  
  df = volatility_df
} else{
  print("Invalid modelling type defined, choose from 'return' or 'volatility'!")
  
}

# Filter dataset based on the dates
filtered_df = subset(df, final_date >= as.Date(Date))
start_date = tail(filtered_df$Date,n=window_length)[1]
filtered_df = subset(filtered_df,as.Date(Date) >= start_date)

# Inspect the results
plot(filtered_df$MS)

# Get only the relevant columns
filtered_df = filtered_df[var_cols]

# Standardize final dataset
filtered_df <- scale(filtered_df)


## METHOD 1: Base case (without factors)

## Run the model

# GLASSO method
#################### estimate the partial correlation matrix with various methods

# Parameters

# Get the sample covariance matrix of the data
s <- var(filtered_df)
rho_param <- rho_hyperparam

# Run the algorithm
result3 <-glasso(s, rho=rho_param)

# Obtain the estimated covariance matrix
estim_covmat = result3$wi

## Transform the covariance matrix into a partial correlation matrix
estimated_partial_corr_matrix = invcov2pcorr(estim_covmat)

print(estimated_partial_corr_matrix)

# Hyperparameter tuning - penalty terms

## Export results
#write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/GLASSO.csv')
# Name the file properly first
final_date_transformed = paste0(substr(final_date,1,4),"_",
                                substr(final_date,6,7),"_",
                                substr(final_date,9,10))
filename = paste0("./Data/Estimated_networks/",method,"_",type,"_",final_date_transformed,".csv")
write.csv(estimated_partial_corr_matrix,filename)


## METHOD PART 2: Run combined model
  
# Import factors
five_factors_df <- read.table('./Data/Stock_prices/F-F_Research_Data_5_Factors_2x3_daily.CSV',sep=",", header=TRUE)

# Format date column
five_factors_df$Date <- as.Date( format( as.Date( five_factors_df$Date, format = "%d/%m/%Y" ), "%Y-%m-%d"))

## parameters
factor_var_cols = c("Mkt.RF","SMB","HML","RMW","CMA","RF")

# Filter dataset based on the dates
filtered_factor_df = subset(five_factors_df, final_date >= as.Date(Date))
#start_date = tail(filtered_factor_df$Date,n=window_length)[1]
filtered_factor_df = subset(filtered_factor_df,as.Date(Date) >= start_date)

# Get only the relevant columns
filtered_factor_df = filtered_factor_df[factor_var_cols]

# Inspect the results
plot(filtered_factor_df$SMB)

# Standardize final dataset
filtered_factor_df <- scale(filtered_factor_df)

# Combine arrays
combined_filtered_df <- cbind(filtered_df,filtered_factor_df)

  # Get the sample covariance matrix of the data
  s <- var(combined_filtered_df)
rho_param <- rho_hyperparam #0.01 #0.85

  # Run the algorithm
  result3 <-glasso(s, rho=rho_param)

  # Obtain the estimated inverse covariance matrix
  estim_inverse_partial_covmat = result3$wi

  # Get the partial correlation network from the inverse covariance matrix
  estimated_partial_corr_matrix = invcov2pcorr(estim_inverse_partial_covmat)

  
  print(estimated_partial_corr_matrix)

  # Hyperparameter tuning - penalty terms
  
  ## Export results
  filename = paste0("./Data/Estimated_networks/",method,"_",type,"_combined_factors_",final_date_transformed,".csv")
  write.csv(estimated_partial_corr_matrix,filename)
  
  
  ## METHOD PART 3: Model with residuals from factor regression
  
factor_residual_matrix <- get_residuals(filtered_df,filtered_factor_df)


factor_residual_matrix <- scale(factor_residual_matrix)
# Get the sample covariance matrix of the data
  s <- var(factor_residual_matrix)
rho_param <- rho_hyperparam

  # Run the algorithm
  result3 <-glasso(s, rho=rho_param)

  # Obtain the estimated inverse covariance matrix
  estim_inverse_partial_covmat = result3$wi

  # Get the partial correlation network from the inverse covariance matrix
  estimated_partial_corr_matrix = invcov2pcorr(estim_inverse_partial_covmat)

  print(estimated_partial_corr_matrix)


  # Hyperparameter tuning - penalty terms
  filename = paste0("./Data/Estimated_networks/",method,"_",type,"_factor_resid_",final_date_transformed,".csv")
  write.csv(estimated_partial_corr_matrix,filename)


