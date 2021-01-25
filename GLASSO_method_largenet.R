# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(pulsar)
library(huge)
#library(devtools)
#library(SpiecEasi)

# Import custom functions
source("common_functions.R")

## Global parameters
method = "GLASSO"
type = "return"
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
window_length = 150
validation_window_length = 40
final_date = as.Date("2020-12-31")
rho_hyperparam = 0.25

###### Import data

# Volatilities
df <- read.table('./Data/Large_network/largenet_log_ret.csv',sep=",", header=TRUE)

# Format date column
df$Date <- as.Date(df$Date)


# Filter dataset based on the dates
filtered_df = subset(df, final_date >= as.Date(Date))
start_date = tail(filtered_df$Date,n=window_length)[1]
filtered_df = subset(filtered_df,as.Date(Date) >= start_date)
print(filtered_df$Date)

# Get validation dataset
validation_df = subset(df,as.Date(Date) > final_date)
validation_final_date = head(validation_df$Date,n=validation_window_length)[validation_window_length]
validation_df = subset(validation_df,validation_final_date >= as.Date(Date))

# Inspect the results
plot(filtered_df$JPM)

# Get only the relevant columns
cols_to_drop = c('Date')
filtered_df = filtered_df[ , !(names(filtered_df) %in% cols_to_drop)]


validation_df = validation_df[ , !(names(validation_df) %in% cols_to_drop)]


# Standardize final dataset
filtered_df <- scale(filtered_df)
validation_df <- scale(validation_df)


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
out.glasso = huge(data.matrix(filtered_df), method = "glasso")

# model selection using ric or stars
out.select = huge.select(out.glasso , criterion = "ric",
                         rep.num=10)

# Get optimal regularisation parameter
chosen_rho = out.select$opt.lambda

# Rerun model with chosen lambda
selected_glasso_entries = huge(data.matrix(filtered_df),
                               method = "glasso",
                               lambda = chosen_rho)

selected_glasso_entries = selected_glasso_entries$path[[1]]

result1 <-glasso(s, rho=chosen_rho)

# Obtain the estimated inverse covariance matrix
estim_inverse_partial_covmat = result1$wi

# Get the partial correlation network from the inverse covariance matrix
estimated_partial_corr_matrix = invcov2pcorr(estim_inverse_partial_covmat)

# Initialize empty matrix and fill up with selected elements
n = dim(estimated_partial_corr_matrix)[1]
d = dim(estimated_partial_corr_matrix)[2]
m = matrix(0, nrow = n,
           ncol = d) 
for (i in 1:n){
  for (j in 1:d){
    
    if (i != j & selected_glasso_entries[i,j] != 0 )
      
      m[i,j] = estimated_partial_corr_matrix[i,j]
  }
}

# Set diagonal entries
diag(m) = 1
estimated_partial_corr_matrix = m



## Export results
# Name the file properly first
final_date_transformed = paste0(substr(final_date,1,4),"_",
                                substr(final_date,6,7),"_",
                                substr(final_date,9,10))
filename = paste0("./Data/Large_network/Estimated_networks/largenet_",method,"_",type,"_",final_date_transformed,".csv")
write.csv(estimated_partial_corr_matrix,filename)


# PLACEHOLDER