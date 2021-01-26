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
method = "NS"
type = "return"
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
window_length = 100
validation_window_length = 40
final_date = as.Date("2020-12-31")

###### Import data

# Volatilities
df <- read.table('./Data/Large_network/largenet_log_ret.csv',sep=",", header=TRUE)

# Format date column
df$Date <- as.Date(df$Date)
print(df$Date)

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

# Neighbourhood selection
#################### estimate the partial correlation matrix with various methods

# Parameters
n=nrow(filtered_df)
p=ncol(filtered_df)
alpha=1
l1=0.01*(sqrt(n)*qnorm(1-alpha/(2*p^2))) #*0.7
iter=3



result1=space.neighbor(data.matrix(filtered_df), lam1=l1, lam2=0)
print(result1)

estimated_partial_corr_matrix = result1$ParCor
print(estimated_partial_corr_matrix)

custom_mb = function(data,reg_param){
  result1=space.neighbor(data, lam1=reg_param, lam2=0)
  
}

# Hyperparameter tuning - penalty terms

out.mb = huge(data.matrix(filtered_df))

# model selection using ric or stars
out.select = huge.select(out.mb , criterion = "ric",
                          rep.num=10)

# Get optimal regularisation parameter
chosen_lambda = out.select$opt.lambda

# Rerun model with chosen lambda
result1=space.neighbor(data.matrix(filtered_df),
                       lam1=chosen_lambda, lam2=0)

estimated_partial_corr_matrix = result1$ParCor
print(estimated_partial_corr_matrix)



## Export results
# Name the file properly first
final_date_transformed = paste0(substr(final_date,1,4),"_",
                                substr(final_date,6,7),"_",
                                substr(final_date,9,10))
filename = paste0("./Data/Large_network/Estimated_networks/largenet_",method,"_",type,"_",final_date_transformed,"_window_size_",window_length,".csv")
write.csv(estimated_partial_corr_matrix,filename)


# PLACEHOLDER