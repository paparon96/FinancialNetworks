# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)

# Import custom functions
source("common_functions.R")

# Import data
df <- read.table('./Data/Stock_prices/log_returns_all_ts.csv',sep=",", header=TRUE)

## Global parameters
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
#N = dim(stock_df)[1]
window_length = 150
final_date = "30/06/2020"   #as.Date("2020-06-30")
final_date_loc = match(final_date,as.Date(df$Date))
final_date_loc = match(final_date,df$Date)

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




## METHOD 1: Base case (without factors)

## Run the model

# Neighbourhood selection
#################### estimate the partial correlation matrix with various methods
alpha=1
l1=(1/sqrt(n)*qnorm(1-alpha/(2*p^2)))*0.7
iter=3

n=nrow(filtered_df)
p=ncol(filtered_df)

result1=space.neighbor(data.matrix(filtered_df), lam1=l1, lam2=0)
print(result1)

estimated_partial_corr_matrix = result1$ParCor

# Hyperparameter tuning - penalty terms

## Export results
write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/NE.csv')


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

## Run the model

# Neighbourhood selection
#################### estimate the partial correlation matrix with various methods

n=nrow(combined_filtered_df)
p=ncol(combined_filtered_df)

alpha=1
l1=(1/sqrt(n)*qnorm(1-alpha/(2*p^2)))*0.7
iter=3



result1=space.neighbor(data.matrix(combined_filtered_df), lam1=l1, lam2=0)
print(result1)

estimated_partial_corr_matrix = result1$ParCor

# Hyperparameter tuning - penalty terms

## Export results
write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/NE_combined_factors.csv')


## METHOD PART 3: Model with residuals from factor regression

factor_residual_matrix <- get_residuals(filtered_df,filtered_factor_df)


factor_residual_matrix <- scale(factor_residual_matrix)

## Run the model

# Neighbourhood selection
#################### estimate the partial correlation matrix with various methods

n=nrow(factor_residual_matrix)
p=ncol(factor_residual_matrix)

alpha=1
l1=(1/sqrt(n)*qnorm(1-alpha/(2*p^2)))*0.7
iter=3



result1=space.neighbor(data.matrix(factor_residual_matrix), lam1=l1, lam2=0)
print(result1)

estimated_partial_corr_matrix = result1$ParCor

# Hyperparameter tuning - penalty terms

## Export results
write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/NE_factor_residuals.csv')