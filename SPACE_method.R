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

# SPACE method
#################### estimate the partial correlation matrix with various methods

# Parameters
n=nrow(filtered_df)
p=ncol(filtered_df)
alpha=1
l1=(1/sqrt(n)*qnorm(1-alpha/(2*p^2)))*0.7
iter=3

result2=space.joint(data.matrix(filtered_df), lam1=l1*n*1.56, lam2=0, iter=iter)
print(result2)

estimated_partial_corr_matrix = result2$ParCor

# Hyperparameter tuning - penalty terms

## Export results
write.csv(estimated_partial_corr_matrix,'./Data/Estimated_networks/SPACE.csv')


