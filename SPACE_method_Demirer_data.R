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
method = "SPACE"
type = "volatility"
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
window_length = 150
validation_window_length = 40
final_date = as.Date("2008-09-15")

###### Import data

# Volatilities
df <- read.table('./Data/Stock_prices/Demirer2017_dataset.csv',sep=";", header=TRUE)

# Format date column
df$Date <- as.Date( format( as.Date( df$dd.mm.yy, format = "%d/%m/%y" ), "%Y-%m-%d"))
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
plot(filtered_df$jpm)

# Get only the relevant columns
cols_to_drop = c('Date','dd.mm.yy')
filtered_df = filtered_df[ , !(names(filtered_df) %in% cols_to_drop)]


validation_df = validation_df[ , !(names(validation_df) %in% cols_to_drop)]


# Standardize final dataset
filtered_df <- scale(filtered_df)
validation_df <- scale(validation_df)




## METHOD 1: Base case (without factors)

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
custom_space = function(data,lambda){
  path <-  lapply(seq(length(lambda)), function(i) {
    tmp <- space.joint(data, lam1=i, lam2=0)
    tmp <- tmp$ParCor
  })
  my_list = list("path" = path)
  return(my_list)
}

lmax=(sqrt(n)*qnorm(1-alpha/(2*p^2)))
lams <- getLamPath(lmax, lmax*.05, len=20)

spaceargs <- list(lambda=lams)
out.space <- pulsar(data.matrix(filtered_df),
                    fun=custom_space, fargs=spaceargs, rep.num=10,
                    criterion='stars', lb.stars=TRUE, ub.stars=TRUE)

# Get optimal lambda
chosen_lambda = lams[out.space$stars$opt.index]

# Refit with optimal lambda
result2=space.joint(data.matrix(filtered_df),
                    lam1=chosen_lambda, lam2=0)

estimated_partial_corr_matrix = result2$ParCor
print(estimated_partial_corr_matrix)

## Export results
# Name the file properly first
final_date_transformed = paste0(substr(final_date,1,4),"_",
                                substr(final_date,6,7),"_",
                                substr(final_date,9,10))
filename = paste0("./Data/Estimated_networks/Demirer_",method,"_",type,"_",final_date_transformed,".csv")
write.csv(estimated_partial_corr_matrix,filename)


# PLACEHOLDER