# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(pulsar)
library(huge)
library(sparsebn)
#library(devtools)
#library(SpiecEasi)

# Import custom functions
source("common_functions.R")

## Global parameters
method = "DAG"
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

# SPARSEBN - DAG method
#################### estimate the partial correlation matrix with various methods

# Parameters
dat <- sparsebnData(filtered_df, type = "c")

# Run the algorithm
dags = estimate.dag(dat)
dags.fit <- estimate.parameters(dags, data = dat)

# Get adjacency matrix of one particular solution from the solution path
A = dags.fit[[10]]$coefs

# Convert sparse matrix into regular matrix format
estimated_network = as.matrix(A)


## Hyperparameter-tuning - penalty term
custom_dag = function(data,lambda){
  path <-  lapply(seq(length(lambda)), function(i) {
    
    print(i)
    
    dat <- sparsebnData(data, type = "c")
    
    # Run the algorithm
    dags = estimate.dag(dat)
    dags.fit <- estimate.parameters(dags, data = dat)
    
    tmp <- as.matrix(dags.fit[[i]]$coefs)
  })
  my_list = list("path" = path)
  return(my_list)
}

dagargs = seq(from=length(dags.fit)-1,to=1,by=-1)
dagargs <- list(lambda=dagargs)

out.dag <- pulsar(filtered_df,
                  fun=custom_dag, fargs=dagargs, rep.num=10,
                  criterion='stars', lb.stars=TRUE, ub.stars=TRUE)

# Get optimal lambda
chosen_lambda = out.dag$stars$opt.index

# Refit with optimal lambda
A = dags.fit[[chosen_lambda]]$coefs

# Convert sparse matrix into regular matrix format
estimated_network = as.matrix(A)

print(estimated_network)



## Export results
# Name the file properly first
final_date_transformed = paste0(substr(final_date,1,4),"_",
                                substr(final_date,6,7),"_",
                                substr(final_date,9,10))
filename = paste0("./Data/Estimated_networks/Demirer_",method,"_",type,"_",final_date_transformed,".csv")
write.csv(estimated_network,filename)


# PLACEHOLDER