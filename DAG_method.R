# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)
library(corpcor)
library(sparsebn)
library(pulsar)
library(huge)

# Import custom functions
source("common_functions.R")

## Global parameters
method = "DAG"
type = "return"
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
window_length = 150
final_date = as.Date("2020-03-16")

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

dagargs = seq(from=length(dags.fit),to=1,by=-1)
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
filename = paste0("./Data/Estimated_networks/",method,"_",type,"_",final_date_transformed,".csv")
write.csv(estimated_network,filename)


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

# Parameters
dat <- sparsebnData(combined_filtered_df, type = "c")

# Run the algorithm
dags = estimate.dag(dat)
dags.fit <- estimate.parameters(dags, data = dat)

# Get adjacency matrix of one particular solution from the solution path
A = dags.fit[[10]]$coefs

# Convert sparse matrix into regular matrix format
estimated_network = as.matrix(A)

## Hyperparameter-tuning - penalty term
dagargs = seq(from=length(dags.fit),to=1,by=-1)
dagargs <- list(lambda=dagargs)

out.dag <- pulsar(combined_filtered_df,
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
filename = paste0("./Data/Estimated_networks/",method,"_",type,"_combined_factors_",final_date_transformed,".csv")
write.csv(estimated_network,filename)



## METHOD PART 3: Model with residuals from factor regression

factor_residual_matrix <- get_residuals(filtered_df,filtered_factor_df)


factor_residual_matrix <- scale(factor_residual_matrix)


# Parameters
dat <- sparsebnData(factor_residual_matrix, type = "c")

# Run the algorithm
dags = estimate.dag(dat)
dags.fit <- estimate.parameters(dags, data = dat)

# Get adjacency matrix of one particular solution from the solution path
A = dags.fit[[10]]$coefs

# Convert sparse matrix into regular matrix format
estimated_network = as.matrix(A)


## Hyperparameter-tuning - penalty term
dagargs = seq(from=length(dags.fit),to=1,by=-1)
dagargs <- list(lambda=dagargs)

out.dag <- pulsar(factor_residual_matrix,
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
filename = paste0("./Data/Estimated_networks/",method,"_",type,"_factor_resid_",final_date_transformed,".csv")
write.csv(estimated_network,filename)
