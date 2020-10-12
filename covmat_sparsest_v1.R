# Import packages
library(space)
library(glasso)
library(dplyr)
library(ggplot2)


# Set working directory
setwd("/Research/FinNetworks")

# Import data
JPM <- read.table('./Data/Stock_prices/JPM.csv',sep=",")
JPM$Date <- as.Date(as.character(JPM$V1) , format = "%y-%m-%d")

MS <- read.table('./Data/Stock_prices/MS.csv',sep=",")
BAC <- read.table('./Data/Stock_prices/BAC.csv',sep=",")
C <- read.table('./Data/Stock_prices/C.csv',sep=",")
WFC <- read.table('./Data/Stock_prices/WFC.csv',sep=",")
GS <- read.table('./Data/Stock_prices/GS.csv',sep=",")
USB <- read.table('./Data/Stock_prices/USB.csv',sep=",")
TD <- read.table('./Data/Stock_prices/TD.csv',sep=",")
BK <- read.table('./Data/Stock_prices/BK.csv',sep=",")
TFC <- read.table('./Data/Stock_prices/TFC.csv',sep=",")


# Preprocess data
stock_df <- data.frame(MS$V1,
                       as.numeric(as.character(MS$V6)),as.numeric(as.character(JPM$V6)),
                       as.numeric(as.character(BAC$V6)),as.numeric(as.character(C$V6)),as.numeric(as.character(WFC$V6)),
                       as.numeric(as.character(GS$V6)),as.numeric(as.character(USB$V6)),as.numeric(as.character(TD$V6)),
                       as.numeric(as.character(BK$V6)),as.numeric(as.character(TFC$V6)))

# Rename columns
names(stock_df) = c("Date","MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")

# Sample plotting
#plot(stock_df$Date,stock_df$MS)

# Create log return columns
stock_df <- stock_df %>%
  mutate(lag_1 = lead(MS,1),
         MS = log(MS/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(JPM),
         JPM = log(JPM/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(BAC),
         BAC = log(BAC/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(C),
         C = log(C/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(WFC),
         WFC = log(WFC/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(GS),
         GS = log(GS/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(USB),
         USB = log(USB/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(TD),
         TD = log(TD/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(BK),
         BK = log(BK/lag_1)*100) %>% select(-c('lag_1'))

stock_df <- stock_df %>%
  mutate(lag_1 = lead(TFC),
         TFC = log(TFC/lag_1)*100) %>% select(-c('lag_1'))

# Drop first row
stock_df <- stock_df[2:2519,]

# Remove rows with NA values

stock_df = stock_df[complete.cases(stock_df), ]

n=nrow(stock_df)
p=ncol(stock_df)

### Export dataset
write.csv(stock_df,'./Data/Stock_prices/log_returns_all_ts.csv')


################
# Different network estimation methods

## Global parameters
var_cols = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")
N = dim(stock_df)[1]
window_length = 150



## 1. Neighbourhood selection
#################### estimate the partial correlation matrix with various methods
alpha=1
l1=1/sqrt(n)*qnorm(1-alpha/(2*p^2))
iter=3

result1=space.neighbor(data.matrix(stock_df[N-window_length:N,var_cols]), lam1=l1*0.7, lam2=0)
print(result1)


#### 2. Joint method with no weight (SPACE)
result2=space.joint(data.matrix(stock_df[N-window_length:N,var_cols]), lam1=l1*n*1.56, lam2=0, iter=iter)
print(result2)


#### 3. GLASSO

# Get the covariance matrix of the data
s <- var(data.matrix(stock_df[N-window_length:N,var_cols]))

# Run GLASSO
result3 <-glasso(s, rho=.01)

print(result3)
