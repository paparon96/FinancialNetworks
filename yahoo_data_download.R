LoadLibraries()

# Set time interval
from_date = "2010-01-02"
to_date = "2020-12-30"

# Import bank names
banks <- as.data.frame( read_excel( "Data/Stock_prices/Bank_tickers.xlsx" ))
banks <- drop_na( banks )

# List all days in the specified time interval
Date <- as.Date( as.Date( from_date ):as.Date( to_date ), origin = "1970-01-01" )

# Create empty data frame with row indices
stock_prices <- data.frame( Date, row.names = Date )

# Dowload stock prices
for( i in 1:nrow(banks[ "Yahoo Finance Ticker" ])){
  
  ticker <- banks[ i , "Yahoo Finance Ticker" ]
  print( ticker )
  prices <- quantmod::getSymbols( ticker, src = "yahoo", from = from_date, 
                                       to = to_date, auto.assign = FALSE )[ , 6 ]
  colnames( prices ) <- ticker
  stock_prices <- merge( stock_prices, prices[ , 1 ], by = 0, all.x = TRUE )
  stock_prices <- subset( stock_prices, select = -c( Row.names ))
  rownames( stock_prices ) <- Date
}


# Find rows with NAs for all banks
ind <- apply( subset( stock_prices, select = -c( Date )), 1, function( x ) all( is.na( x )))
stock_prices_weekdays <- stock_prices[ !ind, ]
write.csv( stock_prices_weekdays, "Data/Stock_prices/Demirer_stock_prices.csv", row.names = FALSE )
