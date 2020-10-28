#Load libraries
LoadLibraries()

vol <- list()

# Loading data
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
vol$data$volatility_risk_factors <- as.data.frame( cbind( vol$data$volatility[ 2500, ], risk_factors_5[ , -1 ] ))

# Build VAR model
vol$static <- list()
vol$static$var <- VAR( vol$data$volatility[ , -1 ], p = 2, type = "const" )

# Total spillover table
vol$static$spill_table <- spilloverDY12( vol$static$var, n.ahead = 10, no.corr = F )
print( vol$static$spill_table )


# Rolling spillover
vol$dynamic <- list()
vol$dynamic$rolling_spill <- spilloverRollingDY12( vol$data$volatility[ , -1 ], n.ahead = 10, 
                                                 no.corr = F, "VAR", 
                                                 params_est = list( p = 2, type = "const" ), 
                                                 window = 200 )


# Extracting spills from spills table
vol$dynamic$rolling_spill <- CreateRollingDataFrame( vol$dynamic$rolling_spill, F )

vol$dynamic$rolling_spill$Date <- as.Date( vol$data$high_low$Date[ 200:( nrow( vol$dynamic$rolling_spill ) + 199 ) ], format = "%d/%m/%Y" )

par( mar = c( 5, 5, 3, 3 ), mfrow = c( 1, 1 ), cex.axis = 1 ,cex.lab = 1.2, family = "A" )
plot( vol$dynamic$rolling_spill$Date, vol$dynamic$rolling_spill$Total, type = "l", lwd = 2, col = "black",
      xlab = "Time", ylab = "Total spillover index", 
      main = "Log volatility" )
axis( side = 4 )

# Add risk factors

# Run regressions and save resids

vol$factors <- list()
vol$factors$resid <- lapply( vol$data$volatility[ , -1 ], function( y ) print( resid( lm( y ~ risk_factors_5$Mkt.RF + risk_factors_5$SMB +
                                                                                                    risk_factors_5$HML + risk_factors_5$RMW + risk_factors_5$CMA +
                                                                                                    risk_factors_5$RF ))))
vol$factors$resid <- as.data.frame( lapply( vol$data$volatility[ , -1 ], function( y ) resid( lm( y ~ risk_factors_5$Mkt.RF + risk_factors_5$SMB +
                                                                                     risk_factors_5$HML + risk_factors_5$RMW + risk_factors_5$CMA +
                                                                                     risk_factors_5$RF ))))
colnames( vol$factors$resid ) <- colnames( vol$data$volatility[ , -1 ] )
vol$factors$resid$Date <- vol$data$volatility$Date
