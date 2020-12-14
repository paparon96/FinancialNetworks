#Load libraries
LoadLibraries()

return <- list()

# Loading data
return$data <- list()
return$data$return <- as.data.frame( read.csv( "Data/Stock_prices/log_returns_all_ts.csv", header = TRUE, sep = "," ) )
return$data$return$Date <- format( as.Date( return$data$return$Date, format = "%d/%m/%Y" ), "%Y-%m-%d" )
risk_factors_5 <- as.data.frame( read.csv( "Data/Stock_prices/F-F_Research_Data_5_Factors_2x3_daily.CSV", header = TRUE, sep = "," ))

# Build VAR model
return$static <- list()
return$static$var <- VAR( return$data$return[ , -c( 1:2 ) ], p = 2, type = "const" )

# Total spillover table
return$static$spill_table <- spilloverDY12( return$static$var, n.ahead = 10, no.corr = F )
print( return$static$spill_table )
return$static$pairwise <- NetPairwiseSpilloversThreshold( return$static$spill_table, 0 )
mean( return$static$pairwise$table$Weight )


# Rolling spillover
return$dynamic <- list()
return$dynamic$rolling_spill <- spilloverRollingDY12( return$data$return[ , -c( 1:2 ) ], n.ahead = 10, 
                                                   no.corr = F, "VAR", 
                                                   params_est = list( p = 2, type = "const" ), 
                                                   window = 200 )


# Extracting spills from spills table
return$dynamic$rolling_spill <- CreateRollingDataFrame( return$dynamic$rolling_spill, F )


return$dynamic$rolling_spill$Date <- as.Date( return$data$return$Date[ 200:( nrow( return$dynamic$rolling_spill ) + 199 ) ], formats = c("YYYY-mm-dd", "%Y/%m/%d") )



par( mar = c( 5, 5, 3, 3 ), mfrow = c( 1, 1 ), cex.axis = 1 ,cex.lab = 1.2, family = "A" )
plot( return$dynamic$rolling_spill$Date, return$dynamic$rolling_spill$Total, type = "l", lwd = 2, col = "black",
      xlab = "Time", ylab = "Total spillover index", 
      main = "Log returnatility" )
axis( side = 4 )


# Add risk factors

# Run regressions and save resids

return$factors <- list()
return$factors$resid <- lapply( return$data$return[ , -c( 1, 2 ) ], function( y ) print( summary( lm( y ~ risk_factors_5$Mkt.RF + risk_factors_5$SMB +
                                                                                            risk_factors_5$HML + risk_factors_5$RMW + risk_factors_5$CMA +
                                                                                            risk_factors_5$RF ))))

return$factors$resid <- as.data.frame( lapply( return$data$return[ , -c( 1, 2 ) ], function( y ) resid( lm( y ~ risk_factors_5$Mkt.RF + risk_factors_5$SMB +
                                                                                                    risk_factors_5$HML + risk_factors_5$RMW + risk_factors_5$CMA +
                                                                                                    risk_factors_5$RF ))))

colnames( return$factors$resid ) <- colnames( return$data$return[ , -c( 1, 2 ) ] )
return$factors$resid$Date <- return$data$return$Date
  

# Summary of statistics


statistics_march <- as.data.frame( matrix( rep( 0, 30 ), nrow = 10 ) )
rownames( statistics_march ) <- colnames( vol$data$volatility )[ -1 ]
colnames( statistics_march ) <- c( 'Mean', 'Median', 'Std' )
statistics_march[ , 1 ] <- sapply( return$data$return[ 2234:2383, -c( 1, 2 ) ], mean )
statistics_march[ , 2 ] <- sapply( return$data$return[ 2234:2383, -c( 1, 2 ) ], median )
statistics_march[ , 3 ] <- sapply( return$data$return[ 2234:2383, -c( 1, 2 ) ], sd )

statistics_june <- as.data.frame( matrix( rep( 0, 30 ), nrow = 10 ) )
rownames( statistics_june ) <- colnames( vol$data$volatility )[ -1 ]
colnames( statistics_june ) <- c( 'Mean', 'Median', 'Std' )
statistics_june[ , 1 ] <- sapply( return$data$return[ 2308:2457, -c( 1, 2 ) ], mean )
statistics_june[ , 2 ] <- sapply( return$data$return[ 2308:2457, -c( 1, 2 ) ], median )
statistics_june[ , 3 ] <- sapply( return$data$return[ 2308:2457, -c( 1, 2 ) ], sd )


# BigVAR

# Perform the estimation
mod1<-constructModel( as.matrix( return$data$return[ 2184:2383, -c( 1, 2 ) ] ), p = 2, "Basic", gran = c( 150, 10 ), RVAR = FALSE, h = 10, cv = "Rolling", MN = FALSE, verbose = FALSE, IC = TRUE )
results <- cv.BigVAR( mod1 )
results
plot( results )
spilloverDY12( results, n.ahead = 10, no.corr = T)


