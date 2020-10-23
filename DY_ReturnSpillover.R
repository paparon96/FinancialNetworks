#Load libraries
LoadLibraries()

return <- list()

# Loading data
return$data <- list()
return$data$return <- as.data.frame( read.csv( "Data/Stock_prices/log_returns_all_ts.csv", header = TRUE, sep = "," ) )

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
