LoadLibraries()

DN_matrix <- read.csv( "Data/Estimated_networks/Demirer_NS_volatility_2008_09_15.csv" )

DN_matrix[ "X" ] <- paste( rep( "V", nrow( DN_matrix ), DN_matrix[ "X" ]  ) )

DN_matrix$X <- sub( "^", "V", DN_matrix$X )


data_columns <- as.data.frame( matrix( rep( 0, ( nrow( DN_matrix ) *  ( nrow( DN_matrix ) - 1 ) / 2 * 4 )), ncol = 4 ) )
colnames( data_columns ) <- c( "Source", "Target", "Type", "Weight" )

data_columns[ "Weight" ] <- as.matrix( DN_matrix[ , 2:107] )[ lower.tri( as.matrix( DN_matrix[ , 2:107] ), diag = FALSE) ]

grid <- combn( colnames( DN_matrix[ , 2:107] ), 2, simplify = FALSE )

grid <- unlist( grid )
data_columns[ "Source" ] <- grid[ c( TRUE, FALSE ) ]
data_columns[ "Target" ] <- grid[ c( FALSE, TRUE ) ]
data_columns[ "Type" ] <- "Directed"

write.csv( data_columns, "Data/Estimated_networks/gephi_data.csv", row.names = FALSE )
