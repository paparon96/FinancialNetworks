CreateRollingDataFrame <- function( rolling_spills, within = T ){
  rows <- nrow( as.data.frame( overall( rolling_spills, within ) ) )
  spills_data <- as.data.frame( matrix( nrow = rows, ncol = 76 ) )
  spills_data[ , 1 ] <- as.data.frame( overall(rolling_spills, within ) )
  spills_data[ , 2:11 ] <- as.data.frame( to( rolling_spills, within ) )
  spills_data[ , 12:21 ] <- as.data.frame( from( rolling_spills, within ) )
  spills_data[ , 22:31 ] <- as.data.frame( net( rolling_spills, within ) )
  spills_data[ , 32:76 ] <- as.data.frame( pairwise( rolling_spills, within ) )
  colnames( spills_data ) <- c( "Total", colnames( as.data.frame( to( rolling_spills, within ) ) ), 
                                colnames( as.data.frame( from( rolling_spills, within ) ) ),
                                colnames( as.data.frame( net( rolling_spills, within ) ) ),
                                colnames( as.data.frame( pairwise( rolling_spills, within ) ) )
                                )
  return( spills_data )
}
