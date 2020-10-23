NetPairwiseSpilloversThreshold <- function( spill_table, threshold ){ 
  
  spill_table <- as.data.frame( spill_table$tables[ 1 ] )
  
  net_table <- data.frame( matrix( nrow = ( length( spill_table ) * ( length( spill_table ) - 1 ) / 2 ), ncol = 4 ) )
  colnames( net_table ) <- c( "Source", "Target", "Type", "Weight" )            
  count <- 0
  n <- 1
  
  for ( i in 1:length( spill_table ) ){
    for ( j in n:length( spill_table ) ){
      if ( i != j ){
        if( abs( ( spill_table[ j, i ] - spill_table[ i, j ] ) ) >= threshold ){
          
          count <- count + 1
          net_table$Type[ count ] <- "Directed"
          
          if( ( spill_table[ j, i ] - spill_table[ i, j ] ) > 0 ){
            
            net_table$Source[ count ] <- colnames( spill_table )[ i ]
            net_table$Target[ count ] <- rownames( spill_table )[ j ]
            net_table$Weight[ count ] <- spill_table[ j, i ] - spill_table[ i, j ]
          }else{
            
            net_table$Source[ count ] <- colnames( spill_table )[ j ]
            net_table$Target[ count ] <- rownames( spill_table )[ i ]
            net_table$Weight[ count ] <- - ( spill_table[ j, i ] - spill_table[ i, j ] )
          }
        }
      }
    }
    n <- n + 1
  }
  
  net_table <- drop_na( net_table )
  
  graph_names <- c()
  weights <- c()
  
  for ( i in 1:nrow( net_table ) ){
    
    graph_names <- c( graph_names, net_table[ i, 1 ] )
    graph_names <- c( graph_names, net_table[ i, 2 ] )  
    weights <- c( weights, net_table[ i, 4 ] )
    
  }
  
  net_data <- list( table = net_table, names = graph_names, weights = weights )
  return( net_data )

}

