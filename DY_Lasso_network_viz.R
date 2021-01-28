# Load the necessary libraries
LoadLibraries()

# Set parameters for the network to plot
threshold <- 0.0
arrow_size <- 0.8
edge_size <- 200
window <- 50



# Last date of window
date <- as.Date( "2020-06-30" )
date <- as.Date( "2020-03-16" )
date <- "2020-12-31"

dates <- c( "2019-12-31", "2020-03-16", "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31" )

for( date in dates ){
  csv_name <- paste( 'Data/Large_network/Estimated_networks/largenet_DY_return_', date, '_window_size_', window, '.csv', sep = '' )
  csv_name <- gsub( "-", "_", csv_name )
  graphs <- PlotLassoNetworks( date, "Return", window, return$data$return, arrow_size, edge_size, threshold )
  write.csv( graphs$spill[ 1 ], csv_name )
  
  upper <- as.matrix( as.data.frame( graphs$spill[ 1 ] ))
  upper[ lower.tri( upper ) ] <- 0
  lower <- as.matrix( as.data.frame( graphs$spill[ 1 ] ))
  lower[ upper.tri( lower ) ] <- 0
  net <- lower - t( upper )
  net <- net - t( net )
  diag( net ) <- diag( upper )
  
  csv_net_name <- paste( 'Data/Large_network/Estimated_networks/largenet_DY_net_return_', date, '_window_size_', window, '.csv', sep = '' )
  csv_net_name <- gsub( "-", "_", csv_net_name )
  write.csv( net, csv_net_name )
}



# Function
PlotLassoNetworks <- function( date, measure, window, data, arrow_size, edge_size, threshold ){
  rownames( data ) <- c( 1:length( data$Date ) )
  end_date <- as.numeric( rownames( data[ as.Date( data$Date ) == date, ] ) )
  start_date <- end_date - 199
  mod1<-constructModel( as.matrix( subset( data[ start_date:end_date, ], select = -c( Date ) ) ), p = 2, "Basic", gran = c( 150, 10 ), RVAR = FALSE, h = 10, cv = "Rolling", MN = FALSE, verbose = FALSE, IC = TRUE )
  graph_var <- cv.BigVAR( mod1 )
  graph_spill <- spilloverDY12( graph_var, n.ahead = 10, no.corr = T )
  graph <- NetPairwiseSpilloversThreshold( graph_spill, threshold )
  
  graph$graph <- make_graph( graph$names, directed = T, isolates = setdiff( colnames( vol$data$volatility[ , -1 ] ), graph$names ) )
  graph$graph <- set_edge_attr( graph$graph, "weight", value = ( graph$weights * edge_size ) )
  
  
  par( mar = c( 0, 0, 2.5, 0 ), mfrow = c( 1, 1 ), cex = 0.9 )
  vt_coords <- layout_in_circle( graph$graph, order = order( V( graph$graph )$name, decreasing = T ) )
  vt_plot <- plot( graph$graph, layout = vt_coords, vertex.size = 3,
                   vertex.label.dist = 0.5, vertex.color = "red", edge.arrow.size = arrow_size,
                   edge.width = E( graph$graph )$weight, family = "A", main = measure ) 
  
  mtext( paste( data$Date[ end_date ] ), side = 3, line = -2.8, family = "A", outer = TRUE, cex = 0.9 )
  
  
  network <- list( VAR = graph_var, spill = graph_spill, graph = graph, plot = plot )
  
  return( network )
}


