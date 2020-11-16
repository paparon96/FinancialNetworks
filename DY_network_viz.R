# Load the necessary libraries
LoadLibraries()

# Set parameters for the network to plot
threshold <- 0.0
arrow_size <- 0.8
edge_size <- 200
window <- 150


# Last date of window
date <- as.Date( "2020-03-05" )


# Function to plot graph (first run the function's code)
graphs <- PlotNetworks( date, "Volatility", window, vol$data$volatility, arrow_size, edge_size, threshold )
graphs$spill
graphs <- PlotNetworks( date, "Volatility", window, vol$factors$resid, arrow_size, edge_size, threshold )
graphs$spill

graphs <- PlotNetworks( date, "Return", window, return$data$return[ , -1 ], arrow_size, edge_size, threshold )
graphs$spill
graphs <- PlotNetworks( date, "Return", window, return$factors$resid, arrow_size, edge_size, threshold )
graphs$spill


# Function
PlotNetworks <- function( date, measure, window, data, arrow_size, edge_size, threshold ){
  rownames( data ) <- c( 1:length( data$Date ) )
  end_date <- as.numeric( rownames( data[ as.Date( data$Date ) == date, ] ) )
  start_date <- end_date - 199
  graph_var <- VAR( subset( data[ start_date:end_date, ], select = -c( Date ) ), p = 2, type = "const" )
  
  graph_spill <- spilloverDY12( graph_var, n.ahead = 10, no.corr = F )
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


