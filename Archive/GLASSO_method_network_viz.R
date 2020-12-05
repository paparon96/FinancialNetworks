# Import packages
library(circlize)


## METHOD 1: Base case (without factors)

# Import data
temp = read.csv("./Data/Estimated_networks/GLASSO.csv", sep=",", row.names=1)

# Transform to matrix format
network_matrix = as.matrix(temp)

# Parameters
varnames = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")

# Create vectors for network visualisation
n = length(varnames)*(length(varnames)-1)
orig = vector(mode="character", length=n)
dest = vector(mode="character", length=n)
weight <- rep(0, n)

# Fill vectors with the data
k = 0
for (i in 1:length(varnames)){
  for (j in 1:length(varnames)){
      if (i != j){
        orig[k] = varnames[i]
        dest[k] = varnames[j]
        weight[k] = network_matrix[i,j]
        
        k = k + 1
        
      }
  
  }
}

# Create dataframe from vectors
df<- data.frame(orig, dest,  
                      weight) 


# Visualisation
rownames(network_matrix) = varnames
colnames(network_matrix) = varnames

diag(network_matrix) <- 0

circos.clear()
title = "./Figures/GLASSO_method_network_chord_diagram.pdf"
pdf(title) 

chordDiagram(network_matrix, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow")


dev.off()


## METHOD PART 2: Run combined model

# Import data
temp = read.csv("./Data/Estimated_networks/GLASSO_combined_factors.csv", sep=",", row.names=1)

# Transform to matrix format
network_matrix = as.matrix(temp)

# Parameters
varnames = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC","Mkt.RF","SMB","HML","RMW","CMA","RF")

# Create vectors for network visualisation
n = length(varnames)*(length(varnames)-1)
orig = vector(mode="character", length=n)
dest = vector(mode="character", length=n)
weight <- rep(0, n)

# Fill vectors with the data
k = 0
for (i in 1:length(varnames)){
  for (j in 1:length(varnames)){
    if (i != j){
      orig[k] = varnames[i]
      dest[k] = varnames[j]
      weight[k] = network_matrix[i,j]
      
      k = k + 1
      
    }
    
  }
}

# Create dataframe from vectors
df<- data.frame(orig, dest,  
                weight) 


# Visualisation
rownames(network_matrix) = varnames
colnames(network_matrix) = varnames

diag(network_matrix) <- 0

circos.clear()
title = "./Figures/GLASSO_combined_factors_method_network_chord_diagram.pdf"
pdf(title) 

chordDiagram(network_matrix, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow")


dev.off()


## METHOD PART 3: Model with residuals from factor regression

# Import data
temp = read.csv("./Data/Estimated_networks/GLASSO_factor_residuals.csv", sep=",", row.names=1)

# Transform to matrix format
network_matrix = as.matrix(temp)

# Parameters
varnames = c("MS","JPM","BAC","C","WFC","GS","USB","TD","BK","TFC")

# Create vectors for network visualisation
n = length(varnames)*(length(varnames)-1)
orig = vector(mode="character", length=n)
dest = vector(mode="character", length=n)
weight <- rep(0, n)

# Fill vectors with the data
k = 0
for (i in 1:length(varnames)){
  for (j in 1:length(varnames)){
    if (i != j){
      orig[k] = varnames[i]
      dest[k] = varnames[j]
      weight[k] = network_matrix[i,j]
      
      k = k + 1
      
    }
    
  }
}

# Create dataframe from vectors
df<- data.frame(orig, dest,  
                weight) 


# Visualisation
rownames(network_matrix) = varnames
colnames(network_matrix) = varnames

diag(network_matrix) <- 0

circos.clear()
title = "./Figures/GLASSO_factor_residuals_method_network_chord_diagram.pdf"
pdf(title) 

chordDiagram(network_matrix, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow")


dev.off()

