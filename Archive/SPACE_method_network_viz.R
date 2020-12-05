# Set working directory
setwd("/Egyetem/BGSE/Studies/Research/FinNetworks/FinancialNetworks")

# Import packages
library(circlize)

# Import data
temp = read.csv("./Data/Estimated_networks/SPACE.csv", sep=",", row.names=1)

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
title = "./Figures/SPACE_method_network_chord_diagram.pdf"
pdf(title) 

chordDiagram(network_matrix, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow")


dev.off()

