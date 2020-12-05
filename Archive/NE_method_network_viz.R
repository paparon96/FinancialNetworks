# Import packages
library(circlize)

# Import data
temp = read.csv("./Data/Estimated_networks/NE.csv", sep=",", row.names=1)

# Transform to matrix format
NE_matrix = as.matrix(temp)

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
        weight[k] = NE_matrix[i,j]
        
        k = k + 1
        
      }
  
  }
}

# Create dataframe from vectors
df<- data.frame(orig, dest,  
                      weight) 


# Visualisation
rownames(NE_matrix) = varnames
colnames(NE_matrix) = varnames

diag(NE_matrix) <- 0

circos.clear()
title = "./Figures/NE_method_network_chord_diagram.pdf"
#pdf(title) 

chordDiagram(NE_matrix, directional = 1, 
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow")


#dev.off()

