invcov2pcorr <- function(invcov) {
  
  n = dim(invcov)[1]
  d = dim(invcov)[2]
  corr <- matrix(, nrow = n, ncol = d)
  
  for (i in 1:n){
    for (j in 1:d){
      
      if (i==j){
        corr[i,j] = 1 
        
      }
      else{
        
        corr[i,j] = -invcov[i,j] / sqrt(invcov[i,i]*invcov[j,j])
        
      }
    }
    
    
    
  }
  
  
  return(corr)
}

get_residuals <- function(filtered_df,filtered_factor_df){
  
  
  n = dim(filtered_df)[1]
  d = dim(filtered_df)[2]
  resids <- matrix(, nrow = n, ncol = d)
  
  for (i in 1:d){
    
    # Temporary dataframe
    temp = as.data.frame(cbind(filtered_df[,i],filtered_factor_df))
    
    # Run regression model
    linearMod <- lm(V1 ~ ., data=temp)
    
    # Get residuals
    temp_resids = residuals(linearMod)
    
    # Assign residuals to the new array
    resids[,i] = temp_resids
  }
  return(resids)
}


dag_matrix <- function(A,n,d){
  
  b = as(A, "dgCMatrix")
  temp = cbind.data.frame(r = b@i + 1, c = b@p + 1, x = b@x)
  print(temp)
  
  mat <- matrix(0, nrow = n, ncol = d)
  
  counter = 0
  col = 1
  
  for (i in 1:dim(temp)[1]){
    
      print(i)
    counter = counter + 1
    
      print(temp$r[i])
      print(col)
      mat[temp$r[i],col] = temp$x[i]
      
      max = 0
      for (j in  1:length(temp$c)){
        
        if (counter >= temp$c[j]){
          
          max = j
          
          
        }
      }
    col = max
    
    
  }
  
  return(mat)
}


