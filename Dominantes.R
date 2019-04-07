Dominantes <- function(x) {
  n <- nrow(x)
  dominantes <- matrix(NA,n,2)
  dominantes[1,1] <- x[1,1]
  dominantes[1,2] <- x[1,2]
  
  for(i in 1:n){
    agregar <- TRUE
    for(j in 1:n){
      if(!is.na(dominantes[j,1])){
        
        if(isDominante(x[i,], dominantes[j,])){
          #es dominante
          dominantes[j,1] <- NA
          dominantes[j,2] <- NA
          agregar <- TRUE
        }
        else if(isDominante(dominantes[j,], x[i,])){
          #es dominada
          agregar <- FALSE
          break()
        }
        
      }
    }
    
    if(agregar){
      for(j in 1:n){
        if(is.na(dominantes[j,1])){
          dominantes[j,1] <- x[i,1]
          dominantes[j,2] <- x[i,2]
          break()
        }
      }
    }
    
  }
  # plot(dominantes, col="blue", type="p")
  return(dominantes)
}