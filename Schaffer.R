Schaffer <- function(n, limInf, limSup, xr) {
  
  obj <- matrix(NA,n,2)
  
  if (length(xr) == 1) {
    x <- vector("numeric", n)
    for(i in 1:n){
      x[i] <- runif(1, limInf, limSup)
      obj[i,1] <- x[i]^2
      obj[i,2] <- (x[i]-2)^2
    }
    obj <- cbind(obj, x)
  }
  else{
    for(i in 1:n){
      obj[i,1] <- xr[i]^2
      obj[i,2] <- (xr[i]-2)^2
    }
    obj <- cbind(obj, xr)
  }
  
  #plot(x)
  return(obj);
}