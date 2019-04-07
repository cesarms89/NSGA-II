Fobj1 <- function(x) {
  sum<-0;
  for(i in 1:3){
    sum<-sum+(x[i]-(1/sqrt(3)))^2
  }
  return(1- exp(-1*sum) )
}
Fobj2 <- function(x) {
  sum<-0;
  for(i in 1:3){
    sum<-sum+(x[i]+(1/sqrt(3)))^2
  }
  return(1- exp(-1*sum) )
}

Fonseca <- function(n, limInf, limSup, xr) {
  
  obj <- matrix(NA,n,2)
  
  if (length(xr) == 1){
    x <- matrix(NA,n,3)
    for(i in 1:n){
      x[i,] <- runif(3, limInf, limSup)
      # print(x[i,])
      obj[i,1] <- Fobj1(x[i,])
      obj[i,2] <- Fobj2(x[i,])
    }
    obj <- cbind(obj, x)
  }
  else{
    for(i in 1:n){
      obj[i,1] <- Fobj1(xr[i,])
      obj[i,2] <- Fobj2(xr[i,])
    }
    obj <- cbind(obj, xr)
  }
  #plot(x, col="blue", type="p")
  return(obj)
}