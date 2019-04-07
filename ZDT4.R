getZDT4 <- function(x) {
  f1 <- x[1]
  g <- 0
  for (i in 2:10)
    g <- g + x[i]*x[i] - 10*cos(4*pi*x[i])
  g <- g + 91
  h <- 1 - sqrt(f1/g)
  f2 <- g * h
  return(c(f1, f2))
}

ZDT4 <- function(n, limInf, limSup, xr) {
  obj <- matrix(NA, 0, 2)
  if (length(xr) == 1){
    x <- matrix(NA, 0, 10)
    for (i in 1:n) {
      x <- rbind(x, c(runif(1), runif(9, limInf, limSup)))
      obj <- rbind(obj, getZDT4(x[i,]))
    }
    obj <- cbind(obj, x)
  }
  else{
    for (i in 1:n) {
      obj <- rbind(obj, getZDT4(xr[i,]))
    }
    obj <- cbind(obj, xr)
  }
  #plot(x, col="blue", type="p")
  return(obj)
}