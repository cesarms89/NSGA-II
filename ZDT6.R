getZDT6 <- function(x) {
  f1 <- 1 - (exp(-4*x[1])) * (sin(4*pi*x[1]))^6
  g <- 0
  for (i in 2:10)
    g <- g + x[i]
  g <- g/9
  g <- g^0.25
  g <- 1 + 9*g
  h <- 1 - ((f1/g)^2)
  f2 <- g * h
  return(c(f1, f2))
}

ZDT6 <- function(n, limInf, limSup, xr) {
  obj <- matrix(NA, 0, 2)
  if (length(xr) == 1){
    x <- matrix(NA, 0, 10)
    for (i in 1:n) {
      x <- rbind(x, runif(10, limInf, limSup))
      obj <- rbind(obj, getZDT6(x[i,]))
    }
    obj <- cbind(obj, x)
  }
  else{
    for (i in 1:n) {
      obj <- rbind(obj, getZDT6(xr[i,]))
    }
    obj <- cbind(obj, xr)
  }
  #plot(x, col="blue", type="p")
  return(obj)
}