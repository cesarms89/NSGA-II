getZDT3 <- function(x) {
  f1 <- x[1]
  g <- 0
  for (i in 2:30)
    g <- g + x[i]
  g <- 1 + (9 * g / (30-1))
  h <- 1 - sqrt(f1/g) - (f1/g)*sin(10*pi*f1)
  f2 <- g * h;
  return(c(f1, f2))
}

ZDT3 <- function(n, limInf, limSup, xr) {
  obj <- matrix(NA, 0, 2)
  if (length(xr) == 1){
    x <- matrix(NA, 0, 30)
    for (i in 1:n) {
      x <- rbind(x, runif(30, limInf, limSup))
      obj <- rbind(obj, getZDT3(x[i,]))
    }
    obj <- cbind(obj, x)
  }
  else{
    for (i in 1:n) {
      obj <- rbind(obj, getZDT3(xr[i,]))
    }
    obj <- cbind(obj, xr)
  }
  #plot(x, col="blue", type="p")
  return(obj)
}