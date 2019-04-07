#tip, 1 solo hijo por generacion en lugar de hacer 100
SBX <- function(p1, p2, limInf, limSup, numVar) {
  # print(paste("p1: ", p1, ", p2: ", p2))
  
  li <- limInf
  ei <- limSup
  n <- 20
  ui <- runif(1)
  
  c1 <- matrix(NA, 1, numVar)
  c2 <- matrix(NA, 1, numVar)
  
  for (i in 1:numVar) {
    
    if (runif(1) <= 0.1) {
      # print("MUTA")
      c1[1,i] <- MutaPolinomial(p1[i], limInf, limSup)
      c2[1,i] <- MutaPolinomial(p2[i], limInf, limSup)
    }
    else if(runif(1) <= 0.5){
      c1[1,i] <- p1[i]
      c2[1,i] <- p2[i]
    }
    else{
      x1 <- max(c(p1[i], p2[i]))
      x2 <- min(c(p1[i], p2[i]))
      
      B1 <- 1 + ((2*(x2 - li)) / x1 - x2)
      B2 <- 1 + ((2*(ei - x1)) / x1 - x2)
      
      alfa1 <- 2 - ((B1)^-(n+1))
      alfa2 <- 2 - ((B2)^-(n+1))
      
      if (ui <= (1/alfa1))
        B1q <- (ui * alfa1)^(1/(n+1))
      else
        B1q <- (1/(2-ui*alfa1))^(1/(n+1))
      
      if (ui <= (1/alfa2))
        B2q <- (ui * alfa2)^(1/(n+1))
      else
        B2q <- (1/(2-ui*alfa2))^(1/(n+1))
      
      c1[1,i] <- 0.5 * (x1+x2 - B1q*(x1-x2))
      c2[1,i] <- 0.5 * (x1+x2 + B2q*(x1-x2))
      
      if(is.nan(c1[1,i]))
        c1[1,i] <- runif(1, limInf, limSup)
      if(is.nan(c2[1,i]))
        c2[1,i] <- runif(1, limInf, limSup)
      
    }
    
  }
  # print(paste("c1: ", c1, " c2: ", c2))
  return(rbind(c1,c2))
}

CruzaSimple <- function(p1, p2, limInf, limSup) {
  if (runif(1) > 0.5){
    c1 <- (p1+p2)/2
    c2 <- (p1-p2)/2
  }
  else{
    c1 <- (p1-p2)/2
    c2 <- (p1+p2)/2
  }
  c1 <- MutaPolinomial(p1, limInf, limSup)
  c2 <- MutaPolinomial(p1, limInf, limSup)
  return(c(c1, c2))
}

EvolucionDiferencial <- function(p1, p2, limInf, limSup, numVar) {
  if (runif(1) <= 1.0) {
    c1 <- matrix(NA, 1, numVar)
    c2 <- matrix(NA, 1, numVar)
    
    # c1[1,1] <- MutaPolinomial(p1[1], 0, 1)
    # c2[1,1] <- MutaPolinomial(p2[1], 0, 1)
    # for (i in 2:numVar) {
    #   c1[1,i] <- MutaPolinomial(p1[i], limInf, limSup)
    #   c2[1,i] <- MutaPolinomial(p2[i], limInf, limSup)
    # }
    for (i in 1:numVar) {
      c1[1,i] <- MutaPolinomial(p1[i], limInf, limSup)
      c2[1,i] <- MutaPolinomial(p2[i], limInf, limSup)
    }
  }
  else{
    # c1[1,1] <- evdf(p1[1], 0, 1)
    # c2[1,1] <- evdf(p2[1], 0, 1)
    # for (i in 2:numVar) {
    #   c1[1,i] <- evdf(p1[i], limInf, limSup)
    #   c2[1,i] <- evdf(p2[i], limInf, limSup)
    # }
    for (i in 1:numVar) {
      c1[1,i] <- evdf(p1[i], limInf, limSup)
      c2[1,i] <- evdf(p2[i], limInf, limSup)
    }
  }
  return(rbind(c1,c2))
}

evdf <- function(p, limInf, limSup) {
  u <- p
  x <- runif(3, limInf, limSup)
  j <- sample(1:3,1,replace=F)
  jrand <- runif(1)
  f <- 0.5
  cr <- 0.9
  vi <- x[1] + f*(x[2]-x[3])
  if (jrand <= cr || jrand == x[j])
    u <- vi
  else if (jrand > cr || jrand != x[j])
    u <- x[j]
  return(u)
}