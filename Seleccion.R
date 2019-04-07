Seleccion <- function(P, limInf, limSup, numVar) {
  i <- 1
  xt <- matrix(NA, 0, numVar)
  #Seleccion de los padres y la tecnica de cruza
  while (i <= nrow(P)) {
    temp <- SBX(P[i, 3:(2+numVar)], P[i+1, 3:(2+numVar)], limInf, limSup, numVar)
    # temp <- EvolucionDiferencial(P[i, 3:(2+numVar)], P[i+1, 3:(2+numVar)], limInf, limSup, numVar)
    xt <- rbind(xt, temp)
    i <- i + 2
  }
  return(xt)
}