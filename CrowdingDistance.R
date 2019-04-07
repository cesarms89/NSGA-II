CrowdingDistance <- function(soluciones, frente, numVar) {
  
  soluciones <- getFront(frente, soluciones)
  
  l <- nrow(soluciones)
  distancia <- vector("numeric",l)
  
  distancia[1]<-Inf
  distancia[l]<-Inf
  
  if (l > 2) {
    soluciones<-soluciones[order(soluciones[,1], decreasing = FALSE),]
    for(i in 2:(l-1))
      distancia[i] <- distancia[i] + (soluciones[i+1, 1] - soluciones[i-1, 1]) / (max(soluciones[,1]) - min(soluciones[,1]))
    
    soluciones <- soluciones[order(soluciones[,2], decreasing = FALSE),]
    for(i in 2:(l-1))
      distancia[i] <- distancia[i] + (soluciones[i+1, 2] - soluciones[i-1, 2]) / (max(soluciones[,2]) - min(soluciones[,2]))
    
    soluciones <- cbind(soluciones, distancia)
    soluciones <- soluciones[order(soluciones[,(2+numVar+2)], decreasing = TRUE),]
    # print(soluciones)
    return(soluciones)
  }
  else if (l == 1 || l == 2) {
    soluciones <- cbind(soluciones, distancia)
    return(soluciones)
  }
}