# source("Schaffer.R")
# source("Fonseca.R")
# source("ZDT1.R")
# source("ZDT2.R")
# source("ZDT3.R")
# source("ZDT4.R")
source("ZDT6.R")
source("isDominante.R")
source("Dominantes.R")
source("FastNondominatedSort.R")
source("CrowdingDistance.R")
source("Seleccion.R")
source("Cruza.R")
source("Muta.R")

num <- 100
limInf <- 0
limSup <- 1
numVar <- 10
maxGen <- 250

contador<-301
for (contador in 301:330) {
  set.seed(contador)
  P <- ZDT6(num, limInf, limSup, NA)
  Q <- ZDT6(num, limInf, limSup, NA)
  
  t <- 1 #Generacion 1
  for (t in 1:maxGen) {
    # print(paste("Generacion ", t))
    
    R <- P
    R <- rbind(R, Q)
    Fr <- FastNondominatedSort(R, numVar, num)
    # points(getFront(1, Fr), col="red")
    
    np <- 1
    for (i in 1:max(Fr[,(2+numVar+1)])) {
      cd <- CrowdingDistance(Fr, i, numVar)
      #cd <- getFront(i, Fr)
      for (j in 1:nrow(cd)) {
        if (np <= num) {
          P[np,] <- c(cd[j,1:(2+numVar)])
          np <- np+1
        }
        else break()
      }
    }
    # P<-Fr[1:100,1:(2+numVar)]
    
    #Nueva poblacion
    #Seleccion, SBX, Mutacion Poli
    xr <- Seleccion(P, limInf, limSup, numVar)
    Q <- ZDT6(num, limInf, limSup, xr)
    # plot(P[,1:2], main=paste("Generación ",t), xlim=c(0,1), ylim=c(0,1))
    # plot(getFront(1, Fr), main=paste("Generación ",t))
    t <- t+1
  }
  plot(getFront(1, Fr), main=paste("Experimento ", contador))
  write.table(getFront(1, Fr)[,1:2], file = paste("FUN_ZDT6_",(contador-300),".txt"), sep = " ", col.names = FALSE, row.names = FALSE)
  write.table(getFront(1, Fr)[,3:(2+numVar)], file = paste("VAR_ZDT6_",(contador-300),".txt"), sep = " ", col.names = FALSE, row.names = FALSE)
}