source("Schaffer.R")
source("Fonseca.R")
source("ZDT1.R")
source("isDominante.R")
source("Dominantes.R")
source("FastNondominatedSort.R")
source("CrowdingDistance.R")
source("Seleccion.R")
source("Cruza.R")
source("Muta.R")

num <- 200
limInf <- -4
limSup <- 4
maxGen <- 50
numVar <- 3

# set.seed(1)
P <- Fonseca(num, limInf, limSup, NA)
Q <- Fonseca(num, limInf, limSup, NA)

# dom <- Dominantes(P)
# plot(R)
#plot(dom)
# points(Q, col="blue")

# P <- FastNondominatedSort(P)
# # plot(P)
# # points(getFront(1, Fr), col="red")

t <- 1 #Generacion 1
for (t in 1:maxGen) {
  print(paste("Generacion ", t))
  
  R <- P
  R <- rbind(R, Q)
  # R[order(R[,3], decreasing = TRUE),]
  Fr <- FastNondominatedSort(R, numVar)
  # points(getFront(20, Fr), col="red")
  
  np <- 1
  for (i in 1:max(Fr[,4])) {
    cd <- CrowdingDistance(Fr, i, numVar)
    for (j in 1:nrow(cd)) {
      if (np <= num) {
        P[np,] <- c(cd[j,1:(2+numVar)])
        np <- np+1
      }
      else break()
    }
  }
  
  #Nueva poblacion
  #Seleccion, Cruza, Mutacion Poli
  xr <- Seleccion(P, limInf, limSup, numVar)
  Q <- Fonseca(num, limInf, limSup, xr)
  t <- t+1
  plot(P[,1:2])
}

