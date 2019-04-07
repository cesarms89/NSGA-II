FastNondominatedSort <- function(population, numVar, num) {
  popNum <- nrow(population)
  popDominantes <- vector("list", popNum) #Sp
  popDominados <- vector("list", popNum) #np
  # print(popNum)
  
  for (p in 1:popNum){
    for (q in 1:popNum){
      if(isDominante(population[p,], population[q,])){
        #Guarda el indice de los dominados y de los dominantes
        popDominantes[[q]] <- c(popDominantes[[q]], p)
        popDominados[[p]] <- c(popDominados[[p]], q)
      }
      else if(isDominante(population[q,], population[p,])){
        popDominantes[[p]] <- c(popDominantes[[p]], q)
        popDominados[[q]] <- c(popDominados[[q]], p)
      }
    }
  }
  # print(popDominantes)
  
  #obtenemos la cantidad de elementos dominados por cada elemento
  numDominados <- lapply(popDominantes,length);
  # print(numDominados)
  
  #Creacion del frente 1
  F1 <- matrix(NA, 0, (2+numVar))
  for (i in 1:popNum){
    if(numDominados[[i]]==0){
      F1 <- rbind(F1, c(population[i,]))
    }
  }
  
  i <- 1
  Frentes <- vector("list", 1)
  Frentes[[1]] <- F1
  
  #Creacion de n frentes
  while (TRUE){
    H <- matrix(NA, 0, (2+numVar))
    for (i in 1:length(Frentes)) {
      for (q in 1:length(numDominados)) {
        numDominados[[q]] <- numDominados[[q]]-1
        if (numDominados[q]==0) {
          H <- rbind(H, c(population[q,]))
          # H <- rbind(H, c(population[q,1], population[q,2]))
        }
      }
    }
    if(nrow(H)!=0){
      i <- i+1
      Frentes[[i]] <- H
    }
    else
      break()
    # print(Frentes)
  }
  
  #Convirtiendo la lista de frentes a matriz
  populationF <- matrix(NA, 0, (2+numVar+1))
  for (i in 1:length(Frentes)) {
    for (j in 1:nrow(Frentes[[i]]))
      populationF <- rbind(populationF, c(Frentes[[i]][j,], i))
  }
  
  #Inicio rellenar con los mejores de los peores en caso de no haber mas frentes
  itemp <- 1
  ftemp <- max(populationF[,(2+numVar+1)])+1
  while(nrow(populationF)<num){
    if(numDominados[itemp]>0){
      for (j in 1:length(numDominados)) {
        if (numDominados[[itemp]] <= numDominados[[j]] && itemp!=j) {
          populationF <- rbind(populationF, c(population[j,], ftemp))
        }
      }
    }
    itemp <- itemp+1
  }
  #Fin rellenar con los mejores de los peores en caso de no haber mas frentes
  
  # print(populationF)
  return(populationF)
}

#obtener el frente x
getFront <- function(f, Population) {
  m <- matrix(NA, 0, (2+numVar+1))
  for (i in 1:nrow(Population)) {
    if (Population[i,(2+numVar+1)]==f) {
      m <- rbind(m, c(Population[i,]))
    }
    else if(Population[i,(2+numVar+1)]>i)
      break()
  }
  return(m)
}