MutaPolinomial <- function(x, limInf, limSup) {
  n <- 20
  ui <- runif(1)

  if (ui < 0.5)
    di <- ((2*ui)^(1/(n+1)))-1
  else
    di <- 1 - ((2*(1-ui))^(1/(n+1)))

  xt <- x + (limSup-limInf) * di
  
  if(xt < limInf)
    xt <- limInf + 0.000001
  else if(xt > limSup)
    xt <- limSup - 0.000001
  
  return(xt)
  # return(runif(1, limInf, limSup))
}