isDominante <- function(a, b) {
  # iguales<-TRUE
  # for(i in 1:length(a)){
  #   if (a[i]!=b[i])
  #     iguales<-FALSE;
  # }
  # if(iguales)
  #   return(TRUE)
  # for(i in 1:length(a)){
  #   if (a[i]>b[i])
  #     return(FALSE)
  # }
  # return(TRUE)
  return(all(a[1:2] <= b[1:2]) && any(a[1:2] < b[1:2]))
  # return((all(a[1:2] <= b[1:2]) && any(a[1:2] < b[1:2])) || all(a[1:2] == b[1:2]))
}