# x1=runif(num,-1,1)^2
# x2=(runif(num,-1,1)-2)^2
# #solucion<-vector("numeric", 100)
# plot(x1, x2)
# 
# 
# # ctrl+l
# # rm(list=ls())
# # for(i in 1:length(n))
# for(i in 1:num){ 
#   #print(paste("Num ", i, ": ", n[i])) 
#   print(paste(x1[i], " ", x2[i])) 
# }

num <- 200
m<-matrix(,num,2)
for(i in 1:num){
  tmp<-runif(1,-4,4)
  m[i,1]<-tmp^2
  m[i,2]<-(tmp-2)^2
}
plot(m)