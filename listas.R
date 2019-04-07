t<-matrix(0,1,3)
t<-rbind(t,c(x[1,],1))

ls = vector("list", 5)
ls[[1]]<-c(x[1,])
ls[[3]]<-c(x[3,])

ls[[2]]<-matrix(0,2,3)

ls[[2]]
ls[[2]][1,]


frentes <- vector("list", 1)
frentes[[1]] <- c(1,2)
frentes[[2]] <- c(3,4)
frentes[[3]] <- c(5,6)

length(frentes)

h <- vector("numeric",0)
i<-1
for (i in 1:length(frentes)) {
  print(frentes[[i]])
}