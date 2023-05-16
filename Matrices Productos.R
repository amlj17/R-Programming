#Matrices
#Definimos la matriz
A<-matrix(c(2,1,5,3,1,0,0,2,3), nrow = 3, byrow = T)
B<-matrix(c(1,2,3), nrow=3)
C<-matrix(,nrow=3, ncol = 1)
#calculo directo
A%*%B

#definicion del producto de matrices
for(i in 1:3){
  for(j in 1:1){
    suma<-0
    for(k in 1:3){
      suma<-suma+A[i,k]*B[k,j]
    }
    C[i,j]<-suma
  }
}
A
B
C

