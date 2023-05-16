#vectores y matrices

#generar un vector
c(1,3,4,5)
x<-c(1,3,4,5)
x
#referirme a un elemento del vector
x[4]
y<-c("lunes", "martes", "miercoles")
y
y[2]
z<-c(9,10,11)
#juntar dos vectores
w<-c(x,z)
w
#asignar una secuencia
w<-seq(1,10)
w
#incrementos de paso 
w<-seq(1,10, by = 0.5)
w
#longitud total del vector
w<-seq(1,10, length=50)
w

#Matrices
a<-matrix(1:6,nrow = 2)
a
a<-matrix(1:6,ncol = 2)
a
#ordenar por renglones
a<-matrix(1:6,nrow = 2, byrow = T)
a
help("matrix")

#Operaciones con matrices
