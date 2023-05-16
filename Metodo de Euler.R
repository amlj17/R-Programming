#Metodo de Euler
#Entradas:
#definimos la funcion
#y' = t^-2(sin2t-2ty)
f<-function(x,q){x^(-2)*((sin(2*x))-(2*x*q))}
#se evalua x como t y q como w
a<-readline("Introduce el minimo del intervalo: ")
a<-as.numeric(a)
b<-readline("Introduce el maximo del intervalo: ")
b<-as.numeric(b)
Alfa<-readline("Introducir el valor inicial alfa = w: ")
Alfa<-as.numeric(Alfa)
N<-readline("Introducir el numero de puntos: ")
N<-as.numeric(N)
Tabla<-matrix(nrow = N+1,ncol = 2)
#Paso 1
h<-(b-a)/N
t<-a
w<-Alfa
Tabla[1,1]<-t
Tabla[1,2]<-w

for(i in 1:N){
  w<-w+h*f(t,w)
  t<-a+i*h
  Tabla[i+1,1]<-t
  Tabla[i+1,2]<-w
}

print(Tabla)
