#Metodo de R_K
#Entradas:
#definimos la funcion
f<-function(t,w){(w/t)-(w/t)^2}
a<-readline("Introduce el minimo del intervalo: ")
a<-as.numeric(a)
b<-readline("Introduce el maximo del intervalo: ")
b<-as.numeric(b)
Alfa<-readline("Introducir el valor inicial alfa = w: ")
Alfa<-as.numeric(Alfa)
N<-readline("Introducir el numero de puntos: ")
N<-as.numeric(N)
Tabla<-matrix(nrow = N+1,ncol = 6)
#Paso 1
h<-(b-a)/N
t<-a
w<-Alfa
Tabla[1,1]<-t
Tabla[1,6]<-w

for(i in 1:N){
    #Aplicamos la relacion de recurrencia
  k1<-h*f(t,w)
  k2<-h*f(t+(h/2),w+(k1/2))
  k3<-h*f(t+(h/2),w+(k2/2))
  k4<-h*f(t+h,w+k3)
  w<-w+(k1+2*k2+2*k3+k4)/6
  t<-a+i*h
  Tabla[i+1,1]<-t
  Tabla[i+1,2]<-k1
  Tabla[i+1,3]<-k2
  Tabla[i+1,4]<-k3
  Tabla[i+1,5]<-k4
  Tabla[i+1,6]<-w
}

print(Tabla)
