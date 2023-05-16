#Tarea 6 
#Andres Moguel
#Lagrange grado n

#Comenzamos con la definicion de la funcion
f <- function(x){
  x*log(x)
}

plot(f, col = 'orange')

#Definimos entradas, queremos grado n, pedimos el grado
grado <- as.numeric(readline("Ingrese el grado deseado del polinomio: "))
#creamos los puntos x necesarios para el polinomio
m = grado+1
puntosx<-vector(length = m)
contador=0
for (i in puntosx) {
  contador<-contador+1
  punto <- as.numeric(readline("Ingrese el valor _ para el calculo: "))
  puntosx[contador]<-punto
}

#creamos los puntos "y" (f(xi)) necesarios para el polinomio
puntosy<-vector(length = m)
contador=0
for(j in puntosy){
  contador<-contador +1
  for(i in puntosx)
  puntosy[contador]<-f(puntosx[contador])
}

#Aplicamos entonces el metodo de lagrange
#primero pedimos el valor a aproximar:
a<-as.numeric(readline("Ingrese el valor en el que desea aproximar f(x): "))
P<-0.0
for(k in 1:m){
  producto <- 1
  for(l in 1:m){
    if(l!=k){
      producto<-producto*((a - puntosx[l])/(puntosx[k]-puntosx[l]))
    }
    l<-l+1
  }
  P<- P + f(puntosx[k])*producto
  k<-k+1
}
print(paste("Aproximando en el punto dado tenemos que el polinomio da: ", P))
