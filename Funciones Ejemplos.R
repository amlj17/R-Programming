## Definicion de funciones
##
f<-function(x){x*cos(x)}
#evaluar una funcion
y<-f(pi)
##
#graficar f(x)
plot(f,0,2*pi,col="red")
##
# otra funcion
#
# definimos y graficamos g(x)
g<-function(x){x^2-4*x+4-log(x)}
par(new=TRUE) #ambas graficas en una ventana
plot(g,0,2*pi,col="blue")
abline(h=0, v=0, col="green")
abline(0,1,col="yellow") #ordenada, pendiente
