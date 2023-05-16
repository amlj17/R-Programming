#Metodo de Euler
#Entradas:
#definimos la funcion
#y' = t^-2(sin2t-2ty) = f(t, w)
f<-function(x,q){x^(-2)*((sin(2*x))-(2*x*q))}
#f prima se define como:
f1<-function(x,q){(6*x*q+2*x*cos(2*x)-4*sin(2*x))/(x^3)}
#f bi prima se define como:
f2<-function(x,q){(2*(-2*x^2*sin(2*x)-12*x*q-6*x*cos(2*x)+9*sin(2*x)))/(x^4)}
#f tri prima se define como:
f3<-function(x,q){(8*(-x^3*cos(2*x)+4*(x^2)*sin(2*x)+15*x*q+9*x*cos(2*x)-12*sin(2*x)))/(x^5)}
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
    #Aplicamos la relacion de recurrencia
  w<-w+h*f(t,w)+(h^2/2)*f1(t,w)+(h^3/6)*f2(t,w)+(h^4/24)*f3(t,w)
  t<-a+i*h
  Tabla[i+1,1]<-t
  Tabla[i+1,2]<-w
}

print(Tabla)
