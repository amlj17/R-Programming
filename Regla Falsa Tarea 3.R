#Tarea 3 
#Andres Moguel
#Regla Falsa

#Comenzamos con la definicion de la funcion
f <- function(x){
  2*x*cos(2*x)-(x+1)^2
}

plot(f, -1, 0)

#definimos la presicion
tol <- 10^-6
#pedimos las entradas ponemos p1 y p0 mayor a 0 ya que buscamos la raiz positiva
#se recomienda elegir un p0 y p1 entre 0 y 2 basado en la grafica proporcionada
num <- as.numeric(readline("Ingrese el numero maximo de iteraciones: "))
p0 <- as.numeric(readline("Ingrese la primer aproximacion inical: "))
p1 <- as.numeric(readline("Ingrese la segunda aproximacion inicial: "))

#inicializamos el contador
i <- 2

#procedemos con el algoritmo
while(i <= num){
  p2<-(p1-f(p1)*((p1 - p0)/(f(p1)-f(p0))))
  
  if(abs(p2 - p1)<= tol | f(p2) == 0){
    print('La aproximacion a la raiz es:')
    print(p2)
    plot(f, 0, 2)
    break
  }
  i <- i + 1
  if(f(p1)*f(p2) < 0){
    p0 <- p1
  }
  p1 <- p2
}

if(i == num +1){
  print("se llego al numero maximo de iteraciones")
}