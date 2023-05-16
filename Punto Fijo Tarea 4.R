#Tarea 4 
#Andres Moguel
#Newton-Raphson

#g(x) = x que usamos para justificar el intervalo 
g <- function(x){
  (5/(x^2))+2
}

#funcion identidad
iden <- function(x){
  x
}

#graficamos junto con una recta identidad
#Lo hacemos en el intervalo que elegimos donde hay un único punto fijo
#es el intervalo [2.6, 2.8]
plot(g, 2.6, 2.68, col = "blue")
par(new = TRUE)
plot(iden, 2.6, 2.8, col = "orange")

#elegiremos la aproximavcion inicial p = dentro de este intervalo
# Por ejemplo podemos elegir 
#p = -2.685

#definimos la presicion
tol <- 10^-5

#Pedimos las entradas al programa: aproximacion inicial y numero de iteraciones
num <- as.numeric(readline("Ingrese el numero maximo de iteraciones: "))
p0 <- as.numeric(readline("Ingrese la primer aproximacion inical: "))

i <- 1
while (i <= num) {
  p <- g(p0)
  if(abs(p - p0) <= tol | g(p) == p){
    print("El punto fijo y raiz es:")
    print(p)
    break
  }
  i <- i +1
  p0 <- p
}

if(i == num +1){
  print("se llego al numero maximo de iteraciones")
}

