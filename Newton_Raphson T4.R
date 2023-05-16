#Tarea 4 
#Andres Moguel
#Newton-Raphson

#Comenzamos con la definicion de la funcion
f <- function(x){
  2*x*cos(2*x)-(x+1)^2
}

#definimos la derivada de la funcion
der <- function(x){
  -2*(1 + x - cos(2*x) + 2*x*sin(2*x))
}

plot(f, col = 'orange')
plot(der, col = "blue")
#definimos la presicion
tol <- 10^-6

#Pedimos las entradas al programa: aproximacion inicial y numero de iteraciones
num <- as.numeric(readline("Ingrese el numero maximo de iteraciones: "))
p0 <- as.numeric(readline("Ingrese la primer aproximacion inical: "))

#inicializamos el contador
i <- 1

while (i <= num) {
  p <- p0 - (f(p0)/der(p0))
  
  if (abs(p - p0)<= tol || f(p) == 0){
    print("La aproximación a la raiz es: ")
    print(p)
    break
  }
  i <- i+1
  p0 <- p
}

if(i == num +1){
  print("se llego al numero maximo de iteraciones")
}