#comenzemos definiendo la funcion
f <- function(x){2*x*cos(2*x)-(x+1)^2}
#graficamos f
plot(f)

#Justificacion de intervalos
#viendo esta primer grafica se recomienda escoger intervalos entre 0 y 1
#la grafica parece cortar el eje x entre estos valores a simple vista
#ejemplos
# a = 0 , b =1
# a = 0.5  b = 1

tol <- 10^-6
num <- as.numeric(readline("Ingrese el numero maximo de iteraciones: "))
a <- as.numeric(readline("Ingrese el extremo inferior de su intervalo: "))
b <- as.numeric(readline("Ingrese el extremo superior de su intervalo (debe ser mayor que el inferior) : "))

i <- 1
while(i<=num){
  x <- (a+b)/2
  w <- abs(a-b)
  if(w <= tol | f(x) == 0){
    print("La aproximación a la raíz es: ")
    print(x)
    plot(f, a,b, col = "blue")
    break
  }
  if((f(a)*f(x)) < 0){
    b <- x
  }else{
    a <- x
  }
  i<- (i+1)
}
if(i == num+1){
  print("se llego al numero maximo de iteraciones")
}

