# Tarea 11
#Punto fijo

g1 <- function(x2,x3){1-cos(x2*x3)}
g2 <- function(x1,x3){1-(1-x1)^(1/4)-0.05*((x3)^2)+0.15*x3}
g3 <- function(x1,x2){x1^2+0.1*(x2)^2-0.01*x2+1}
Po <- c(0.05,0.1,0.6)
P <- Po
N0<-readline(prompt="ingrese el numero de iteraciones: ")
N0<-(as.numeric(N0))
Tol<-10^(-5)
i<-1
contador <- 0
while(i<=N0)
{
  P[1]<-g1(Po[2], Po[3])
  P[2]<-g2(Po[1], Po[3])
  P[3]<-g3(Po[1], Po[2])
  if(((P[1]==g1(P[2], P[3]) && P[2]==g2(P[1], P[3]) && P[3]==g3(P[1], P[2]))))
  {
    contador<- 1
  }
  if( (max(abs(P-Po)) <= Tol) |  (contador == 1))
  {
    print("La aproximacion al sistema es:")
    print(P)
    print("con iteraciones:")
    print(i)
    break;
  }
  i=(i+1)
  Po<-P
}
if(i==N0+1)
{
  print("fin, numero maximo de iteraciones")
}