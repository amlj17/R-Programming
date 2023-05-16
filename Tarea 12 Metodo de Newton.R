#Tarea 12
#Andres Moguel
#Newton No Lineales
f1 <- function(x1,x2,x3){x1^2+x2-37}
f2 <- function(x1,x2,x3){x1-x2^2-5}
f3 <- function(x1,x2,x3){x1+x2+x3-3}
j11 <- function(x1,x2,x3){2*x1}
j12 <- function(x1,x2,x3){1}
j13 <- function(x1,x2,x3){0}
j21 <- function(x1,x2,x3){1}
j22 <- function(x1,x2,x3){-2*x2}
j23 <- function(x1,x2,x3){0}
j31 <- function(x1,x2,x3){1}
j32 <- function(x1,x2,x3){1}
j33 <- function(x1,x2,x3){1}

#APROX INICIAL
X0 <- c(0,0,0)
Fx <- matrix(1,3,1)
Y <- matrix(1,3,1)
J <- matrix(1,3,3)
#numero de ecuaciones
n <- 3
N0<-readline(prompt="Ingrese el numero de iteraciones a realizar: ")
N0<-(as.numeric(N0)) 
Tol<-10^-5 

k <- 1
while(k<=N0)
{
  Fx[1,1] <- f1(X0[1],X0[2],X0[3])
  Fx[2,1] <- f2(X0[1],X0[2],X0[3])
  Fx[3,1] <- f3(X0[1],X0[2],X0[3])
  
  J[1,1] <- j11(X0[1],X0[2],X0[3])  
  J[1,2] <- j12(X0[1],X0[2],X0[3])  
  J[1,3] <- j13(X0[1],X0[2],X0[3])  
  J[2,1] <- j21(X0[1],X0[2],X0[3])  
  J[2,2] <- j22(X0[1],X0[2],X0[3]) 
  J[2,3] <- j23(X0[1],X0[2],X0[3])  
  J[3,1] <- j31(X0[1],X0[2],X0[3])  
  J[3,2] <- j32(X0[1],X0[2],X0[3])  
  J[3,3] <- j33(X0[1],X0[2],X0[3])  
  
  Y <- solve(J) %*% (-Fx)
  X0 <- X0+Y
  
  if (max(Y)<Tol)
  {
    print("La Aproximacion al sistema es:")
    print(X0)
    break
  }
  
  k <- k+1
  
}


if(k==N0+1)
{
  print("Fin, numero máximo de iteraciones")
}
