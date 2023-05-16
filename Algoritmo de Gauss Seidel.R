#Algoritmo de Gauss Seidel
ec <- readline(prompt="Dame el numero de ecuaciones: ")
ec <- as.numeric(ec)
N0 <- readline(prompt="Dame el numero de iteraciones: ")
N0 <- as.numeric(N0)
tol <- 10^-5
A=matrix(c(4,-1,2,-1,0,1,-3,1,-1,2,1,1,5,-1,-1,0,1,-1,4,1,1,0,-1,0,4),,5)
B <- c(6,6,6,6,6)
X0 <- matrix(c(0,0,0,0,0),,1)
k=1
X1=0

while(k <= N0)
{
  for(i in 1: ec)
  {
    suma1=0
    l <- i-1
    j=1
    #evitamos a i llegando a 1 menos que i con l
    #aqui asignamos los de X1 en vez de X0
    while(j<=l)
    {
      
      suma1=suma1+(A[i,j]*X1[j])
      
      j <- j+1
      
    }
    suma2=0
    #evitamos i asignando ahora a j con i+1
    j <- i+1
    
    while(j<=ec)
    {
      
      suma2=suma2+(A[i,j]*X0[j])
      j <- j+1
      
    }
    
    suma <- suma1+suma2
    
    suma <- -suma + B[i]
    
    X1[i] <- (1/A[i,i])*(suma)
  }
  
  
  if( max(abs(X1-X0))/max(abs(X1)) < tol)
  {
    
    print("La solucion es: ")
    print(X1)
    break
  }
  
  X0 <- X1
  k <- k+1
  x <- 0
  
}

if(k==N0+1)
{
  print("Excedio el numero de iteraciones")
  
}