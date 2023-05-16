#Algoritmo de Jacobi
ec <- readline(prompt="Dame el numero de ecuaciones: ")
ec <- as.numeric(ec)
num <- readline(prompt="Dame el numero de iteraciones: ")
num <- as.numeric(num)
tol <- 10^-5
A=matrix(c(4,-1,2,-1,0,1,-3,1,-1,2,1,1,5,-1,-1,0,1,-1,4,1,1,0,-1,0,4),,5)
B <- c(6,6,6,6,6)
X0 <- matrix(c(0,0,0,0,0),,1)
k=1
x=0

while(k <= num)
{
  for(i in 1: ec)
  {
    suma1=0
    l <- i-1
    j=1
    #evitamos a i llegando a 1 menos que i con l
    while(j<=l)
    {
      
      suma1=suma1+(A[i,j]*X0[j])
      
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
    
    x[i] <- (1/A[i,i])*(suma)
  }
  
  
  if( max(abs(x-X0))/max(abs(x)) < tol)
  {
    
    print("La solucion es: ")
    print(x)
    break
  }
  
  X0 <- x
  k <- k+1
  x <- 0
  
}

if(k==num+1)
{
  print("Excedio el numero de iteraciones")
  
}