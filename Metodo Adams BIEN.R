{
  ftw <- function(t,w){
    (-5*w)+(5*t^2)+(2*t)
  }
  
  ft <- function(t){
    t^2+((1/3)*exp(-5*t))
  }
  
  A<-readline("Introducir el valor minimo del intervalo: ")
  a<-as.numeric(A)
  B<-readline("Introducir el valor maximo del intervalo: ")
  b<-as.numeric(B)
  N<-readline("Introducir el numero de puntos a aproximar: ")
  N<-as.numeric(N)
  
  h <- (b-a)/N
  t <- a
  k <- c(1)
  k[1] <- c(a)
  
  alfa<-readline("Introducir el valor inical dado: ")
  alfa<-as.numeric(alfa)
  
  w <- alfa
  p <- c(1)
  #cambiar alfa
  p[1] <- c(alfa)
  i <- 1
  print(p[1])
  
  while(i<=3)
  {
    K1<-h*ftw(t,w)
    K2<-h*ftw((t+(h/2)),(w+(K1/2)))
    K3<-h*ftw((t+(h/2)),(w+(K2/2)))
    K4<-h*ftw((t+h),(w+K3))
    
    w <- w+((K1+(2*K2)+(2*K3)+K4)/6)
    t <- a+(i*h)
    p[i+1] <- c(w)
    k[i+1] <- c(t)
    i <- i+1
  }
  
  
  while(i<=N)
  {
    t <- a+(i*h)
    w <- p[4]+(h/24)*((55*ftw(k[4],p[4]))-(59*ftw(k[3],p[3]))+(37*ftw(k[2],p[2]))-(9*ftw(k[1],p[1])))
    w <- p[4]+(h/24)*((9*ftw(t,w))+(19*ftw(k[4],p[4]))-(5*ftw(k[3],p[3]))+(ftw(k[2],p[2])))
    k[i+1] <- c(t)
    p[i+1] <- c(w)
    i <- i+1
    
    j=1
    while(j<=3)
    {
      k[j] <- k[j+1]
      p[j] <- p[j+1]
      k[4] <- t
      p[4] <- w
      j <- j+1
      
    }
  }
  
  print("La aproximacion es la siguiente:")
  cat(c("(", k[101], ",", p[101], ")"))
  print(" ")
  
  
  #C?lculo de la soluci?n Analitica
  y <- c(1)
  y[1] <- c(1)
  j=1
  while(j<=N+1)
  {
    y[j] <- c(ft(k[j]))
    j <- j+1
  }
  
  #calculo de errores
  t <- c(1)#absoluto
  t[1] <- c(1)
  d <- c(1)#real
  d[1] <- c(1)
  j=1
  
  errorA<-matrix(nrow = N+1,ncol = 1)
  errorR<-matrix(nrow = N+1,ncol = 1)
  while(j<=N+1)
  {
    errorA[j,1] <- abs(p[j+1]-p[j])
    errorR[j,1] <- abs(y[j]-p[j])
    j <- j+1
  }
  
  plot(k,p, col="blue")
  par(new=T)
  plot(ft)
}
