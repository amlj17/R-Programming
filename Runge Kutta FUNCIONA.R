#Definimos las funciones dadas
#u1'
f1 <- function(t,u1,u2){3*u1+2*u2-(2*(t^(2))+1)*exp(2*t)}
#u2'
f2 <- function(t,u1,u2){4*u1+u2+((t^(2))+2*t-4)*exp(2*t)}

funciones<-c(f1,f2)

A<-readline("Introducir el valor minimo del intervalo: ")
a<-as.numeric(A)
B<-readline("Introducir el valor maximo del intervalo: ")
b<-as.numeric(B)
ALPHA1<-readline("Introducir el numero alpha (condicion inicial) dado: ")
alpha1<-as.numeric(ALPHA1)
ALPHA2<-readline("Introducir el numero alpha (condicion inicial) dado: ")
alpha2<-as.numeric(ALPHA2)
N<-readline("Introducir el numero de puntos: ")
N<-as.numeric(N)

#numero de ecuaciones
m<-2

h<-(b-a)/N
t<-a
w<-c(alpha1,alpha2)

tabla<-matrix(nrow=N+1,ncol = 3)
tabla[1,1]<-t
tabla[1,2]<-w[1]
tabla[1,3]<-w[2]

#definir K
k1<-c(h*f1(t,w[1],w[2]),h*f2(t,w[1],w[2]))
k2<-c(h*f1(t+(h/2),w[1]+(k1[1])/2,w[2]+(k1[2])/2),h*f2(t+(h/2),w[1]+(k1[1])/2,w[2]+(k1[2])/2))
k3<-c(h*f1(t+(h/2),w[1]+(k2[1])/2,w[2]+(k2[2])/2),h*f2(t+(h/2),w[1]+(k2[1])/2,w[2]+(k2[2])/2))
k4<-c(h*f1(t+h,w[1]+k3[1],w[2]+k3[2]),h*f2(t+h,w[1]+k3[1],w[2]+k3[2]))


#paso 3
for(i in 1:N){
  #P4
  for(j in 1:m){
    k1[j]<-h*funciones[[j]](t,w[1],w[2])
  }
  #P5
  for(j in 1:m){
    k2[j]<-h*funciones[[j]](t+(h/2),w[1]+(k1[1])/2,w[2]+(k1[2])/2)
  }
  #p6
  for(j in 1:m){
    k3[j]<-h*funciones[[j]](t+(h/2),w[1]+(k2[1])/2,w[2]+(k2[2])/2)
  }
  #p7
  for(j in 1:m){
    k4[j]<-h*funciones[[j]](t+h,w[1]+(k3[1]),w[2]+(k3[2]))
  }
  #p8
  for(j in 1:m){
   w[j]<-w[j]+(k1[j]+2*k2[j]+2*k3[j]+k4[j])/6 
  }
  t<-a+i*h
  tabla[i+1,1]<-t
  tabla[i+1,2]<-w[1]
  tabla[i+1,3]<-w[2]
}

#Definimos las funciones reales o sols analiticas
fAnalyt1 <- function(t){1/3*exp(5*t)-(1/3)*exp(-t)+exp(2*t)}
fAnalyt2 <- function(t){1/3*exp(5*t)+(2/3)*exp(-t)+t^2*exp(2*t)}

colnames(tabla) <- c('t','w1','w2')
tablavals <- as.data.frame((tabla))

x <- seq(from=a,to=b, by=h) 

plot(x,fAnalyt1(x), col="green", type = 'l')

ggplot(tablavals, aes(x=t,y=w2))+
  geom_line(color="red")

plot(x,fAnalyt2(x),type = 'l')

ggplot(tablavals, aes(x=t,y=w1))+
  geom_line(color="blue")