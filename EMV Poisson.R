#Generacion de la grafica de la funciÛn de verosimilitud de una VA Poisson
#EstimaciÛn de la varianza

datos <- rpois(100,1)
hist(datos,col="green", freq=F)
n <- length(datos)
EMV<-sum(datos)/n
EMV

# FunciÛn de verosimilitud para una VA poisson
v.g<- function(y,lambda,n){
  #Nota si se hacia vero <-- no funcionaba por esto asi se pone el return
  return(-n*lambda + log(lambda)*sum(y) - sum(log(factorial(y))))
}

# Estimacion
estima <- optim(c(1),v.g,method="BFGS",control=list(fnscale=-1),y=datos,n=100)   
estima$par
estima$value
estima$convergence

#Gr·fica de la funcion de verosimilitud
sec<- seq(0, 10, 0.01)

logvero<- v.g(y=datos,lambda=sec,n=100)
quartz()
plot(sec, logvero, type="l", lwd=2, lty=1, col="red", xlab="lambda", ylab="fn de verosimilitud en lambda")
abline(v=estima$par,col="blue")

#Replica de la estimaciÛn k veces
k<-100000
vecpar<-numeric(k)
for (i in 1:k)
{
  
  datos <- rpois(100,1)
  
  # Estimacion
  estima <- optim(c(1),v.g,method="BFGS",control=list(fnscale=-1),y=datos,n=100)   
  estima$par
  vecpar[i] <- estima$par
  
  #Gr·fica de la funcion de verosimilitud
  sec<- seq(0, 20, 0.01)
  logvero<- v.g(y=datos,lambda=sec,n=100)
  abline(v=estima$par,col="blue",add=T)
  #Estimaciones teÛricas que acotan a las experimentales
  abline(v=EMV,col="red",lwd=2, add=T)
  abline(v=EMV-1.96*sqrt(EMV/n),col="red",lwd=2, add=T)
  abline(v=EMV+1.96*sqrt(EMV/n),col="red",lwd=2, add=T)
}

sd<- sd(vecpar)
quartz()
hist(vecpar,col="skyblue", freq=T,breaks=sqrt(k),main="DistribuciÛn de las estimaciones de lambda^2",
     xlab="lambda",ylab="Frecuencia absoluta",border="aliceblue",ylim=c(0,1200))
abline(v=mean(vecpar)+sd,col="red",lwd=2)
axis(1, at=mean(vecpar)+sd,labels=sprintf("%.3f",mean(vecpar)+sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar)-sd,col="red",lwd=2)
axis(1, at=mean(vecpar)-sd,labels=sprintf("%.3f",mean(vecpar)-sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar)+2*sd,col="green4",lwd=2)
axis(1, at=mean(vecpar)+2*sd,labels=sprintf("%.3f",mean(vecpar)+2*sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar)-2*sd,col="green4",lwd=2)
axis(1, at=mean(vecpar)-2*sd,labels=sprintf("%.3f",mean(vecpar)-2*sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar)+3*sd,col="purple",lwd=2)
axis(1, at=mean(vecpar)+3*sd,labels=sprintf("%.3f",mean(vecpar)+3*sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar)-3*sd,col="purple",lwd=2)
axis(1, at=mean(vecpar)-3*sd,labels=sprintf("%.3f",mean(vecpar)-3*sd),mgp=c(0,0.1,0),cex.axis=0.7)
abline(v=mean(vecpar),col="black",lwd=1.5,lty=2)
axis(1, at=mean(vecpar),labels=sprintf("%.3f",mean(vecpar)),mgp=c(0,0.1,0),cex.axis=0.7)
arrows(mean(vecpar)-sd,600,mean(vecpar)+sd,600,col="gold3",lwd=2,code=3) 
arrows(mean(vecpar)-2*sd,400,mean(vecpar)+2*sd,400,col="gold3",lwd=2,code=3) 
arrows(mean(vecpar)-3*sd,200,mean(vecpar)+3*sd,200,col="gold3",lwd=2,code=3)
text(x=mean(vecpar),y=610, labels="aprox   68%")
text(x=mean(vecpar),y=410, labels="aprox   95%") 
text(x=mean(vecpar),y=210, labels="aprox   99%") 
