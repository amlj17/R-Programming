#Reaseguro 
#Pareto un solo riesgo
set.seed(2018)
theta = 1000
alpha = 3
nSim = 10000
library(actuar)
X <-  rpareto(nSim, shape = alpha, scale = theta)

par(mfrow=c(1,3))
plot(density(X), xlim=c(0,3*theta), ylim=c(0,0.008), main="Total Loss", xlab="Losses")
plot(density(0.75*X), xlim=c(0,3*theta), ylim=c(0,0.008), main="Insurer (75%)", xlab="Losses")
plot(density(0.25*X), xlim=c(0,3*theta), ylim=c(0,0.008), main="Reinsurer (25%)", xlab="Losses")

#Retension optima
#Ejercicio Pareto de 3 ci
theta1 = 1000; theta2 = 2000; theta3 = 3000;
alpha1 = 3; alpha2 = 3; alpha3 = 4;
library(actuar)
propnfct <- function(alpha,theta){
  mu    <- mpareto(shape=alpha, scale=theta, order=1)
  var   <- mpareto(shape=alpha, scale=theta, order=2) - mu^2
  mu/var
}
temp <- propnfct(alpha1, theta1)*mpareto(shape=alpha1, scale=theta1, order=1)+
  propnfct(alpha2, theta2)*mpareto(shape=alpha2, scale=theta2, order=1)+
  propnfct(alpha3, theta3)*mpareto(shape=alpha3, scale=theta3, order=1)  
KVec <- seq(100, 2500, length.out=20)
Lambdavec <- 2*KVec/temp
c1 <- propnfct(alpha1, theta1)
c2 <- propnfct(alpha2, theta2)
c3 <- propnfct(alpha3, theta3)
c1Vec <- c2Vec <- c3Vec <- 0*KVec 
for (j in 1:20) {
  c1Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha1, theta1)
  c2Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha2, theta2)
  c3Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha3, theta3)
}
plot(KVec, c1Vec, type="l", ylab="proportion", xlab="required revenue (K)", ylim=c(0,1))
lines(KVec, c2Vec)
lines(KVec, c3Vec)
theta1 = 1000; theta2 = 2000; theta3 = 3000;
alpha1 = 3; alpha2 = 3; alpha3 = 4;
library(actuar)
propnfct <- function(alpha,theta){
  mu    <- mpareto(shape=alpha, scale=theta, order=1)
  var   <- mpareto(shape=alpha, scale=theta, order=2) - mu^2
  mu/var
}
temp <- propnfct(alpha1, theta1)*mpareto(shape=alpha1, scale=theta1, order=1)+
  propnfct(alpha2, theta2)*mpareto(shape=alpha2, scale=theta2, order=1)+
  propnfct(alpha3, theta3)*mpareto(shape=alpha3, scale=theta3, order=1)  
KVec <- seq(100, 2500, length.out=20)
Lambdavec <- 2*KVec/temp
c1 <- propnfct(alpha1, theta1)
c2 <- propnfct(alpha2, theta2)
c3 <- propnfct(alpha3, theta3)
c1Vec <- c2Vec <- c3Vec <- 0*KVec 
for (j in 1:20) {
  c1Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha1, theta1)
  c2Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha2, theta2)
  c3Vec[j] <- (Lambdavec[j]/2) * propnfct(alpha3, theta3)
}
plot(KVec, c1Vec, type="l", ylab="proportion", xlab="required revenue (K)", ylim=c(0,1))
lines(KVec, c2Vec)
lines(KVec, c3Vec)

#Ejercicio de Manejo de Portafolios
# For the gamma distributions, use
alpha1 <- 2;      theta1 <- 100
alpha2 <- 2;      theta2 <- 200
# For the Pareto distributions, use
alpha3 <- 2;      theta3 <- 1000
alpha4 <- 3;      theta4 <- 2000
# Limits
M1     <- 100
M2     <- 200

# Simulate the risks
nSim <- 10000  #number of simulations
set.seed(2017) #set seed to reproduce work 
X1 <- rgamma(nSim,alpha1,scale = theta1)  
X2 <- rgamma(nSim,alpha2,scale = theta2)  
# For the Pareto Distribution, use
library(actuar)
X3 <- rpareto(nSim,scale=theta3,shape=alpha3)
X4 <- rpareto(nSim,scale=theta4,shape=alpha4)
# Portfolio Risks
X         <- X1 + X2 + X3 + X4
Yretained <- pmin(X1,M1) + pmin(X2,M2)
Yinsurer  <- X - Yretained

# Expected Claim Amounts
ExpVec <- t(as.matrix(c(mean(Yretained),mean(Yinsurer),mean(X))))
colnames(ExpVec) <- c("Retained", "Insurer","Total")
round(ExpVec,digits=2)

# Quantiles
quantMat <- rbind(
  quantile(Yretained, probs=c(0.80, 0.90, 0.95, 0.99)),
  quantile(Yinsurer,  probs=c(0.80, 0.90, 0.95, 0.99)),
  quantile(X       ,  probs=c(0.80, 0.90, 0.95, 0.99)))
rownames(quantMat) <- c("Retained", "Insurer","Total")
round(quantMat,digits=2)

par(mfrow=c(1,3))
plot(density(Yretained), xlim=c(0,500), main="Retained Portfolio Risk", xlab="Loss (Note the different horizontal scale)", ylab = "Density (Note different vertical scale)")
plot(density(Yinsurer), xlim=c(0,15000), main="Insurer Portfolio Risk", xlab="Loss")
plot(density(X), xlim=c(0,15000), main="Total Portfolio Risk", xlab="Loss")


