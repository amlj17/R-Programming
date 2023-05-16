setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II/Trabajo en R/R IE II")
if(!require(readr)) install.packages("readr")
if(!require(foreign)) install.packages("foreign")
if(!require(dplyr)) install.packages("dplyr")
library(readr)
library(foreign)
library(dplyr)
library(normtest)
library(nortest)
library(dgof)

n=100
#set.seed(65)
x<-rgamma(n,1,2)
hist(x, main = "Gamma")

x<-sort(x, decreasing = F)
#weisberg
pvaloresizqWB<-vector(mode="numeric", length = n)
pvaloresderWB<-vector(mode="numeric", length = n)
#Spiegel
pvaloresizqSP<-vector(mode="numeric", length = n)
pvaloresderSP<-vector(mode="numeric", length = n)
#geary
pvaloresizqGR<-vector(mode="numeric", length = n)
pvaloresderGR<-vector(mode="numeric", length = n)
#Shapiro Francia
pvaloresizqSF<-vector(mode="numeric", length = n)
pvaloresderSF<-vector(mode="numeric", length = n)
#Kol SM
pvaloresizqKS<-vector(mode="numeric", length = n)
pvaloresderKS<-vector(mode="numeric", length = n)

z=rnorm(1000,0,1)
z=sort(z, decreasing = FALSE)

for(i in 1:n){
  limite=n-i
  #izq
  b1=x[i:n]
  #der
  b2=x[1:limite]
  
  #spiel
  rizqSP = spiegelhalter.norm.test(b1)$p.value
  rderSP = spiegelhalter.norm.test(b2)$p.value
  pvaloresizqSP[i]=rizqSP
  pvaloresderSP[i]=rderSP
  
  
  #geary
  rizqGR = geary.norm.test(b1)$p.value
  rderGR = geary.norm.test(b2)$p.value
  pvaloresizqGR[i]= rizqGR
  pvaloresderGR[i] = rderGR
  
  #Shapiro Francia
  rizqSF = sf.test(b1)$p.value
  rderSF = sf.test(b2)$p.value
  pvaloresizqSF[i] = rizqSF
  pvaloresderSF[i] = rderSF
  
  #KS
  rizqKS = ks.test(b1, "pnorm")$p.value
  rderKS = ks.test(b2, "pnorm")$p.value
  pvaloresizqKS[i]= rizqKS
  pvaloresderKS[i] = rderKS
  
  #weiseberg
  rizqWB = wb.norm.test(b1)$p.value
  rderWB = wb.norm.test(b2)$p.value
  pvaloresizqWB[i] = rizqWB
  pvaloresderWB[i] = rderWB
}

par(mfrow=c(2,1))
plot(pvaloresizqWB, type = "l", main = "Extracciones por la izquierda", col="blue", ylim = c(0,1))
lines(pvaloresizqSP, type = "l",col="cadetblue1")
lines(pvaloresizqGR, type = "l",col="chartreuse")
lines(pvaloresizqSF, type = "l",col="chocolate4")
lines(pvaloresizqKS, type = "l",col="blueviolet")
abline(h=0.05, col="orange")
abline(h=0.01, col="red")
abline(h=0.1, col = "black")
plot(pvaloresderWB, type = "l", main = "Extracciones por la derecha", col="blue", ylim = c(0,1))
lines(pvaloresderSP, type = "l", col="cadetblue1")
lines(pvaloresderGR, type = "l",col="chartreuse")
lines(pvaloresderSF, type = "l",col="chocolate4")
lines(pvaloresderKS, type = "l",col="blueviolet")
abline(h=0.05, col="orange")
abline(h=0.01, col="red")
abline(h=0.1, col = "black")
