setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II/Trabajo en R/R IE II")
if(!require(readr)) install.packages("readr")
if(!require(foreign)) install.packages("foreign")
if(!require(dplyr)) install.packages("dplyr")
library(readr)
library(foreign)
library(dplyr)
n=1000
set.seed(65)
x<-rgamma(n,2,3)
hist(x, main = "Gamma")

x<-sort(x, decreasing = F)
pvaloresizq<-vector(mode="numeric", length = n)
pvaloresizqSW<-vector(mode="numeric", length = n)
pvaloresder<-vector(mode="numeric", length = n)
pvaloresderSW<-vector(mode="numeric", length = n)

#pearson
library(nortest)

for(i in 1:n){
  limite=n-i
  b1=x[i:n]
  b2=x[1:limite]
  rizq = pearson.test(b1)
  riSW = shapiro.test(b1)
  rder = pearson.test(b2)
  rdSW = shapiro.test(b2)
  pvaloresizq[i]=rizq$p.value
  pvaloresder[i]=rder$p.value
  pvaloresizqSW[i]=riSW$p.value
  pvaloresderSW[i]=rdSW$p.value
}

par(mfrow=c(2,1))
plot(pvaloresizq, type = "l", main = "Extracciones por la izquierda", col="blue")
lines(pvaloresizqSW, type = "l",col="cadetblue1")
abline(h=0.05, col="orange")
abline(h=0.01, col="red")
abline(h=0.1, col = "green")
plot(pvaloresder, type = "l", main = "Extracciones por la derecha", col="blue")
lines(pvaloresizqSW, type = "l", col="cadetblue1")
abline(h=0.05, col="orange")
abline(h=0.01, col="red")
abline(h=0.1, col = "green")



