setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II/Trabajo en R/R IE II")
if(!require(readr)) install.packages("readr")
if(!require(foreign)) install.packages("foreign")
if(!require(dplyr)) install.packages("dplyr")
library(readr)
library(foreign)
library(dplyr)

# Lectura de datosen formato de R
PER_F23<- read.dbf("PER_F23.dbf")
dim(PER_F23)
str(PER_F23)
names(PER_F23)

sum(as.numeric(PER_F23$FACTOR))

PER_F23=select(PER_F23,c("FACTOR","EDAD"))
class(PER_F23$FACTOR)="Numeric"
class(PER_F23$EDAD)="Numeric"
#Se expande la tabla de frecuencia con libreria vcdExtra funcion expand.dft
library(vcdExtra)
prueba<-expand.dft(PER_F23,var.names = NULL,freq = "FACTOR")
detach("package:vcdExtra", unload = TRUE)
detach("package:vcd", unload = TRUE)

#Histograma
hist(prueba$EDAD, main = "Histograma edades", xlab = "Edades Edo 23", col="blue")

#Cramer-Von Mises
library(goftest)
cvm.test(prueba$EDAD, "pnorm")
detach("package:goftest", unload = TRUE)

#KS Test
library(dgof)
ks.test(prueba$EDAD, "pnorm")
detach("package:dgof", unload = TRUE)

#Ji Cuadrada
library(nortest)
pearson.test(prueba$EDAD)
detach("package:nortest", unload = TRUE)

#Jarque-BERA
library(normtest)
jb.norm.test(prueba$EDAD)
detach("package:normtest", unload = TRUE)
