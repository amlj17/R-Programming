setwd("~/Desktop/UIA/7mo Semestre UIA/Regresión")
autos = read.csv("cars.csv", header = TRUE, sep = ",")
str(autos)
#1
Modelo = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11, data = autos)
summary(Modelo)

#2
Modelo2 = lm(y ~ x1 + x6, data = autos)
summary(Modelo2)

#Añadimos variables
Modelo3 = lm(y ~ x1 + x2 + x6 + x10 + x11, data = autos)
summary(Modelo3)

#3
anova(Modelo2)

