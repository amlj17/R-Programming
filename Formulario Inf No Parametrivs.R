################
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test
#Prueba de Pearson, Spearman, Tau de Kendall
################

#Introducción
data(women)
women
cor(women$height, women$weight)

windows()
pairs(women$height ~ women$weight)  #permite elaborar un plot de correlacion

shapiro.test(women$height)
shapiro.test(women$weight)

#Correlación de Pearson
cor.test(women$height, women$weight, method = "pearson", alternative = "two.sided")
#si alternative="two.sided" 
#H0: corr=0
#vs
#H1: corr<>0
#si alternative="less" 
#H0: corr>=0
#vs
#H1: corr<0
#si alternative="greater" 
#H0: corr<=0
#vs
#H1: corr>0

#Correlación de Spearman
cor.test(women$height, women$weight, method = "spearman", alternative = "two.sided")

#Correlación de Tau de kendall
cor.test(women$height, women$weight, method = "kendall", alternative = "two.sided")

#Si hay normalidad en las dos variables usaré Pearson, pero si en una no hay normalidad uso spearman

################
# https://www.rdocumentation.org/packages/snpar/versions/1.0/topics/runs.test
# Prueba de aleatoriedad
################

library(randtests)
library(snpar)

x <- rnorm(100)
runs.test(x)

################
#https://rdrr.io/cran/DescTools/man/KendallW.html
# W de Kendall
#################	

library(Kendall)
library(rcompanion)

x=round(runif(10,min=1,max=10))
y=round(runif(10,min=1,max=10))
z=round(runif(10,min=1,max=10))

A<-data.frame(x,y,z)
A

K<-kendallW(A, correct = FALSE, test = FALSE, na.rm = FALSE)
K

################
#http://faculty.cas.usf.edu/mbrannick/regression/Part3/Partials.html
#https://cran.r-project.org/web/packages/ppcor/ppcor.pdf
# Correlación parcial
#################
x <-rnorm(100, 2, 5)
y <-rbeta(100, 2, 15)
z <-rgamma(100, 2, 20)

library(ppcor)

pcor.test(x,y,z,method=c("pearson"))
pcor.test(x,y,z,method=c("spearman"))
pcor.test(x,y,z,method=c("kendall"))

################
#   
# Medidas de asociación
#################	

v=round(runif(5,min=1,max=4))
w=round(runif(5,min=6,max=10))
x=round(runif(5,min=6,max=10))
y=round(runif(5,min=6,max=10))
z=round(runif(5,min=6,max=10))

# Tabla de contingencia con al menos 5 unidades en cada celda
A<-data.frame(w,x,y,z)
A

# Tabla de contingencia con celdas con menos 5 unidades 
B<-data.frame(v,x,y)
B

#a) Test Ji cuadrada de independencia de Pearson
test.independencia <- chisq.test(A)
test.independencia
test.independencia$statistic

#b) Test de independencia de Fisher (al menos una celda tiene menos de 5 observaciones)
fisher.test(B)

# Las siguientes solo son medidas de asociación, no pruebas de hipótesis
#c) Phi
Phi = sqrt(test.independencia$statistic/sum(A))
Phi

#d) V de Cramer
VCramer = sqrt(test.independencia$statistic/(min((ncol(A)-1),(nrow(A)-1))))
VCramer

#e) Contingencia
Contingencia <- sqrt(test.independencia$statistic/(test.independencia$statistic+sum(A)))
Contingencia

#f) Sakoda 
q <- min(nrow(A),ncol(A))
Sakoda = sqrt(test.independencia$statistic*q/((q-1)*(test.independencia$statistic+sum(A))))
Sakoda

################
#https://www.cienciadedatos.net/documentos/17_mann%E2%80%93whitney_u_test#:~:text=El%20test%20de%20Mann%E2%80%93Whitney,muestras%20proceden%20de%20poblaciones%20equidistribuidas.
#https://www.cienciadedatos.net/documentos/18_prueba_de_los_rangos_con_signo_de_wilcoxon
# Wilcoxon
#################

x <- rbeta(100,5,2)
y <- rgamma(100,1,2)

#a) Contraste de mediana para una muestra 
wilcox.test(x, mu=4, alternative = "less")

#b) Contraste de medianas para dos muestras pareadas
wilcox.test(x, y, paired = TRUE, alternative = "greater")

#c) Contraste de medianas para dos muestras independientes (U de Mann–Whitney)
wilcox.test(x, y, paired = FALSE, alternative = "two.sided",mu=5)
#H0: MeH-MeM=5
#vs
#H1: MeH-MeM<>5

