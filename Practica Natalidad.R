#Directorio de Trabajo
setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II")
# Instalar paquetes y librerías requeridos
InsPack = function (pack){
  if (pack %in% rownames (installed.packages ()) == FALSE) {
    print (paste("Instalando", pack))
    install.packages (pack)}
}

InsPack("data.table")
#install.packages("data.table")
library(data.table)

# Lectura de datos y guardado en formato de R
datos<-fread("sinac_2020.csv")
dim(datos)
head (datos)
save (datos, file = "natalidad.RData")

#Lectura de base de datos
natalidad<-get(load("natalidad.RData"))
dim(natalidad)
head(natalidad)
attach(natalidad)
str(natalidad)

natalidad<-natalidad[order(ENTIDADNACIMIENTO),]
natalidadBJ<-subset(natalidad, ENTIDADNACIMIENTO==9 & MUNICIPIONACIMIENTO==14 & PESO<9999 & TALLA<99)

#1. Existe normalidad en la variable de Peso del recién nacido.
#H0: Existe normalidad en la variable del peso de los recién nacidos de la alcaldía Benito Juarez
#vs
#H1: No Existe normalidad en la variable del peso de los recién nacidos de la alcaldía Benito Juárez
#Primero veamos el histograma
hist(natalidadBJ$PESO, xlab = "Peso (gramos)", ylab = "Frecuencia", main ="Peso Recién Nacidos Benito Juárez", col = "steelblue" )
#Aplicamos Prueba de Normalidad
#Jarque-BERA
library(normtest)
jb.norm.test(natalidadBJ$PESO)

#2 La correlación entre el Peso y la Talla es positiva.
#Veamos el histograma de la Talla
hist(natalidadBJ$TALLA, xlab = "Talla (cm)", ylab = "Frecuencia", main ="Talla Recién Nacidos Benito Juárez", col = "orange" )
jb.norm.test(natalidadBJ$TALLA)
#No existe normalidad en ninguna de las variables entonces utilizamos la correlacion de Spearman
#H0: Correlación entre el peso y la talla es positiva (rho >= 0)
#vs
#H1: Correlacion entre el peso y la talla no es positiva (rho<0)
#generemos un grafico de dispersion
plot(natalidadBJ$PESO, natalidadBJ$TALLA, ylab = "Talla (cm)", xlab = "Peso (gramos)", main = "Peso y Talla Recién Nacidos")
#Aplicamos Spearman
#si alternative="less" 
#H0: corr>=0
#vs
#H1: corr<0
#Correlación de Spearman
cor.test(natalidadBJ$PESO, natalidadBJ$TALLA, method = "spearman", alternative = "less")

#3 La hora de nacimiento es aleatoria.
natalidadHora2<-subset(natalidad, ENTIDADNACIMIENTO==9 & MUNICIPIONACIMIENTO==14 & HORANACIMIENTO!="99:99")
#Quitamos los dos puntos // cambiamos
natalidadHora2$HORANACIMIENTO<-sapply(strsplit(natalidadHora2$HORANACIMIENTO,":"),
       function(x){
         x<-as.numeric(x)
         x[1]+x[2]/60
       })
natalidadHora2$HORANACIMIENTO=as.numeric(natalidadHora2$HORANACIMIENTO)
L<-max(natalidadHora2$HORANACIMIENTO)-min(natalidadHora2$HORANACIMIENTO)+1
#veamos un histograma
hist(natalidadHora2$HORANACIMIENTO, main = "Hora de Nacimiento Alcaldía Benito Juárez", xlab = "Hora", ylab = "frecuencia", col = "salmon", xlim = c(0,24), breaks = L)
# Prueba de aleatoriedad
#install.packages("randtests")
library(randtests)
#install.packages("snpar")
library(snpar)
#H0: La Hora de Nacimiento es Aleatoria
#vs
#H1: La Hora de nacimiento no es aleatoria.
runs.test(natalidadHora2$HORANACIMIENTO)

#4. 4.- Es independiente la Escala Apgar a la Silverman.
#Se utiliza la prueba ji cuadrada de independencia
#Eliminamos outliers
natalidadBJ2 <- subset(natalidad, ENTIDADNACIMIENTO==9 & MUNICIPIONACIMIENTO==14 & APGAR<99 & SILVERMAN<99)
#Veamos un histograma de cada uno
hist(natalidadBJ2$APGAR, xlab = "Escala Apgar", ylab = "Frecuencia", main = "Escala Apgar Recién Nacidos Benito Juárez", col = "blue")
hist(natalidadBJ2$SILVERMAN, xlab = "Escala Silverman", ylab = "Frecuencia", main = "Escala Silverman Recién Nacidos Benito Juárez", col = "orange")
#HO: Hay Independencia entre la escala Apgar y la Escala Silverman
#vs.
#H1: Hay dependencia entre la escala Apgar y la Escala Silverman
#a) Test Ji cuadrada de independencia de Pearson
A<-table(natalidadBJ2$APGAR, natalidadBJ2$SILVERMAN)
A
matriz <-rbind(c(35,2),c(5591,19))
c<-as.table(matriz)
c
fisher.test(c)
punto4F<-fisher.test(c)$estimate

#5 5.- Es independiente la Escala Apgar a la Silverman en hijos cuyas madres son
#menores de 15 años.
#solo madres menores de 15 años
natalidadBJ3 <- subset(natalidadBJ2, EDAD<15)
hist(natalidadBJ3$APGAR, xlab = "Escala Apgar", ylab = "Frecuencia", main = "Escala Apgar Recién Nacidos Madres menores de 15 años", col = "blue")
hist(natalidadBJ3$SILVERMAN, xlab = "Escala Silverman", ylab = "Frecuencia", main = "Escala Silverman Recién Nacidos Madres menores de 15 años", col = "orange")
#HO: Hay Independencia entre la escala Apgar y la Escala Silverman para madres menores de 15 años.
#vs.
#H1: Hay dependencia entre la escala Apgar y la Escala Silverman para madres menores de 15 años.
B<-table(natalidadBJ3$APGAR, natalidadBJ3$SILVERMAN)
B
matriz2 <-rbind(c(0,2),c(3,1))
w<-as.table(matriz2)
w
fisher.test(w)
punto5F<-fisher.test(w)$estimate

#6 6.- El Sakoda del punto 4 es mayor que el del punto 5 (Nota es simplemente
#una comparación numérica).
#en ambos casos son tablas de 2X2 entonces 
#punto 5
q1<-min(nrow(c), ncol(c))
Sakoda4 = sqrt(punto4F*q1/((q1-1)*(punto4F+sum(c))))
Sakoda4
q2<-min(nrow(w), ncol(w))
sakoda5 =sqrt(punto5F*q2/((q2-1)*(punto5F+sum(c))))

sakoda5
Sakoda4

#7. 7.- La correlación parcial entre Talla y Peso, quitando el efecto de Edad gestacional 
#es positiva y significativa.
#quitamos outliers
natalidadBJ4<-subset(natalidadBJ, EDADGESTACIONAL<99)
hist(natalidadBJ4$PESO, xlab = "Peso (gramos)", ylab = "Frecuencia", main ="Peso Recién Nacidos Benito Juárez", col = "steelblue")
hist(natalidadBJ4$TALLA, xlab = "Talla (cm)", ylab = "Frecuencia", main ="Talla Recién Nacidos Benito Juárez", col = "orange")
hist(natalidadBJ4$EDADGESTACIONAL, xlab = "Edad Gestacional (semanas)", ylab = "Frecuencia", main ="Edad Gestacional Recién Nacidos Benito Juárez", col = "red")
library(ppcor)
#Usamos spearman
pcor.test(natalidadBJ4$PESO, natalidadBJ4$TALLA, natalidadBJ4$EDADGESTACIONAL, method = "spearman")

#8.- El peso promedio de los neonatos es mayor que 2500 gramos.
jb.norm.test(natalidadBJ$PESO)
#No hay normalidad
#H0 la mediana del peso de los neonatos es menor o igual a 2500 gramos
#vs.
#H1 la mediana del peso del los neonatos es mayor a 2500 gramos
#a) Contraste de mediana para una muestra 
wilcox.test(natalidadBJ$PESO, mu=2500, alternative = "greater")

#9 El peso promedio de las niñas es menor que el de los niños.
#H0: Mediana Niñas es mayor o igual a la de los niños
#vs. 
#H1: Mediana Niñas es menor que el de los niños
natalidadH <- subset(natalidadBJ, SEXO==1)
natalidadM <- subset(natalidadBJ, SEXO==2)
#veamos los histogramas:
hist(natalidadH$PESO, main = "Peso Hombres Recién Nacidos", xlab = "Peso (gramos)", ylab = "Frecuencia", col = "blue")
hist(natalidadM$PESO, main = "Peso Mujeres Recién Nacidos", xlab = "Peso (gramos)", ylab = "Frecuencia", col = "orange")
#veamos si existe normalidad
jb.norm.test(natalidadH$PESO)
jb.norm.test(natalidadM$PESO)
#No existe entonces hacemos contraste de medianas
wilcox.test(natalidadM$PESO, natalidadH$PESO, paired = FALSE, alternative = "less")

#10 La diferencia en promedios de edades de los padres es mayor a 5 años.
#Primero seleccionemos edades de los padres
natalidadBJP<-subset(natalidad, ENTIDADNACIMIENTO==9 & MUNICIPIONACIMIENTO==14 & EDADPADRE<80)
natalidadBJM<-subset(natalidad, ENTIDADNACIMIENTO==9 & MUNICIPIONACIMIENTO==14 & EDAD <99)
#veamos los histogramas:
hist(natalidadBJP$EDADPADRE, xlab = "EDAD", ylab ="frecuencia", main = "Edad del Padre", col = "green")
hist(natalidadBJM$EDAD, xlab = "EDAD", ylab ="frecuencia", main = "Edad de la Madre", col = "blue")
library(normtest)
jb.norm.test(natalidadBJP$EDADPADRE)
jb.norm.test(natalidadBJM$EDAD)
wilcox.test(natalidadBJP$EDADPADRE, natalidadBJM$EDAD, paired = FALSE, alternative = "greater",mu=5)
wilcox.test(natalidadBJP$EDADPADRE, natalidadBJP$EDAD, paired = TRUE, alternative = "greater",mu=5)

#la doferencia de las medias de puntos obtenidos como local en torneos con publico y sin publico es (> < = ) a 2.5 pts


#la doferencia de las medias de puntos obtenidos como visitante en torneos con publico y sin publico es (> < = ) a 2.5 pts
