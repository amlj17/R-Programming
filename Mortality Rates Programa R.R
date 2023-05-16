#se establece el directorio de trabajo
setwd("~/Desktop/3er Semestre UIA/Demografía")

# Instalar paquetes y librerías requeridos
InsPack = function (pack) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    print (paste("Instalando ",pack))
    install.packages(pack)}
} 
#Instala e invoca el paquete
InsPack("data.table")
library(data.table)

# asignar CSV a una variable
pob<-fread(file ="POB.csv")

# entre corchetes poner renglones, columnas si es en blanco elige todo
pob<-pob[1:86,]
# dim es para saber el tamaño de matrices
dim(pob)
# attach que existan objetos con nombres originales. Objetos con nombres de las
#columnas originales
# ahora podemos llamar los objetos por su nombre
attach(pob)
head(pob) # nos muestra primeros datos del archivo

# Sacar promedios por columna
# na.rm = FALSE dice al programa que ignore celdas vacias
colMeans(pob, na.rm = FALSE, dims = 1)

# Se leen datos de defunciones elaborados por SS e INEGI
def<-fread(file ="DEF.csv")

# Dentro de los corchetes elegimos de la fila 1-86
# despues de coma en blanco para seleccionar todas las columnas
def<-def[1:86,]
dim(def)
attach(def)
head(def)
colMeans(def, na.rm = FALSE, dims = 1)

# str nos dice la estructura
str(def)
str(pob)

# invocamos los datos y lo ponemos en data frame.
# invocamos los datos que queremos, para nuestro Estado
# Cambiar nombre de Mex a Durango
# Poblaciones para 2015 de DURANGO edades 0 a 85
# hacemos esto tanto para hombres como para mujeres
pob2015H=data.frame(P2015DURH)
pob2015M=data.frame(P2015DURM)
colnames(pob2015H)<- 2015
rownames(pob2015H)<- 0:85
colnames(pob2015M)<- 2015
rownames(pob2015M)<- 0:85

# Defunciones para 2015 de DURANGO edades 0 a 85
def2015H=data.frame(D2015DURH)
def2015M=data.frame(D2015DURM)
colnames(def2015H)<- 2015
rownames(def2015H)<- 0:85
colnames(def2015M)<- 2015
rownames(def2015M)<- 0:85

# Elegimos el año que queremos
# Elegimos el intervalo de edades que queremos
year <- 2015
ages <- 0:85

# A continuación se calcula la tasa de mortalidad especifica para hombres
# Esto se hace para cada año, cada edad (especifica por edad)
# Muertes Numerador Exposure Denominador, poblacion expuesta
deathsH <- def2015H[paste(ages),paste(year)]
exposureH <- pob2015H[paste(ages),paste(year)]

# Podemos visualizar nuestros nuevos datos para el año y las edades
deathsH
exposureH

# Tasa de mortalidad especifica en Durango para Hombres en un año
# Asignamos la variable smr.h (specific mortality rate hombres) con su debida fórmula
smr.h=(deathsH/exposureH)*1000
smr.h

#Graficamoos de forma logaritmica para hombres
plot(log(smr.h), col = "dark blue", type = "l", pch = 18, main = "Specific Mortality Rates for Men in Durango", xlab = "Age", ylab = "log(smr)")

# Muertes Numerador Exposure Denominador, poblacion expuesta
#asignamos valores a dos variables
deathsM <- def2015M[paste(ages),paste(year)]
exposureM <- pob2015M[paste(ages),paste(year)]

# Podemos visualizar nuestros nuevos datos para el año y las edades
deathsM
exposureM

# Tasa de mortalidad especifica en Durango para Mujeres en un año
smr.m=(deathsM/exposureM)*1000
smr.m

#Graficamoos de forma logaritmica para mujeres
plot(log(smr.m), col = "dark red",  type = "l", pch = 17, xlab = "Age", ylab = "log(smr)", main = "SMR for Women in Durango" )

#Graficamos las cosas juntas (correr dos lineas juntas)
plot(log(smr.m), col = "dark red",  type = "l", pch = 17, xlab = "Age", ylab = "log(smr)", main = "Durango Hombres y Mujeres" )
lines(log(smr.h), col = "dark blue", type = "l", pch = 18, xlab = "Age", ylab = "log(smr)")
legend("topleft", c("Hombres", "Mujeres"), fill=c("dark blue", "dark red"))

