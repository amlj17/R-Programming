#Andres Moguel
#IE II

#Establecemos el directorio de trabajo
setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II")

#leemos los datos
germany = read.csv("pais.csv", header = TRUE)
germany

#visualizamos los datos y podemos leerlos por columnas
head(germany)
str(germany)
attach(germany)

#Establecemos factores
#Epoca 3 niveles
Epoca = as.factor(Epoca)
Epoca
#Genero 3 niveles
Gender = as.factor(Gender)
Gender

# Estadísticos descriptivos de los grupos
require("dplyr")
group_by(germany, Epoca, Gender) %>%
  summarise(
    count = n(),
    mean = mean(Life, na.rm = TRUE),
    sd = sd(Life, na.rm = TRUE)
  )

# One-way ANOVA Exp de vida en funcion del sexo
res.aov11 <- aov(Life ~ Gender, data = germany)
summary(res.aov11)
#el p valor es menor a alfa entonces hay diferencias significativas por sexo
#no se necesita post hoc dado que solamente hay 2 niveles

# One-way ANOVA Exp de vida en funcion a epoca historica
res.aov12 <- aov(Life ~ Epoca, data = germany)
summary(res.aov12)
#El p-valor es menor a alfa entonces hay diferencias significativas por epoca
#hay 3 niveles tenemos que saber de acuerdo a cual es.
#hacer post hoc

#Ahora de dos vías (sin interacción) entre sexo y expecativa de vida
anova2 = aov(Life~Gender+Epoca,data=germany)
summary(anova2)
#Tanto para sexo como la época hay diferencias significativas
#revisar pruebas post hoc

#Ahora de dos vías (con interacción)
anova3 = aov(Life~Gender*Epoca,data=germany)
summary(anova3)
#Para un alfa de 0.05 
#hay significancia tanto en genero como en época y en la interaccion
#revisar prueba post hoc

#recordemos la prueba de hipotesis
#H0: miu(M, Soviet) = ... = miu(F, Euro)
#vs.
#H1: al menos un par es distinto.

#Veamos la prueba post hoc
# Post hoc
TukeyHSD(anova3)
#Hay diferencias significativas entre todas las epocas 
#hay diferencias significativas entre todas las epocas y por sexo
#en todos los casos se rechaza HO
