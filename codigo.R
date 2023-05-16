#Así se cargan los datos
setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II")
diet = read.csv("Diet.csv",row.names = 1)
diet

head(diet)

#Así se ve la estructura de los datos
str(diet)

#Así se preservan los nombres originales de las variables
#para que posteriormente puedan ser invocados
attach(diet)

#Es importante identificar la variable dependiente
#En este caso es la diferencia en peso antes y despúes de la dieta
#Asimismo las vías, deben ser declarados como factores con sus
#respectivos niveles
weight.loss = pre.weight - weight6weeks 
diet.type   = factor(Diet)
gender      = factor(gender)

#Ahora en términos descriptivos se pueden hacer box-plot's
windows()
boxplot(weight.loss~diet.type,ylim=c(-2.5,10),
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="blue")

#Si se hiciera la prueba de una vía, pérdida de peso en función de la dieta
anova1 = oneway.test(weight.loss~diet.type,data=diet)
anova1

# O bien, en función del sexo
anova1 = oneway.test(weight.loss~gender,data=diet)
anova1

#Ahora de dos vías (sin interacción)
anova2 = aov(weight.loss~diet.type+gender,data=diet)
summary(anova2)

#Ahora de dos vías (con interacción)
anova3 = aov(weight.loss~diet.type*gender,data=diet)
summary(anova3)

#Post hoc tests
TukeyHSD(anova3,which = "diet.type")

#Only interpret post hoc tests for the significant factors from the ANOVA. If
#the interaction is NOT significant, interpret the post hoc tests for significant main effects
#but if it is significant, only interpret the interactions post hoc tests.


#Ejemplo2
#Relativo a un estudio donde se evalua el efecto de la vitamina C sobre
#el crecimiento de los dientes de puercos en Guinea. Se hace con 60 animales, 
#cada animal recibió uno de los 3 niveles de dosis de vitamina C (0.5, 1, y 2 mg al dia) 
#ya fuere por jugo de naranja o por acido ascorbico. Luego se mide la longitud de los dientes.

# Almacena datos precargados de R en la variable data
data <- ToothGrowth
data

dim(data)
str(data)
head(data)
attach(data)

windows()
#quartz() # para MAC
boxplot(len ~ supp * dose, data=data, col = c("blue", "red"), ylab="Tooth Length")

# Gráfica de interacción de dos vías 
windows()
#quartz() # para MAC
interaction.plot(x.factor=dose,trace.factor=supp, 
                 response = len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("blue", "red"))

supp = as.factor(supp)
supp
dose1= "NA"
dose1[dose == 0.5] = "DB"
dose1[dose == 1] = "DM"
dose1[dose == 2] = "DA"
dose1 = as.factor(dose1)
dose1

#Se añade la nueva variable a la base de datos original
data=cbind(data,dose1)
head(data)

# Estadísticos descriptivos de los grupos
require("dplyr")
group_by(data, supp, dose1) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )

# One-way ANOVA
res.aov11 <- aov(len ~ supp, data = data)
summary(res.aov11)

# One-way ANOVA
res.aov12 <- aov(len ~ dose1, data = data)
summary(res.aov12)

# Two-way ANOVA sin efecto de interacción (esto asume independencia entre los factores)
res.aov2 <- aov(len ~ supp + dose1, data = data)
summary(res.aov2)

# Two-way ANOVA con efecto de interacción (esto asume dependencia entre los factores)
res.aov3 <- aov(len ~ supp + dose1 + supp:dose1, data = data)
summary(res.aov3)
# que es equivalente a 
res.aov3 <- aov(len ~ supp*dose1, data = data)
summary(res.aov3)

# No es necesario realizar el Post hoc sobre “supp” porque tiene dos niveles y eso 
# ya se probó con la ANOVA. Aunque se puede calcular nuevamente

# Post hoc
TukeyHSD(res.aov3)

#Verificación de los supuestos
#Igualdad de varianzas
require("car")
leveneTest(len ~ supp*dose1, data = data)

#Normalidad
residuos <- residuals(object = res.aov3)
#Histograma
windows()
#quartz() # para MAC
hist(residuos, breaks=sqrt(60),col="green")
# Se aplica el test Shapiro-Wilk, que aunque existen más de 16 pruebas útiles para 
# ello en R, esta es la más potente (menor error tipo II). 
shapiro.test(residuos)

#Independencia
#Se asume por el diseño del experimento. Para ello NO se debe ejecutar
#ninguna prueba estadística, ya sea de independencia o de correlaciones.

