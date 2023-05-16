setwd("~/Desktop/UIA/6to Semestre UIA/Inferencia Estadistica II/Trabajo en R/R IE II")

# Instalar paquetes y librerías requeridos
InsPack = function (pack) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    print (paste("Instalando ",pack))
    install.packages(pack)}
} 
#Instala e invoca el paquete
InsPack("data.table")
library(data.table)
library(dplyr)
library(ggplot2)


# asignar CSV a una variable
datos<-fread(file ="gst.csv")

# attach que existan objetos con nombres originales. Objetos con nombres de las
#columnas originales
# ahora podemos llamar los objetos por su nombre
attach(datos)
head(datos) # nos muestra primeros datos del archivo
#filtramos
#gastos2021TODOS=data.frame(clave,gasto)
#gastos2021TODOS=subset(gastos2021TODOS, clave!="B00")

gastos2021TOT=data.frame(clave,gasto)
gastos2021TOT=subset(gastos2021TOT, clave=="B00")

hist((gastos2021TOT$gasto), col = "red", main = "Distribucion Gastos del Hogar", xlab = "gastos totales")
describe(gastos2021TOT$gasto)

selectos<-datos %>% 
  select(clave,gasto) %>% 
  filter(clave%in%c("B09","B10","B07","B06","B11")) %>% 
  group_by(clave) %>% 
  summarise(mean_gasto = mean(gasto),
            median_gasto = median(gasto),
            std_gasto = sd(gasto),
            iqr_gasto = IQR(gasto),
            max_gasto = max(gasto),
            min_gasto = min(gasto))

selectos<-datos %>% 
  select(clave,gasto) %>% 
  filter(clave%in%c("B09","B10","B07","B06","B11"))

selectos2<-tapply(selectos$gasto, selectos$clave, sum)

barplot(log(selectos2), main="Gastos por Productos", xlab = "Producto", ylab = " log Gasto Total", col="green", space = 0.5)

total<-103614955

boxplot(log(selectos$gasto)~selectos$clave)
