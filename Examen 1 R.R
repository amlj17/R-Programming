setwd("~/Desktop/UIA/7mo Semestre UIA/Regresión")

#Pregunta 3 
vino = read.csv("vinos.csv")
attach(vino)
#Primero haremos un modelo de regresión simple que relacione la calidad del vino con la
#cantidad de azufre.
fitv = lm(calidad ~ SO2, data = vino)
summary(fitv)
#Visualizar datos
plot(vino$SO2,vino$calidad,
     xlab="Contenido de SO2",
     ylab="Calidad General")
abline(fitv)

#Probemos la significancia de la regresión:
#Tenemos: 
#HO: B1 = 0
#vs
#H1: B1 <> 0
#t0 = B1_hat/se(b1_hat) = sqrt(MSRes/Sxx)
b1_hat = -0.012762
#SE(b1) = sqrt ( MSres / Sxx)
#MSres = SSres/n-2
#SSres = sum residuals ^2
dof = length(SO2)-2
SSres = sum((fitv$residuals)^2)
MSres = SSres/dof
#Sxx = sum (xi - x_bar^2)
x_bar = mean(vino$SO2)
Sxx = sum((vino$SO2 - x_bar)^2)
SE_B1 = sqrt(MSres/Sxx)
#Entonces calculamos t0:
t_0 = b1_hat/SE_B1
#Calculamos el verdadero valor de t con alfa = 0.05
alfa = 0.05
valor_t = qt(alfa/2, dof, lower.tail = FALSE)

#Encontramos la correlación
#Cov(X,Y)/(sqrt(Var(x))*sqrt(Var(Y)))
Covarianza = cov(calidad, SO2)
Var_calidad = var(calidad)
Var_azufre = var(SO2)
correlacion = Covarianza/(sqrt(Var_calidad)*sqrt(Var_azufre))

#Pregunta 4
anuncios = read.csv("anuncios.csv")
attach(anuncios)
#inciso A
fita = lm(impresiones ~ gastos, data = vino)
summary(fita)
#Visualizar datos
plot(anuncios$gastos, anuncios$impresiones,
     xlab="Gastos en Millones",
     ylab="Impresiones en millones")
abline(fita)

#inciso b:
#Probemos la significancia de la regresión:
#Tenemos: 
#HO: B1 = 0
#vs
#H1: B1 <> 0
#t0 = B1_hat/se(b1_hat) = sqrt(MSRes/Sxx)
b1_hat1 = 0.1665
#SE(b1) = sqrt ( MSres / Sxx)
#MSres = SSres/n-2
#SSres = sum residuals ^2
dof1 = length(gastos)-2
SSres1 = sum((fita$residuals)^2)
MSres1 = SSres1/dof1
#Sxx = sum (xi - x_bar^2)
x_bar1 = mean(anuncios$gastos)
Sxx1 = sum((anuncios$gastos - x_bar1)^2)
SE_B1_1 = sqrt(MSres1/Sxx1)
#Entonces calculamos t0:
t_0_a = b1_hat1/SE_B1_1
#Calculamos el verdadero valor de t con alfa = 0.05
alfa1 = 0.05
valor_t_a = qt(alfa1/2, dof1, lower.tail = FALSE)

#inciso c:
# intervalo es B1_hat +- valor t real * SE(B1)
#Del inciso b) ya tenemos b1_hat, el valor de t real y el SE(B1)
#podemos ya construir el intervalo:
int_izqB1 = b1_hat1 - valor_t_a*SE_B1_1
int_derB1 = b1_hat1 + valor_t_a*SE_B1_1

#inciso d:
#definimos el valor nuevo x0: como la media de los gastos
x_nuevo = mean(anuncios$gastos)
#y0_hat +- t valor * sqrt( MSres(1 + 1/n + (X0 - x-bar)^2/sxx))
#para y0 gorro tenemos: los valores de b1 hat y b0 hat
#ya definimos en b) b1 hat b0 hat es:
b0_hat = 62.5480
#entonces
y_0_gorro = b0_hat + b1_hat1*(x_nuevo)
#definimos n:
n = as.numeric(length(anuncios$gastos))
# EL INTERVALO ES y0_hat +- t valor(alfa/2) * sqrt( MSres(1 + 1/n + (X0 - x-bar)^2/sxx))
#DE B) ya tenemos el valor de t como t_0_a y ya tenemos MSres asi como x barra y Sxx
#Construyendo el IC tenemos
int_izq_valN = y_0_gorro - t_0_a*sqrt(MSres1*(1+(1/n)+ ((x_nuevo- x_bar1)^2/(Sxx1))))
int_der_valN = y_0_gorro + t_0_a*sqrt(MSres1*(1+(1/n)+ ((x_nuevo- x_bar1)^2/(Sxx1))))

