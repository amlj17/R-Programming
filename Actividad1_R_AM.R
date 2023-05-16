setwd("~/Desktop/UIA/7mo Semestre UIA/Regresión")
 #Conjunti de datos de mujeres, altura y peso
#Predecir peso a través de la altura

women$weight
fit <- lm(weight ~ height, data=women)
summary(fit)
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in pounds)")
abline(fit)

#Prueba de significancia estadistica:
# H0: B1 = 0 
#vs. 
# H1 B1 ≠ 0
#alfa = 0.05 
alfa1 = 0.05
# dof = n - 2 
dof_altura = length(women$height) - 2
#encontramos el valor de t real
valor_t_r = qt(alfa1/2, dof_altura, lower.tail = FALSE)
#calculemos el valor de t de prueba
#De sumarry de fit 2 vemos que:
#B1(hat) = 3.45
b1_hat_altura = 3.45
# SSres = suma de residuales ak cuadrado
SSres_altura = sum((fit$residuals)^2)
#MSres = SSres / n-2
MSres_altura = SSres_altura/(length(women$height)-2)
#x bar
x_bar_altura = mean(women$height)
#Sxx = suma (x - xbar)^2
Sxx_altura = sum((women$height - x_bar_altura)^2)
#SE(B1 hat) = raiz (MSres/Sxx)
SEB1_altura = sqrt(MSres_altura/Sxx_altura)
#t valor calculado
t0_altura = b1_hat_altura/SEB1_altura
abs(t0_altura) > valor_t_r
#se rechaza H0 y concluimos que hay una relacion lineal



cohete = read.csv("Rocket.csv",row.names = 1)
cohete
attach(cohete)
fit2<-lm(ShearStrength_yi_psi ~ Age_of_Propellant_xi_weeks, data = cohete)
summary(fit2)
fitted(fit2)
residuals(fit2)
plot(cohete$Age_of_Propellant_xi_weeks, cohete$ShearStrength_yi_psi,
     xlab = "Edad en Semanas del lote",
     ylab = "Resistencia al Corte psi")
abline(fit2)

#Prueba de significancia estadistica:
# H0: B1 = 0 
#vs. 
# H1 B1 ≠ 0
#alfa = 0.05 
alfa = 0.05
# dof = n - 2 
dof = length(cohete$ShearStrength_yi_psi) - 2
#encontramos el valor de t real
valor_t = qt(alfa/2, dof, lower.tail = FALSE)
#calculemos el valor de t de prueba
#De sumarry de fit 2 vemos que:
#B1(hat) = -37.154
b1_hat = -37.15
# SSres = suma de residuales ak cuadrado
SSres = sum((fit2$residuals)^2)
#MSres = SSres / n-2
MSres = SSres/(length(cohete$Age_of_Propellant_xi_weeks)-2)
#x bar
x_bar_rock = mean(cohete$Age_of_Propellant_xi_weeks)
#Sxx = suma (x - xbar)^2
Sxx = sum((cohete$Age_of_Propellant_xi_weeks - x_bar_rock)^2)
#SE(B1 hat) = raiz (MSres/Sxx)
SEB1 = sqrt(MSres/Sxx)
#t valor calculado
t0 = b1_hat/SEB1
abs(t0) > valor_t
#se rechaza H0 y concluimos que hay una relacion lineal