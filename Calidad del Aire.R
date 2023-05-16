#Ejercicio 5 tarea 1 RL
setwd("~/Desktop/UIA/7mo Semestre UIA/Regresión")
temps_uso = read.csv("Temps.csv",row.names = 1)
temps_uso
attach(temps_uso)
# a) Encuentre la correlación entre el uso del vapor y la temperatura
#Formula de correlación:
#Cov(X,Y)/(sqrt(Var(x))*sqrt(Var(Y)))
Covar = cov(Temp, Usos)
Var_temp = var(Temp)
Var_usos = var(Usos)
correl_ro = Covar/(sqrt(Var_temp)*sqrt(Var_usos))

#b) pruebe la hipótesis ro = 0
# H0: ro = 0
# vs.
# H1: ro <> 0
Prueba_correl_b = cor.test(Temp, Usos, method ="pearson", alternative = "two.sided")
#Encontrar t
alfaa = 0.05
n = as.numeric(length(Usos))
t0 = (correl_ro*sqrt(n-2))/(sqrt(1-correl_ro^2))
t_real = qt(alfaa/2, (n-2), lower.tail = FALSE)

#Vemos que el p valor es menor a un alfa de 0.05 y por lo tanto se rechaza
# la hipotesis nula y se concluye que la correlacion de pearson es distinta de 0

#c) pruebe la hipótesis ro = 0.5
# H0: ro = 0.5
# vs.
# H1: ro <> 0.5


#d) Encuentre el IC del 99% CI para ro
Prueba_correl_d = cor.test(Temp, Usos, method ="pearson", alternative = "two.sided", conf.level = 0.99)
print(Prueba_correl_d)
#Observamos que el IC del 99% CI para ro es:
# 99 percent confidence interval:
# 0.9996244 ≤ ro ≤ 0.9999879

#Ejercicio 6.
Estadisticas_NFL= read.csv("Ejercicio6.csv")
head(Estadisticas_NFL)
attach(Estadisticas_NFL)
#a) ajuste un modelo de regresión lineal simple que relacione "y" con "x8"
fit2<-lm(y ~ x8, data = Estadisticas_NFL)
summary(fit2)
#Nos queda
# Y = 21.788251 -0.007025x
# B1_hat = -0.007025
# B0_hat = 21.788251
fitted(fit2)
residuals(fit2)
plot(Estadisticas_NFL$x8, Estadisticas_NFL$y,
     xlab = "Yardas corriendo del oponente",
     ylab = "Juegos ganados")
abline(fit2, col = "blue")

#b) construya la tabla de análisis de varianza y pruebe la significancia de la regresion
anova(fit2)
#El valor de F de la prueba es: 31.103
f0 = 31.103
#Calculemos el f real
alfa = 0.05
dof = length(Estadisticas_NFL$x8)-2
valor_f = qf(alfa,1,dof, lower.tail = FALSE)
#Tenemos: 
#HO: B1 = 0
#vs
#H1: B1 <> 0
# ya que f0 > valor de f se rechaza H0
#La variable regresora es significativa con un valor de F de 31.103 y un valor P menor que 7.38 X 10^-6

#c IC de la pendiente 95%
# intervalo es B1_hat +- valor t * SE(B1)
b1_hat = as.numeric(fit2$coefficients[2])
alfa1 = 0.05
valor_t = qt(alfa1/2, dof, lower.tail = FALSE)
#SE(b1) = sqrt ( MSres / Sxx)
#MSres = SSres/n-2
#SSres = sum residuals ^2
SSres = sum((fit2$residuals)^2)
MSres = SSres/dof
#Sxx = sum (xi - x_bar^2)
x_bar = mean(Estadisticas_NFL$x8)
Sxx = sum((Estadisticas_NFL$x8 - x_bar)^2)
#Ahora si SE(B1)
SE_B1 = sqrt(MSres/Sxx)
#calculamos los intervalos
int_izqB1 = b1_hat - valor_t*SE_B1
int_derB1 = b1_hat + valor_t*SE_B1
#Concluimos que:
# -0.009614 ≤ B1 ≤ -0.004436

#d) Que porcentaje de la variabilidad total en y se explica en este modelo
# con summary del fit vemos
summary(fit2)
#tomamos el multiple r squared
# R2 = 0.5447
# Aunque la variable regresora es significativa, por el inciso b, el valor de
# el coeficiente de determinación es R2  = 0.5447. Es decir que el modelo explica
# alrededor del 54.47% de la variabilidad en y

# e) Encuentre un IC del 95% en el numero medio de juegos ganados si las 
# yardas terrestres de los oponentes se limitan a 2000
# Respuesta media
# µ y|x0 con x0 = 2000
#el intervalo es: 
# mu_hat +- t valor * sqrt( MSres(1/n + (X0 - x-bar)^2/sxx))
#encontremos mu_hat
# mu hat = B0_hat + b1_hat(X0)
X0 = 2000
b0_hat = as.numeric(fit2$coefficients[1])
mu_hat = b0_hat + b1_hat*X0
#Ya tenemos del inciso c:
# valor de t, MSres, sxx y x_barra
n = as.numeric(length(Estadisticas_NFL$x8))
#construyendo el intervalo:
int_izq_RM = mu_hat - valor_t*sqrt(MSres*((1/n)+ ((X0- x_bar)^2/(Sxx))))
int_der_RM = mu_hat + valor_t*sqrt(MSres*((1/n)+ ((X0- x_bar)^2/(Sxx))))
#concluimos que:
# 6.7658 ≤ E[Y|2000] ≤ 8.7103

#f) Encontrar estimacion puntual para x8 = 1800 yardas y un IC del 90%
# para una estimacion puntual tomamos el modelo y evaluamos en x = 1800
#Ya conocemos los valores de B0_hat y B1_hat
x_new = 1800
pred_punt_yd = b0_hat + b1_hat*x_new
#Se espera ganar 9.14 juegos, lo redoneamos hacia abajo con 9 victorias
#Para un IC del 90%
alfa3 = 0.1
#el IC es:
#y0_hat +- t valor * sqrt( MSres(1 + 1/n + (X0 - x-bar)^2/sxx))
#Encontremos y0_hat = b0hat `+ b1hat*X0
y0_hat = b0_hat + b1_hat*x_new
#Calculemos el nuevo valor de t
valor_t_90 = qt(alfa3/2, dof, lower.tail = FALSE)
#Tenemos ya todo para el IC
int_izq_NO = y0_hat - valor_t_90*sqrt(MSres*(1+(1/n)+ ((x_new- x_bar)^2/(Sxx))))
int_der_NO = y0_hat + valor_t_90*sqrt(MSres*(1+(1/n)+ ((x_new- x_bar)^2/(Sxx))))
#Concluimos que: 
# 4.9364 ≤ y0 ≤ 13.3497