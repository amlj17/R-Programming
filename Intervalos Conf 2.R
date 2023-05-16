#Examen 1
#INFERENCIA ESTADISTICA II
#Andres Moguel

#Definicion y generacion de datos
sigma1 = 2.2
sigma2 = 2.1
r_0 = 1
m = 100 # muestras datos 1
n = 256 # tamaño de cada muestra
datos1 = matrix(rnorm(m*n, 0, sigma1), n)
datos2 = matrix(rnorm(m*n, 0, sigma2), n)

icdv = var.test(x=datos1, y=datos2, ratio = r_0, alternative = "two.sided", conf.level = 0.99)
icdv$conf.int
icdv
