#Examen 1
#INFERENCIA ESTADISTICA II
#Andres Moguel
set.seed(5)
#Definicion y generacion de datos
sigma1 = 2.5
sigma2 = 2.3
#Definir Sigmas son iguales
r_0 = 1
m = 100 # muestras datos 
n = 256 # tamaño de cada muestra
datos1 = matrix(rnorm(m*n, 0, sigma1), n)
datos2 = matrix(rnorm(m*n, 0, sigma2), n)
dat=rbind(datos1, datos2)

icd2 = function(x, conf.level=0.99){
  d = (length(x)/2)-1
  cu = qf((1-conf.level)/2, d,d, lower.tail = FALSE)
  cl = qf((1-conf.level)/2, d,d, lower.tail = TRUE)
  v1= var(x[1:256])
  v2 = var(x[257:512])
  c((v1/v2)*cl, (v1/v2)*cu)
}

ic2= apply(dat,2,icd2)

icd = function(x){
  var.test(x[1:256], x[257:512], ratio=r_0, alternative = "two.sided", 
           conf.level = 0.99)$conf.int}
ic= apply(dat, 2, icd)
ic

#calculamos el cociente real para poder graficar dentro de IC's
cociente_real = sigma1^2/sigma2^2

#acumulamos dentro de cuantos intervalos cae el cociente real
#Tenemos H0 sigma1^2/sigma2^2 = 1 h1 ≠ 1
icrat=sum(ic[1, ] <= cociente_real & ic[2, ] >= cociente_real)

icrat2 =sum(ic2[1, ] <= cociente_real & ic2[2, ] >= cociente_real)

plot(range(ic2), c(0, m), xlab = "cociente real",ylab = "Numero de muestra", main = "Intervalos de Confianza 99% a Mano")
for (i in 1:m) {
  lines(ic2[, i], rep(i, 2), lwd = 2.5, col="blue")
}
abline(v = cociente_real, lwd = 1.5, col="red")

plot(range(ic), c(0, m), xlab = "cociente real",ylab = "Numero de muestra", main = "Intervalos de Confianza 99% con R")
for (i in 1:m) {
  lines(ic[, i], rep(i, 2), lwd = 2.5, col="blue")
}
abline(v = cociente_real, lwd = 1.5, col="red")

diferencias = ic - ic2
print(diferencias)

