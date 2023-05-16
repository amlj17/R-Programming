#### Intervalo de confianza para la media
#n˙mero de variables aleatorias m
#n˙mero de datos por variable aleatoria

m = 100 # muestras
n = 256 # tamaño de cada muestra
media = 20000
sigma = 8000
#matriz de normales
datos = matrix(rnorm(m*n, media, sigma), n)
dim(datos)

#intervalos de confianza para una t de student
icdt = function(x) t.test(x, conf.level = 0.90)$conf.int
ic= apply(datos, 2, icdt)
ic
icmedia=sum(ic[1, ] <= media & ic[2, ] >= media)
#desmenuzar para ver quÈ se esta haciendo en la linea anterior 
icmedia

plot(range(ic), c(0, m), xlab = "media poblacional",ylab = "N˙mero de muestra")
for (i in 1:m) {
  lines(ic[, i], rep(i, 2), lwd = 2.5, col="blue")
}
abline(v = media, lwd = 2.5, col="red")

#1.- Investigar cu·l es el IC
#2.- øCÛmo podrÌa verificar que lo es? De manera empÌrica
#3.- øCÛmo podrÌa modicarlo, podria llegar a un IC del 100% o al 0%?

#### Intervalo de confianza para la varianza
#n˙mero de variables aleatorias m
#n˙mero de datos por variable aleatoria

m = 100 # muestras
n = 256 # tamaÒo de cada muestra
media = 20000
sigma = 8000
s2=(sigma)^2
datos = matrix(rnorm(m*n, media, sigma), n)
dim(datos)
#chisq.test(x)$conf.int

icdc = function(data,conf.level=0.95) { 
  d=length(data)-1
  cl=qchisq((1 - conf.level)/2,d)
  cu=qchisq((1 - conf.level)/2, d, lower.tail = FALSE)
  v = var(data)
  c(d*v/cu , d*v/cl)
}
ic= apply(datos, 2, icdc)
ic
ics2=sum(ic[1, ] <= s2 & ic[2, ] >= s2)
#desmenuzar para ver quÈ se est· haciendo en la linea anterior 
ics2

plot(range(ic), c(0, m), xlab = "varianza poblacional",ylab = "N˙mero de muestra")
for (i in 1:m) {
  lines(ic[, i], rep(i, 2), lwd = 2.5, col="blue")
}
abline(v = s2, lwd = 2.5, col="red")