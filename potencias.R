library(pwr)

#Ejercicio 1 
#ANOVA vs T de student
#Dos muestras
student = vector(mode = "numeric", length = 1000)
ANV = vector(mode = "numeric", length = 1000)

for ( i in 3:1002) {
  x=pwr.t.test(n=i, d=0.2, sig.level = 0.05, type = "two.sample", alternative = "two.sided") 
  y=pwr.anova.test(k = 2 , n = i , f =0.2 , sig.level =0.05 )
  student[i-2] = x$power
  ANV[i-2] = y$power
}

diferencias = ANV - student

plot(student, type = "l", main = "Potencias por Tamaño de Muestra", xlab = "tamaño (n)", ylab = "Potencia", col="green", ylim = c(0,1))
lines(ANV, type = "l",col="blueviolet")
lines(diferencias, type = "l", col="red")

plot(diferencias, type = "l", col="red", main = "Diferencia de Potencias", xlab = "tamaño (n)", ylab = "ANOVA - t Student")

#Anova Ejercicio 
ANV2=vector(mode = "numeric", length = 1000)

for (i in 2:1001){
  v=pwr.anova.test(k = i , n = 100 , f =0.2 , sig.level =0.05 )
  ANV[i-1] = v$power
} 
plot(ANV2, type = "l", col = "red")

#Ejercicio 2
#Idea proporciones
Proporciones = vector(mode = "numeric", length = 1000)
Proporciones2 = vector(mode = "numeric", length = 1000)
Proporciones3 = vector(mode = "numeric", length = 1000)
for(i in 2:1001){
  g = pwr.2p2n.test(h = 0.5, n1=i, n2=1003-i, sig.level = 0.05)
  f = pwr.2p2n.test(h = 0.5, n1=i, n2=i, sig.level = 0.05)
  j = pwr.2p2n.test(h = 0.5, n1 = 1003-i, n2 =1003 - i, sig.level = 0.05)
  Proporciones[i-1] = g$power #desbal
  Proporciones2[i-1] = f$power #bal +
  Proporciones3[i-1] = j$power #bal - 
}
diferencias2 = abs(Proporciones - Proporciones2) #des vs bal +
diferencias3 = abs(Proporciones - Proporciones3) #des vs bal -
diferencias4 = abs(Proporciones2 - Proporciones3) #bal + vs bal -

plot(Proporciones, type = "l", col = "blueviolet", main = "Potencias: Muestras Balanceadas vs. Desbalanceadas", xlab = "tamaño (n)", ylab = "potencia")
lines(Proporciones2, type = "l", col = "red")
lines(diferencias4, type = "l", col = "blue")
lines(Proporciones3, type = "l", col = "green")

max(diferencias2)
max(diferencias3)
max(diferencias4)
