#para poder reproducir
set.seed(5)
# repeticiones del experimento (dardos lanzados)
n=10000
# hacemos las coordenadas x1 y x2 de una distribución uniforme de[-1, 1]
x1<-runif(n, min=-1, max=1)
x2<-runif(n, min=-1, max=1)
#Distancia de los puntos al centro del circulo (0,0)
z<-sqrt(x1^2+x2^2)
# el area del circulo (cae dentro) es todo z
# en este caso deben se menores al radio^2 para estar dentro del circulo
# in our case radius=1
piest<-4*sum((z<=1))/length(z)
print(piest)
#graficamos dentro o fuera del circulo por color.
InOut<-as.factor(ifelse(z<=1, "In", "Out"))
plot(x1,x2, col=InOut, main="Estimar π con Monte Carlo")