#Pearson
samples = 10 #numero de datos por muestra (X y Y)
r = 0.75 #correlacion de pearson

library('MASS')
#generamos datos multi variados normales
#media 0 (mu)
#desviacion estandard 1 y pearson rho de r 
data = mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
X = data[, 1]  # standard normal (mu=0, sd=1)
Y = data[, 2]  # standard normal (mu=0, sd=1)

# vemos que funcione
cor(X, Y) 

#Spearman
repeat{
  x=runif(10)
  y=runif(10)
  spearman = cor.test(x, y, method = "spearman", alternative = "two.sided")$estimate
  if(spearman==0.5)
    break
}

#Kendall
repeat{
  x=runif(10)
  y=runif(10)
  kendall1 = cor.test(x, y, method = "kendall", alternative = "two.sided")$estimate
  if(spearman==0.25)
    break
}