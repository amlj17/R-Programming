#grafico de barras
library(tidyverse)
library(ggplot2)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = stat(prop), group =1))

ggplot(data = diamonds) +
  stat_summary(mapping = aes(x=cut, y= depth),
               fun.min = min, fun.max = max, fun = median)

#tipo de corte y que claridad tienen
ggplot(data = diamonds) +
  aes(x = cut, fill = clarity) +
  geom_bar()

ggplot(data = diamonds) +
  aes(x = cut, fill = clarity) +
  geom_bar(position = "dodge")

#por porcentajes
ggplot(data = diamonds) +
  aes(x = cut, fill = clarity) +
  geom_bar(position = "fill")

ggplot(data = diamonds) +
  aes(x = price) +
  geom_histogram() #geom_poly / geom_density (suavizada)

ggplot(data = diamonds) +
  aes(x = price) +
  geom_density(aes(fill = cut), alpha = 0.4)

ggplot(diamonds)+
  aes(x =price) +
  geom_density(adjust = 1.5) #adjust es suavizamiento
#adjust bajo cuando queremos exaltar aspectos locales
#adjust alto cuando queremos un overall picture

ggplot(diamonds) +
  aes(x = price) +
  geom_histogram(binwidth = 100)

#freedman diaconis
# --- 2*IQR over cube root of sample size
fd <- function(x){
  n <- length(x) #elementos de un vector (sample size)
  2*IQR(x)/(n)^(1/3)
}

ggplot(diamonds) +
  aes(x = price) +
  geom_histogram(binwidth = fd)

#funcion que regresa otra funcion
power_fun <- function(n){
  function(x) x^n
}

