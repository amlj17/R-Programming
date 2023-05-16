library(ggplot2)
unique(mpg$class)

ggplot(mpg) #es una figura

#añadimos estetica
ggplot(mpg)+
  aes(x = displ, y = hwy) #añade los ejes

ggplot(mpg) + 
  aes(x=displ, y = hwy) +
  geom_point() #añade los puntos

#igual que el anterior ponemos ejes y figura en 1 + dividir por tipo
ggplot(mpg, aes(x=displ, y =hwy, color = drv)) +
  geom_point() + geom_smooth()
#lo de color = drv es 'scaling'
#puede ser color, shape, size (puedes poner mas de 1)


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y= hwy), color = "blue")

#scaling a una variable continua (no discreta)
ggplot(data=mpg, aes(x=displ, y=hwy, color=cty)) +
  geom_point()

ggplot(data=mpg, aes(x=displ, y=hwy, size=cty)) +
  geom_point()

ggplot(data=mpg, aes(x=displ, y=hwy, shape=cty)) +
  geom_point()
#hace un gradiente, shape no se puede

#misma variable muchas esteticas
ggplot(data=mpg, aes(x=displ, y=hwy, color=trans, size = trans)) +
  geom_point()

#stroke nos da el grueso
ggplot(data=mpg, aes(x=displ, y=hwy)) +
  geom_point(shape = 3, stroke = 3)

#####
ggplot(mpg) +
  aes(x=displ, y=hwy, color = displ<5)+
  geom_point()


### facetas ----
### divide graficos por clase o tipo
ggplot(mpg) +
  aes(x=displ, y = hwy, color = drv)+
  geom_point() +
  facet_wrap(~class, nrow=1)

ggplot(mpg) +
  aes(x=displ, y=hwy) +
  geom_point()+
  facet_grid(drv~cyl) #eje y ~ eje x
#podemos poner (. ~ var) o (var ~ .)

#multiples geometrias
p<- ggplot(mpg) +
  aes(x=displ, y=hwy)
p + geom_point() + geom_smooth(se = TRUE,
                               method = 'loess',
                               span = 0.5)
# method = 'lm' da rectas

p + aes(color =drv)+
  geom_point()+
  geom_smooth(aes(linetype = drv)) #drv da tipo de linea

p + 
  geom_smooth(aes(group = drv)) #drv solo separa

#grafica 1
p +
  geom_point()+
  geom_smooth(se= FALSE)

#grafica 2
p +
  geom_point()+
  geom_smooth(mapping = aes(group = drv), se = FALSE)

##grafica 3
p +
  aes(color = drv) +
  geom_point() +
  geom_smooth(se = FALSE)

#grafica 4
p +
  geom_point(aes(color =drv)) +
  geom_smooth()

#grafica 5
p +
  geom_point(aes(color = drv))+
  geom_smooth(aes(linetype = drv))

#grafica 6
p +
  geom_point(mapping= aes(fill = drv), stroke = 3, shape = 21, color = 'white',
             size =3)
#si tienen stroke el color es del stroke