#Uso de If
x<-5
if(x==0){
  print("x es cero")
  x<-x+100
  print(x)
}else{
    print("x no es cero")
    x<-x*100
    print(x)
}

#Uso de for
contador <- seq(1,5,0.5)
for (j in contador){
  print(j)
  print("Siguiente renglon")
}

#Generar una secuencia
seq(1, 9, by = pi)

#Mas decimales
options(digits = x)

#uso de while
x<-10
while (x<=100) {
  print(x)
  x<-x+20
}