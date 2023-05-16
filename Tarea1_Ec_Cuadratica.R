#Tarea 1
## Programa que resuelve ecuaciones cuadráticas
#recibimos datos del usuario y convertimos a numeric
#Andres Moguel

s<-1

while(s!=0){
  
  a<-as.numeric(readline("Ingrese el valor de a: "))
  b<-as.numeric(readline("Ingrese el valor de b: "))
  c<-as.numeric(readline("Ingrese el valor de c: "))
  
  if(a==0){
    print("Error no es una ecuacion cuadratica")
  }else{
    #definimos discriminante
    disc<-b^2-4*a*c

   #resolvemos por casos
    if(disc>0){
      r1 <- (-b+sqrt(b^2-4*a*c))/(2*a)
      r2 <- (-b-sqrt(b^2-4*a*c))/(2*a)
      print("Resultados")
      print(r1)
      print(r2)
    }

    if(disc==0){
    r=-b/(2*a)
    print("Resultados")
    print(r)
  }

    if(disc<0){
      print("las racies son complejas")
      #PARTE COMPLEJA
      R3 = ((b^2 - 4*a*c)*(-1))^(1/2)
      #PARTE REAL
      R4 = (-b)/(2*a)
      #PARTE COMPLEJA ENTRE 2a
      R5= (R3)/(2*a)
      
      cat(c(R4,"+","i",R5))
      cat(c(R4,"-","i",R5))
    }
  }
  s<-as.numeric(readline("Desea salir? (0 si): "))
}