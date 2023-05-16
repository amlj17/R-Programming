#manejo de directorios
dir_original<-getwd()
#ponemos la direccion de esta materia
setwd("~/Desktop/UIA/5to Semestre UIA/Metodos Numericos")
getwd()
#los archivos a leer deben estar en el directorio en el que esta set
readline("Enter para continuar")
#read es para leer
A<-read.table("OtraMatA.txt")
print(A)
#write es para guardar
write.table(A, file = "salida.txt", row.names = FALSE, col.names = FALSE)
