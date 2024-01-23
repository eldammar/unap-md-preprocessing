#Importanto la librería
library(arules) #instalar desde consola: install.packages("arules",dependencies=TRUE)

#Leer el archivo por analizar
Datos_a_priori<-read.csv("02-Reglas-asociacion/data/datos-compras.csv", sep=",", dec=".", header = TRUE)

#Las transacciones deben ser numéricas y los productos factores
str(Datos_a_priori)

#Damos el formato de transaccion a Datos_a_priori
library(plyr) #instalar desde consola: install.packages("plyr",dependencies=TRUE)

#Por cada transaccion se crea una fila conteniendo todos los productos separados con una coma
Lista_productos<-ddply(Datos_a_priori,c("Transaccion"),function(df1)paste(df1$Producto,collapse = ","))

#Quitamos la columna Transaccion
Lista_productos$Transaccion<-NULL

#Guardamos los productos como CSV
#Es muy importante incluir row.names=FALSE para que no agregue una columna con el numero de
#quote=FALSE dejara un producto por celda
write.csv(Lista_productos, "02-Reglas-asociacion/output/lista-productos-r.csv", quote = FALSE, row.names = FALSE)

#Leemos el archivo con las transacciones que se escribieron
transacciones<-read.transactions("02-Reglas-asociacion/output/lista-productos-r.csv", format = "basket", sep = ",", header=TRUE)

#Desplegamos las transacciones en la consola
inspect(transacciones)

#Graficamos los productos que se han vendido mas
itemFrequencyPlot(transacciones, topN=10, type='absolute')

#Obtenemos las reglas de asociación por medio del algoritmo apriori
reglas<-apriori(transacciones, parameter = list(supp=0.1,conf=0.7, minlen=2))

#Ordenamos las reglas en base a su confianza
reglas<-sort(reglas, by="confidence",decreasing = TRUE)

#Viendo la estructura del objeto reglas
str(reglas)

#Desplegamos las reglas
inspect(reglas)

#Revisamos si hay reglas duplicadas
duplicated(reglas)

#Verificamos si hya reglas redundantes
redundantes<-is.redundant(reglas)
redundantes

#Ver cuales son las reglas redundantes
which(redundantes)

#Quitar las reglas redundantes
reglas_podadas <-reglas[!redundantes]

#Desplegamos las reglas sin redundancia
inspect(reglas_podadas)

#Creamos un grafo con las reglas
library(arulesViz) #instalar desde la consola: install.packages("arulesiz",dependencies=TRUE)
plot(reglas_podadas, method="graph", engine="interactive",shading="confidence")
