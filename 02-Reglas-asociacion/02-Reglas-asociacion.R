#=====================================================================================================
#-------------------------REGLAS DE ASOCIACION (EJEMPLO)----------------------------------------------
#=====================================================================================================

# Importacion de la Data
#------------------------
library(arules)
Lista_productos <- read.transactions(file = "02-Reglas-asociacion/data/groceries.csv", format = "basket", sep = ",")

Lista_productos
dim(Lista_productos)
summary(Lista_productos)


size(Lista_productos)
summary(size(Lista_productos))
quantile(size(Lista_productos), probs = seq(0,1,0.1)) 


labels(Lista_productos)
colnames(Lista_productos)
colnames(Lista_productos)[1:5]


inspect(Lista_productos)
inspect(Lista_productos[1:5])


itemFrequency(Lista_productos)
itemFrequency(Lista_productos, type="absolute")
itemFrequency(Lista_productos, type="relative")
itemFrequency(Lista_productos[,1:3])

summary(itemFrequency(Lista_productos))
sum(itemFrequency(Lista_productos))


itemFrequencyPlot(Lista_productos)
itemFrequencyPlot(Lista_productos, support=0.1)
itemFrequencyPlot(Lista_productos, topN=5)
itemFrequencyPlot(Lista_productos, topN=5, horiz=TRUE)

image(Lista_productos)
image(Lista_productos[1:5])
image(sample(Lista_productos, 5))

inspect(arules::subset(Lista_productos, subset = items %ain% "yogurt"))


#Algoritomo Apriori
#---------------------
apriori(Lista_productos)

Lista_productos_Reglas<-apriori(Lista_productos, parameter = list(support=0.02, confidence=0.05, minlen=2))

Lista_productos_Reglas
summary(Lista_productos_Reglas)

inspect(Lista_productos_Reglas)
inspect(Lista_productos_Reglas[1:3])
inspect(sort(Lista_productos_Reglas, by="support"))
inspect(sort(Lista_productos_Reglas, by="support")[1:5])
inspect(sort(Lista_productos_Reglas, by="confidence")[1:5])
inspect(sort(Lista_productos_Reglas, by="lift")[1:5])
inspect(sort(Lista_productos_Reglas, by = "confidence", decreasing = TRUE))
inspect(sort(subset(Lista_productos_Reglas, subset=lift > 0.80), by="lift"))

inspect(subset(Lista_productos_Reglas, subset = lhs %ain% "whole milk" & confidence > 0.2))
inspect(subset(Lista_productos_Reglas, subset = rhs %ain% "yogurt" & confidence > 0.05))

#Graficas
#---------
plot(Lista_productos_Reglas)
plot(Lista_productos_Reglas, method = "graph")
plot(Lista_productos_Reglas,method="matrix", measure="confidence") 

plot(Lista_productos_Reglas, shading = "lift", control = list(main="Paramentros de Reglas", col=rainbow(5)))
plot(Lista_productos_Reglas, measure = c("support", "lift"), shading = "confidence", control = list(main="Paramentros de Reglas", col=rainbow(5)))
plot(Lista_productos_Reglas,method="matrix3D",measure="confidence")
plot(Lista_productos_Reglas, method = "grouped")


#Conversion de reglas a Data
List_productos_Reglas_df<- as(Lista_productos_Reglas, "data.frame")
head(List_productos_Reglas_df)

#Exportar reglas
write(Lista_productos_Reglas, file = "output/reglas-exportar-r.csv", sep = ",", quote = TRUE, row.names = FALSE)

