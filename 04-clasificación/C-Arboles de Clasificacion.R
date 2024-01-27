#===========================================================================
#Arbol de Clasificacion
#===========================================================================
library(rpart)
library(rpart.plot)
library(caret)

#Datos
datos<-read.table("Datos_Arbol.csv", sep = " ", header = T, dec = "../../../../../../../Downloads")
head(datos)

#Data de entrenamiento y prueba
set.seed(45)
i<-createDataPartition(datos$Flag_Fuga, p=0.80, list = FALSE, times = 1)    
Training<-datos[i,]                
Test<-datos[-i, ]                  

#Construccion del Arbol
#--------------------------
arbol<-rpart(Flag_Fuga~., method = "class", data = Training)
print(arbol)


rpart.plot(arbol, extra = 4)

library(partykit)
plot(as.party(arbol), tp_args = list(id=F), main = "Modelo de Arbol de Clasificaci??n")  


#Evaluacion del modelo
#------------------------
pred<-predict(arbol, Test, type = "class")
prob<-predict(arbol, Test, type = "prob") 


#Matriz de Clasificacion
mc<-table(pred, Test$Flag_Fuga)
mc
ta=(mc[1,1]+mc[2,2])/sum(mc)
ta



#AUC
library(pROC)
analysis<-roc(response=Test$Flag_Fuga, predictor = prob[,2])
analysis



#---------------------------------
#Construccion y Poda del arbol
#---------------------------------
arbol<-rpart(Flag_Fuga~., method = "class", data = Training, cp=0.01)  #Entrenamos el arbol
print(arbol)

#Grafico del arbol metodo 1
rpart.plot(arbol, extra = 4)

#Grafico del arbol metodo 2
library(partykit)
plot(as.party(arbol), tp_args = list(id=F), main = "Modelo de Arbol de Clasificaci??n")  


#Encontrando el parametro de complejidad optimo
printcp(arbol)    #Aquel cp cuyo xerror sea el mas peque??o


#Obteniendo el Arbol podado, a partir del cp optimo
arbol_p<-prune(arbol,cp=0.01)
print(arbol_p)   #Arbol podado


#Graficando el arbol podado
library(partykit)
plot(as.party(arbol_p), tp_args = list(id=F), main = "Modelo de Arbol de Clasificaci??n")  



#Evaluacion del arbol podado
#----------------------------
#Prediccion y probabilidad del arbol podado
pred_ap<-predict(arbol_p, Test, type = "class")
prob_ap<-predict(arbol_p, Test, type = "prob")


#Matriz de Clasificacion del arbol podado
mc<-table(pred_ap, Test$Flag_Fuga)
mc
ta<-(mc[1,1]+mc[2,2])/sum(mc)
ta


#AUC del arbol podado
library(pROC)
analysis<-roc(response=Test$Flag_Fuga, predictor = prob_ap[,2])
analysis
