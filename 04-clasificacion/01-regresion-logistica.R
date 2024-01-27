#====================================================================
# # Ejemplo: Creditos de  Banco Aleman                           
#====================================================================

#Lectura de Datos
#-----------------
library(Fahrmeir)
data(credit)
help(credit)

str(credit)
head(credit)
table(credit$Y)

#Particion de la Data
#--------------------
library(caret)
set.seed(123)
train_index<-createDataPartition(credit$Y, p=0.75, list = FALSE, times = 1)    
Training<-credit[train_index,]                
Test<-credit[-train_index, ]

# Modelo de regresion Logistica Binaria
#------------------------------------------
mod <- glm(Y ~ ., family=binomial,data=Training)
summary(mod)

# Seleccion de Variables
#-----------------------
#Modelo de Regresion Logistica con las variables significativas
require(MASS)
mod1<-stepAIC(mod) 
mod1                  #Modelo final con variables seleccionadas
summary(mod1)         #Resumen del modelo final

# Calidad de Ajuste (Prueba de Hosmer y Lemeshow)
#-------------------------------------------------
library(ResourceSelection)
hl <- hoslem.test(mod1$y, fitted(mod1), g=10)
hl

#Pesudo R- Cuadrado
#-------------------
library(pscl)
pR2(mod1)[["McFadden"]]

library(DescTools)
PseudoR2(mod1, c("McFadden"))


#----------------------------------------------
#Evaluacion de la Prediccion del Modelo
#----------------------------------------------

#Estimacion de la probabilidad
#--------------------------------
TestProb<-predict(mod1, newdata=Test, type="response")

#Prediccion del modelo
#-----------------------
TestPred<-  ifelse(TestProb > 0.5, 1, 0)
TestPred <- factor(TestPred, labels = levels(Test$Y))

#Matriz de Confusion
#----------------------
t<-table(TestPred, Test$Y)
t

#Tasa de acierto, tasa de error
#--------------------------------
ta=(t[1,1]+t[2,2])/sum(t)
ta
te=1-ta
te

#AUC
#----
library(pROC)
auc2<-roc(response=Test$Y, predictor = TestProb)
auc2

#Curva ROC
#----------
plot(1-auc2$specificities, auc2$sensitivities, type = "l", xlab = "1-Especificidad", ylab = "Sensibilidad",
     main = "Curva ROC", col="red")
abline(a=0, b=1)

#Otra forma
#-----------
#confusionMatrix(TestPred, Test$Y, positive = "mal")
confusionMatrix(TestPred, Test$Y)

#Curvas ROC
#---------------
library(pROC)
# Area debajo de la curva ROC
analysis <- roc(response=Test$Y, predictor=TestProb)
analysis

# Hallar punto de corte Optimo
#------------------------------
# Usando el criterio del indice J de Youden
# J = Sensitivity + Specificity - 1
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities-1)
head(e)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]
opt_t

# Otra forma de obtener el punto de corte optimo
coords(analysis , "b", ret="t", best.method="youden") 

#Prediccion considerando el punto de corte optimo
TestPred2 <- factor(as.numeric(TestProb >= opt_t ), labels = levels(Test$Y))

#confusionMatrix(TestPred2, Test$Y, positive = "mal")
confusionMatrix(TestPred2, Test$Y)


#Otra forma de obtener AUC, ROC
#--------------------------------
InformationValue::plotROC(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb)
InformationValue::AUROC(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb)
InformationValue::optimalCutoff(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb, optimiseFor = "Both")
InformationValue::optimalCutoff(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb, optimiseFor = "Ones")
InformationValue::optimalCutoff(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb, optimiseFor = "Zeros")

# Estadistica KS (Kolmogorov-Smirnov)
#---------------------------------------
InformationValue::ks_plot(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb)
InformationValue::ks_stat(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb)
InformationValue::ks_stat(actuals = as.numeric(Test$Y)-1, predictedScores = TestProb, returnKSTable = TRUE)

# Coeficiente Gini
#--------------------
2*analysis$auc-1
MLmetrics::Gini(y_pred = TestProb,y_true =as.numeric(Test$Y)-1 )

#Base de datos con las probabilidades y categorias predichas
#-------------------------------------------------------------
Test$Probabilidad<- TestProb  
Test$Prediccion<- TestPred
head(Test)
