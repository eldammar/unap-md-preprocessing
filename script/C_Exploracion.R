#========================================================
# Marketing Directo                          
#========================================================

# Lectura de datos
#-------------------

DMark <- read.csv("./data/MarketingDirecto.csv")
head(DMark)                          
str(DMark)                           

#Ordenar niveles de variables categoricas ordinales
DMark$Edad = factor(DMark$Edad,levels = c("Joven","Media","Adulta"),ordered=TRUE)
DMark$Historial = factor(DMark$Historial,levels = c("Bajo","Medio","Alto"),ordered=TRUE)
#DMark$Edad <- ordered(DMark$Edad, levels =  c("Joven","Media","Adulta"))                   #No es necesario escribir: Order=TRUE viene por defecto 
str(DMark)                                     

#-----------------------------------------------------------------
# 1. Representacion de Datos Cualitativos                         
#-----------------------------------------------------------------

# Tabla de Frecuencia
# ----------------------
ni<-table(DMark$Edad)
fi<-prop.table(table(DMark$Edad))
pi<-prop.table(table(DMark$Edad))*100
edad.tabla<-t(rbind(ni,fi,pi))                   
edad.tabla


# Grofico de Barras
# -------------------
barplot(pi, main="Distribucion de las edades de los clientes", 
        xlab="Grupo Etario",
        ylab="Porcentaje de Clientes")
par(mfrow=c(2,1))                          
barplot(table(DMark$Edad), main="Distribucion de la Edad de los Clientes", 
        col=1,xlab="Edad",ylab="# de Clientes")               
barplot(prop.table(table(DMark$Edad))*100,
        main="Distribucion de la Edad de los Clientes", 
        col=3,xlab="Edad",ylab="% de Clientes")               
par(mfrow=c(1,1))


# Grofico de Sector Circular
#--------------------------------
pie(pi, main="Distribucion de la Edad de los Clientes") 

## Colocar porcentajes
lbls1 <- paste(names(table(DMark$Edad)), "\n",
               prop.table(table(DMark$Edad))*100,"%", sep="")          
pie(pi, labels = lbls1,
    main="Distribucion de la Edad de los Clientes")



#  Tablas de contingencia                                       
#-----------------------------
tabla1=table(DMark$Edad,DMark$Historial)
tabla1


#  c) Distribucion condicional                                     
#----------------------------------
tabla2=prop.table(tabla1,margin=1)                   
tabla2


# Barras agrupadas
#------------------
barplot(tabla2,col=2:4,beside = T,
        xlab="Historial de Compra",
        ylab="Proporcion de Clientes",
        main="Distribucion del historial de compra segun grupo etario")
legend("topright",legend=levels(DMark$Edad),col=2:4,
       pch=15,title="Grupo Etario")                      

## Invirtiendo filas x columnas
tabla=prop.table(table(DMark$Historial,DMark$Edad),margin=2)
tabla

barplot(tabla,col=2:4,beside = T,
        xlab="Grupo Etario",
        ylab="Proporcion de Clientes",
        main="Distribucion del historial de compra segun grupo etario")
legend("topright",legend=levels(DMark$Historial),col=2:4,
       pch=15,title="Historial de Compra")

# Barras Componentes
#--------------------
barplot(tabla,col=2:4,
        xlab="Grupo Etario",
        ylab="Proporcion de Clientes",
        main="Distribucion del historial de compra segun grupo etario")            
legend("topright",legend=levels(DMark$Historial),col=2:4,
       pch=15,title="Historial de Compra")



#Grafico de Mosaico
#-------------------
library(vcd)
mosaicplot(~ Edad+Historial, data = DMark, color = 2:4, 
           main="Distribucion del historial de compra segun grupo etario")          


#------------------------------------------------------------------
#  2. Representacion de Datos Cuantitativos Discretos             
#------------------------------------------------------------------

# Tabla de Frecuencias
#-----------------------
ni<-table(DMark$Hijos)
fi<-prop.table(table(DMark$Hijos))
pi<-prop.table(table(DMark$Hijos))*100
hijos.tabla<-t(rbind(ni,fi,pi))
hijos.tabla

#Grofico de Varas
#-----------------
plot(pi, type="h", lwd=2,
     xlab="Nomero de hijos",
     ylab="Porcentaje de clientes",
     main="Distribucion del nomero de hijos de los clientes")          
points(x =as.numeric(row.names(pi)),
       y =as.numeric(pi),
       pch=19,cex=1.5)            


#-----------------------------------------------------------------
#  3. Representacion de Datos Cuantitativos Continuos             
#-----------------------------------------------------------------

#Tabla de Frecuencias
#-----------------------
factorx <- factor(cut(DMark$Monto, breaks=nclass.Sturges(DMark$Monto),right=TRUE))    
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("Monto","ni")
xout <- transform(xout, 
                  fi=prop.table(ni),
                  pi=prop.table(ni)*100,
                  Ni = cumsum(ni),
                  Fi = cumsum(prop.table(ni)),
                  Pi = cumsum(prop.table(ni))*100
)
xout

library(agricolae)                                            
(table.freq(hist(DMark$Monto,breaks = "Sturges",plot=FALSE))) 

(table.freq(graph.freq(DMark$Monto,plot=FALSE)))              


# Histograma y pologono de frecuencia
#-------------------------------------
h1<-hist(DMark$Monto,breaks = "Sturges",
         xlab="Monto",
         ylab="Nomero de clientes")     
polygon.freq(h1,frequency=1,col="red")  


# Pologono de Frecuencias (solo)
#--------------------------------
h1<-hist(DMark$Monto,border=FALSE)
polygon.freq(h1,frequency=1,col="red")

# Histograma (Comparativo)
par(mfrow=c(1,3))
hist(DMark$Monto[DMark$Edad=="Joven"],ylim=c(0,170))
hist(DMark$Monto[DMark$Edad=="Media"],ylim=c(0,170))
hist(DMark$Monto[DMark$Edad=="Adulta"],ylim=c(0,170))
par(mfrow=c(1,1))

# Tallo y Hojas
#---------------
stem(DMark$Monto)
## (Comparativo)
stem(DMark$Monto[DMark$Edad=="Joven"])
stem(DMark$Monto[DMark$Edad=="Media"])
stem(DMark$Monto[DMark$Edad=="Adulta"])


#Ojiva
#-----
h<-graph.freq(DMark$Monto,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE)
print(points)

#Histograma y Densidad
#----------------------
hist(DMark$Monto,prob=TRUE)
lines(density(DMark$Monto))

#Grofico de Densidad
#--------------------
plot(density(DMark$Monto))


#Boxplots
#---------
boxplot(DMark$Monto)
boxplot(DMark$Monto ~ DMark$Edad)

#----------------------------------------------------------------
#  Anolisis descriptivo                                         
#----------------------------------------------------------------

# Resumen bosico
#------------------
summary(DMark$Monto)
summary(DMark)        

# Funcion para calcular CV o coeficiente de varianza
CV <- function(x){
  (sd(x)/mean(x))*100
}

# Funcion para calcular asimetria
A3 <- function(x){
  3*(mean(x)-median(x))/sd(x)
}

# Funcion para calcular el rango
rango <- function(x){
  diff(range(x))
}

# Funcion para calcular el rango intercuartolico
RIC <- function(x){
  quantile(x,probs = 0.75,type = 6)-quantile(x,probs = 0.25,type = 6)
}

me<-mean(DMark$Monto)
med<-median(DMark$Monto)
q1<-quantile(x = DMark$Monto,probs = 0.25,type = 6)     
q3<-quantile(x = DMark$Monto,probs = 0.75,type = 6)
r<-rango(DMark$Monto)
ric<-RIC(DMark$Monto)
s<-sd(DMark$Monto)
cv<-CV(DMark$Monto)
as3<-A3(DMark$Monto) 

resumen<-as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3))   
colnames(resumen)<-c("Valor")
resumen

# Otras funciones de resumen
Hmisc::describe(DMark$Monto)     
library(psych)
psych::describe(DMark$Monto)     

library(fBasics)
basicStats(DMark$Monto)

skewness(DMark$Monto)
kurtosis(DMark$Monto)



#-----------------------------------------------------------------
#  Anolisis descriptivo comparativo                             
#-----------------------------------------------------------------

me<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=mean)
med<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=median)
q1<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=quantile,probs = 0.25,type = 6)
q3<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=quantile,probs = 0.75,type = 6)
r<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=rango)
ric<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=RIC)   
s<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=sd)
cv<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=CV)
as3<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=A3)

resumen<-as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3))
resumen

psych::describeBy(x = DMark$Monto, group = DMark$Ecivil)    

#

boxplot(DMark$Monto ~ DMark$Ecivil,
        xlab="Estado Civil",ylab="Gasto",
        main="Comparacion del gasto por estado civil")


#-------------------
#   Asociacion                                                    
#-------------------

#Grafico y coeficiente de correlacion
#-------------------------------------
plot(DMark$Salario,DMark$Monto)
cor(DMark$Salario,DMark$Monto)    
cor(DMark$Salario,DMark$Monto, method = "pearson" )


# Matriz de Diagramas de dispersion
#------------------------------------
pairs(~Salario + Monto + Hijos + Catalogos,data=DMark)    
library(psych)
cor(DMark[,c(6,7,9,10)])              
corr.test(DMark[,c(6,7,9,10)])        
cor.plot(cor(DMark[,c(6,7,9,10)]))    

