#=========================================================================
#Analisis Cluster de Particionamiento: K-MEANS
#=========================================================================

# Simulacion 
x <- cbind(rnorm(100,1000,100), c(rnorm(50),rnorm(50,10,1)))
plot(x,pch=16)

#k-means datos originales
res <- kmeans(x,2)
plot(x,col=c("green","red")[res$cluster],pch=16)

# Estandarizacion
xs <- scale(x)
plot(xs,pch=16)

#k-means datos estandarizados
res <- kmeans(xs,2)
plot(x,col=c("green","red")[res$cluster],pch=16)


library(cluster)

#Datos
#------
movil<-read.table("03-clustering/data/data_cluster.csv", sep = " ", header = T)
head(movil)
str(movil)

movil1<-movil[,4:11]
head(movil1)

zmovil1<-scale(movil1)                       


#---------------------
#Conglomerado k-mean
#---------------------
res<-kmeans(scale(movil1),centers= 2) 
res

res$tot.withinss


#
res<-kmeans(scale(movil1),centers= 4,nstart = 50)  
res
res$tot.withinss


#------------------------------------#
# Determinar numero de conglomerados #
#------------------------------------#

# Suma de cuadrados dentro de clusters
#-------------------------------------
wss<-numeric()                
for(h in 2:10){
  b<-kmeans(scale(movil1),centers=h, nstart = 50)    #Metodo de particionamiento: K-Medias 
  wss[h-1]<-b$tot.withinss
}                             
plot(2:10,wss,type="b", main = "Suma de Cuadrados dentro de Cluster")       


# Silueta
#---------
diss.movil <- daisy(scale(movil1))
par(mfrow=c(1,3))
for(h in 2:4){
  res <- kmeans(scale(movil1),h, nstart = 50)
  plot(silhouette(res$cluster, diss.movil))  
}                                               
par(mfrow=c(1,1))


# Criterio de Calinski-Harabasz
#-------------------------------
library(fpc)
ch<-numeric()                  
for(h in 2:10){
  res<-kmeans(scale(movil1),h,nstart = 100)              
  ch[h-1]<-calinhara(scale(movil1),res$cluster)          
}                                                           
plot(2:10,ch,type="b",xlab="k",                             
     ylab="Criterio de Calinski-Harabasz")                  


#------------------------------
#Perfilamiento de los cluster
#------------------------------

head(movil)
movil_cl<-cbind(movil, clust=res$cluster)
head(movil_cl)

install.packages("sqldf")
library(sqldf)
sqldf("select clust, avg(Ingreso_mensual_cliente),avg(N_llamada_recib),avg(N_mens_tex_env),avg(Tot_Min_consum),
      avg(N_recargas),avg(N_llamadas_realiz) , avg(Antig_cliente_meses), avg(N_reclamos)
      from movil_cl   
      group by clust") 


#==================================================================================
#Analisis Cluster Jerarquico: Aglomerativo (AGNES) y Divisivo (DIANA)
#==================================================================================

library(cluster)

#Datos
#-------
movil<-read.table("03-clustering/data/data_cluster.csv", sep = " ", header = T)
head(movil)
str(movil)


movil1<-movil[,4:11]
head(movil1)

#-------------
# AGNES
#-------------
a<-agnes(scale(movil1), metric = "euclidean", method = "ward")
a

plot(a, which=2)          #Dendrograma

a<-cutree(a, k=2)   #k: Numero de conglomerados 
a


#Silhouette
#--------------
diss.movil1 <- daisy(scale(movil1))
res <- agnes(scale(movil1),method="ward")
par(mfrow=c(1,3))
for(h in 2:4){
  conglomerados <- cutree(res,k=h)
  plot(silhouette(conglomerados,diss.movil1))
}


#Base con el cluster de pertenencia
cbind(movil, Clust=a)




#-------------
# DIANA
#-------------

d<-diana(scale(movil1), metric="euclidean")
d

plot(d, which=3)    #Dendrograma

d<-cutree(d,k=2)    #k: Numero de conglomerados
d


#
#Silhouette
#-------------
#diss.movil1=daisy(scale(movil1))
#res=diana(scale(movil))
#par(mfrow=c(1,3))
#for(h in 2:4){
#  conglomerados=cutree(res,h)
#  plot(silhouette(conglomerados,diss.movil1))
#}

#Base con el cluster de pertenencia
#cbind(movil, Clust=d)


# Escalar solo las columnas numéricas de movil1
numeric_cols_movil1 <- movil1[sapply(movil1, is.numeric)]
scaled_movil1 <- scale(numeric_cols_movil1)

# Combina las columnas escaladas con las no numéricas
movil1_scaled <- cbind(movil1[, !sapply(movil1, is.numeric)], scaled_movil1)

# Haz lo mismo para movil
numeric_cols_movil <- movil[sapply(movil, is.numeric)]
scaled_movil <- scale(numeric_cols_movil)
movil_scaled <- cbind(movil[, !sapply(movil, is.numeric)], scaled_movil)

# Ahora puedes usar los datos escalados en el resto de tu código
diss.movil1 <- daisy(movil1_scaled)
res <- diana(movil_scaled)
par(mfrow=c(1,3))
for(h in 2:4){
  conglomerados <- cutree(res, h)
  plot(silhouette(conglomerados, diss.movil1))
}

# Base con el cluster de pertenencia
cbind(movil, Clust = conglomerados)

