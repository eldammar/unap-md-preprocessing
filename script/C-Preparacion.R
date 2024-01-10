#-----------------------------------------------------------------------------------------------
#Preparacion de los Datos: Valores Faltantes, Outliers, Transformacion, Discretizacion
#-----------------------------------------------------------------------------------------------

#==========================================
#Valores Faltantes
#==========================================

mov<-read.table("data/movil_cl.csv", sep = ";", dec=".", header = T)
head(mov)

#Detectando los valores faltantes: Identificar
#----------------------------------------------------

colSums(is.na(mov))
which(colSums(is.na(mov))!=0)

colmiss<-c(2,3,5,6,7)
100*colSums(is.na(mov[,colmiss]))/dim(mov)[1]

library(VIM)
a<-aggr(mov)
a
summary(a)


b<-which(rowSums(is.na(mov))!=0)  


#Tratamiento: ¿Que hacer?
#---------------------------

#1. Eliminar
movcl<-na.omit(mov)
movcl

colSums(is.na(movcl))

#2. Imputacion
#Usando la media
library(DMwR2)
mov.imp.media<-centralImputation(mov)
mov.imp.media

mov[b,]
mov.imp.media[b,c(1,2,3,4,5,6,7)]

mean(mov[,5], na.rm = T)
mean(mov[,6], na.rm = T)
mean(mov[,7], na.rm = T)
table(mov[,2])
table(mov[,3])


#Usando la mediana
library(VIM)
mov.imp.mediana<-initialise(mov, method = "median")
mov.imp.mediana

mov[b,]
mov.imp.mediana[b,c(1,2,3,5,6,7)]

median(mov[,5], na.rm = T)
median(mov[,6], na.rm = T)
median(mov[,7], na.rm = T)
table(mov[,2])

table(mov[,3])


#Usando modelo de regresion
library(simputation)

mov.imp <- impute_lm(mov, Sexo + Estado_Civil + N_llamadas + Minutos_Consum + Antig_Cliente ~ 1)         
mov.imp <- impute_lm(mov, Sexo + Estado_Civil + N_llamadas + Minutos_Consum + Antig_Cliente ~ 1 | nse)   
mov.imp <- impute_lm(mov, N_llamadas ~ Plan)                                                             


#==========================================
#Outlier
#==========================================

mov1<-read.table("./data/movil_out.csv", sep = ";", dec=".", header = T)
head(mov1)

mov2<-mov1[,c(5,6,7)]
head(mov2)

#Detectando los Outliers
#---------------------------
#Box plot
boxplot(mov2)
valoresper<-boxplot(mov2$Minutos_Consum, col="green", main="Minutos Consumidos")$out
valoresper

identify(rep(1, length(mov2[,2])), mov2[,2],labels =valoresper)

#Manualmente
Q3=quantile(mov2$Minutos_Consum, 0.75)
Q1=quantile(mov2$Minutos_Consum, 0.25)
RIC=Q3-Q1
LS=Q3+1.5*RIC
LI=Q1-1.5*RIC

LI
LS

Flag_Atip=ifelse(mov2$Minutos_Consum>LI & mov2$Minutos_Consum<LS,0,1)
df<-data.frame(mov2,Flag_Atip)
subset(df, Flag_Atip==1)

#Flag_Atip=ifelse(mov2$Minutos_Consum<LI |  mov2$Minutos_Consum>LS,1,0)



# Distancia de Mahalanobis
cm <- colMeans(mov2)
S <- cov(mov2)
dm <- sqrt(apply(mov2, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))

# Distancia de Mahalanobis cuadrada
d <- dm^2
d <- mahalanobis(mov2, cm, S)
barplot(d, main="Mahalanobis")
which.max(d)


#==========================================
#Transformacion
#==========================================

mov<-read.table("./data/movil_cl.csv", sep = ";", dec=".", header = T)
head(mov)

movcl<-na.omit(mov)
movcl

head(movcl)
mov_t<-movcl[,c(5,6,7,10)]
head(mov_t)
dim(mov_t)


#Z-score
#--------
zmov_t<-scale(mov_t[,-4])
zmov_t
head(zmov_t)
cbind(zmov_t,mov_t[,4])

#comprobacion
media<-mean(mov_t$N_llamadas)
desv<-sd(mov_t$N_llamadas)
z11=(mov_t[1,1]-media)/desv
z11

mean(zmov_t)
sd(zmov_t)



#Min-Max
#--------
# Usando la libreria dprep
source(file="./script/dprep.R")

mm_mov_t<-mmnorm(mov_t,minval=0,maxval=1 )[,-4]
summary(mm_mov_t)


#Escalamiento decimal
#---------------------

# Usando dprep
source('./script/dprep.R')
ed_mov_t<-decscale(mov_t)[,-4]
ed_mov_t


#Sigmoidal
#----------
sig_mov_t<-signorm(mov_t) [,-4]
summary(sig_mov_t)


#Grafico de comparacion
par(mfrow=c(2,3))
boxplot(mov_t[,1:3],main="mov_t")
boxplot(zmov_t[,1:3],main="znorm mov_t")
boxplot(mm_mov_t[,1:3],main="min-max mov_t")
boxplot(ed_mov_t[,1:3],main="dec scale mov_t")
boxplot(sig_mov_t[,1:3],main="signorm mov_t")


#==========================================
#Discretizacion
#==========================================

library(arules)
head(movcl)

#Intervalo de igual amplitud
#-----------------------------

ncat<-nclass.Sturges(movcl[,5])
movcl$N_llamadas_cat<-discretize(movcl[,5],
                                 method = "interval", 
                                 categories = ncat)

table(movcl$N_llamadas_cat)

#Intervalo de igual frecuencia
#-------------------------------
movcl$N_llamadas_cat_f<-discretize(movcl[,5], 
                                   method = "frequency", 
                                   breaks = 8)
head(movcl)
table(movcl$N_llamadas_cat_f)

#Intervalos predefinidos
#--------------------------

cut(movcl[,5], breaks = ncat)
range(movcl$N_llamadas)

cat<-cut(movcl[,5], breaks = c(0, 30, 50, 80, 120, Inf), 
         right = F)
table(cat)


#Discretizacion por entropia
#----------------------------
library(discretization)
de_mov_t=mdlp(mov_t)$Disc.data
table(de_mov_t[,1])
table(de_mov_t[,2])
table(de_mov_t[,3])


#Discretizacion con chiMerge
#------------------------------
dchi_mov_t=chiM(mov_t,0.05)$Disc.data
table(dchi_mov_t[,1])
table(dchi_mov_t[,2])
table(dchi_mov_t[,3])
