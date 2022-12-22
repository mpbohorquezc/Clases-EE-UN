library(RandomFields)
rm(list=ls())
#############################################################
## ##
## Unconditional simulation ##
## ##
#############################################################
## first let us look at the list of implemented models
RFgetModelNames(type="positive definite", domain="single variable",iso="isotropic")
## our choice is the exponential model;
## the model includes nugget effect and the mean:
model <- RMexp(var=5, scale=10) + # with variance 4 and scale 10
RMnugget(var=1) + # nugget
RMtrend(mean=0.5) # and mean
## define the locations:
from <- 0
to <- 20
x.seq <- seq(from, to, length=200)
y.seq <- seq(from, to, length=200)
simu <- RFsimulate(model, x=x.seq, y=y.seq)
plot(simu)

#############################################################
## ##
## Conditional simulation ##
## ##1.  Modelo exponencial  modelo <- RMexp(var,scale) 
##Fijar varianza por ej en uno y variar scale entre 0.2 y 3

## ##2.  Modelo Matern modelo<-RMmater(nu,var,scale)
##Fijar varianza ej en uno y nu y variar scale entre 0.2 y 3
##Recorrer varios nu, varía de 0 a infinito pero por ahí cada 0.3 entre 0.2 y 3

## ##3.  Modelo Wave modelo<-RMwave(var,scale)
##Fijar varianza ej en uno y variar scale entre 0.1 y 3

##
## 4. RMgencauchy(alpha, beta, var, scale, Aniso, proj)
##alpha mayor que cero y menor o igual a 2 y beta mayor que 0

## 5. RMgengneiting(kappa, mu, var, scale)
## kappa=0,1,2,3 para escoger tipo modelo y mu mayor o igual a d/2
#############################################################
# Simulación inicial sobre la que después se va a condicionar, como si fueran las observaciones
# 100 random locations:
n <- 100
x <- runif(n=n, min=0, max=1)
y <- runif(n=n, min=0, max=1)
#modelo <- RMexp(1,0.2)   
#modelo<-RMmatern(0.6,1,0.8)
modelo=RMwave(1, 1)
dta <- RFsimulate(modelo, x=x, y=y, grid=FALSE)
plot(dta)
# let simulate a field conditional on the above data dta
L <- if (interactive()) 100 else 5
x.seq.cond <- y.seq.cond <- seq(0, 1, length=10)
VectorMedias=c(rep(20,35),rep(23,35),rep(25,30))
cond <- RFsimulate(modelo, x=x.seq.cond, y=y.seq.cond, data=dta)
datos_centrados=data.frame(x=expand.grid(x.seq.cond,y.seq.cond)$Var1,y=expand.grid(x.seq.cond,y.seq.cond)$Var2,datos=cond[[1]])

datos_centrados <- RFspatialPointsDataFrame(coords = datos_centrados[,c("x", "y")],data = datos_centrados[,"E"],RFparams=list(vdim=1, n=1))
dta <- datos_centrados[,"data"]

cond2 <- RFsimulate(modelo, x=x.seq.cond, y=y.seq.cond, data=dta)
datos_centrados_mas_media=cbind(datos_centrados,Mu=VectorMedias)
datos=data.frame(datos_centrados[,1:2],Z=VectorMedias+datos_centrados$E)
###Ubicaciones medias
plot(datos_centrados_mas_media[,1:2],type="n")
text(datos_centrados_mas_media$x,datos_centrados_mas_media$y,datos_centrados_mas_media$Mu)
###Ubicaciones datos finales no centrados
plot(datos_centrados_mas_media[,1:2],type="n")
text(datos_centrados_mas_media$x,datos_centrados_mas_media$y,datos_centrados_mas_media$Mu)
##Construcción matriz Sigma
Sigma=RFcovmatrix(modelo,expand.grid(x.seq.cond,y.seq.cond))

#####################################################################################################3
###  N O T A
##tres funciones para el mismo modelo efecto hueco
#####################################################################################################3

modeloWave=RMwave(1, 4)
modeloSinCard=RMcardinalsine(1, 4)
library(geoR)
x11()
curve(cov.spatial(x,cov.model="wave",cov.pars=c(1,4)),0,100)
x11()
plot(modeloWave,xlim=c(0,100),type="l")
x11()
plot(modeloSinCard,xlim=c(0,100),type="l")

#############################################################
## ##
## Multivariate Unconditional simulation ##
## ##using linear model of coregionalization
#############################################################
#En general, para dos variables M1=c(a,b) y M2=c(c,d) usando dos modelos de covarianza
#La silla total de la variable 1 es a*a+c*c. La silla total de la variable 2 es b*b+d*d. La contribución a la silla de la cruzada por parte del modelo 1 es a*b. La contribución a la silla de la cruzada por parte del modelo 1 es c*d. Entonces las dos matrices de coregionalización si se usan dos modelos es
#B1=matrix(c(a*a,a*b,a*b,b*b),nrow=2, byrow=T)
#B2=matrix(c(c*c,c*d,c*d,d*d),nrow=2, byrow=T)
#Así el modelo lineal de coregionalización es B1*C1(h)+B2*C2(h)

#simulando de un esférico mas un exponencial, 
#1. variar scale en ambos, del esférico hasta 1 y del exponencial hasta 3
#2. variar var en ambos

x <- seq(0, 1,len=10)
y <- seq(0, 1,len=10)
model1 <- RMmatrix(M = c(0.9,0.8), RMspheric(scale = 0.2)) + RMmatrix(M = c(0.6, 0.2), RMexp(scale = 0.2))
plot(model1,xlim=c(0,3))
simu1 <- RFsimulate(RPdirect(model1), x, y)
#plot(simu1)
Sigma=RFcovmatrix(model1,expand.grid(x,y)) #constante para todas las simulaciones

#reconstruir datos
VectoresMedias=data.frame(mu1=c(rep(20,35),rep(23,35),rep(25,30)),mu2=c(rep(1.5,35),rep(0.9,35),rep(1.3,30)))
datos_centrados=data.frame(x,y,E1=simu1$variable1,E2=simu1$variable2)

#simulando de un exponencial mas un wave, modelos usados en el ejemplo
#1. variar scale en ambos
#2. variar var en ambos
model2 <- RMmatrix(M = c(0.9, 0.5), RMexp(scale = 0.7)) + RMmatrix(M = c(0.2, 0.1), RMwave(scale = 0.2))
plot(model2,xlim=c(0,2))
x <- y <- seq(0, 1, len=10)
z1 <- RFsimulate(RPdirect(model2), x,y)
Sigma=RFcovmatrix(model2,expand.grid(x,y)) #constante para todas las simulaciones
VectoresMedias=data.frame(mu1=c(rep(20,35),rep(23,35),rep(25,30)),mu2=c(rep(1.5,35),rep(0.9,35),rep(1.3,30)))
datos_centrados=data.frame(x,y,E1=simu1$variable1,E2=simu1$variable2)

##No funciona aún la simulación condicional bivariada 19 de agosto de 2021

