###############################################################################################
#----------------------------UNIVERSIDAD NACIONAL DE COLOMBIA---------------------------------#
#-------------------------------DEPARTAMENTO DE ESTAD??STICA-----------------------------------#
#------------------------------ESPECIALIZACI??N DE ESTAD??STICA---------------------------------#
#---------------------------------------------------------------------------------------------#
#------------------------------------PATRONES PUNTUALES---------------------------------------#
###############################################################################################

#================================================
#paquetes necesarios
#================================================
library (sp)
library(nlme)
library(rpart)
library(splancs)
library(spatstat)
library(mctest)
library(ape)
library(maptools)
library(rgdal)
library(ecodist)
library(RANN)

#================================================
#Lectura datos
#================================================

setwd("C:/Users/jeimy.aristizabal/Downloads/Datos-20220621T185717Z-001/Datos")
#Datos = Datos_Sismos_2009
Datos=read.table("Datos_Sismos_2009.txt",dec=",",sep="\t",header=T)
summary(Datos)

S <- readShapePoly("colombia.shp")
SP <- as(S, "SpatialPolygons")

sismos=ppp(Datos$X,Datos$Y,marks=Datos$Magnitud,window=owin(c(450433,1806815),c(22875.5,1870870.5)))
unitname(sismos)="meter"

#================================================
#Gr??ficas b??sicas
#================================================
x11()
plot(sismos, main="")
x11()
par(mfrow=c(1,2))
hist(sismos$x,xlab="Este",ylab="",main="")
hist(sismos$y,xlab="Norte",ylab="",main="")


#==========================================================================
###############Pruebas de Aleatoriedad basados en cuadrantes###############
#==========================================================================


#================================================
#Por medio de cuadrados
#================================================

#Prueba Chi-cuadrado
#Ho: la intensidad es homogenea
#Ha: la intensidad no es homogenea
sismos.split <- split(sismos)

#Cuadricula 4x4
quadratcount(sismos, nx = 4, ny = 4)
Q4 = quadratcount(sismos, nx = 4, ny = 4)
x11()
plot(sismos, cex = 0.5, pch = "+",main="")
plot(Q4, add = TRUE, cex = 0.8,col=4)
plot(S,add = TRUE)
quadrat.test(sismos, nx = 4, ny = 4)

#Cuadricula 5x5
quadratcount(sismos, nx = 5, ny = 5)
Q5 = quadratcount(sismos, nx = 5, ny = 5)
x11()
plot(sismos, cex = 0.5, pch = "+",main="")
plot(Q5, add = TRUE, cex = 0.8,col=4)
plot(S,add = TRUE)
quadrat.test(sismos, nx = 5, ny = 5)

#Cuadricula 6x6
quadratcount(sismos, nx = 6, ny = 6)
Q4 = quadratcount(sismos, nx = 6, ny = 6)
x11()
plot(sismos, cex = 0.5, pch = "+",main="")
plot(Q4, add = TRUE, cex = 0.8,col=4)
plot(S,add = TRUE)
quadrat.test(sismos, nx = 6, ny = 6)

# Lamda Ret??cula 4X4
plot(sismos, main="Eventos", pch=18)
lambda1<-quadratcount(sismos, nx=4,ny=4)
plot(lambda1, main=" Intensidad reticula 4*4", cex=1)
# Lamda Ret??cula 5X5
lambda2<-quadratcount(sismos, nx=5,ny=5)
plot(lambda2, main=" Intensidad reticula 5*5", cex=1)
#Lamda 6X6
lambda3<-quadratcount(sismos, nx=6,ny=6)
plot(lambda3, main="Intensidad reticula 6*6", cex=1)

#==========================================================================
###############Pruebas de Aleatoriedad basados en distancias###############
#==========================================================================

#=================================================================
#Gr??ficas de las funciones de distribuci??n emp??ricas G-hat y F-hat
#=================================================================
n <- length(sismos$x)
x11()
par(mfrow=c(1,2))

#================================================
#Para calcular la funci??n G
#================================================
sismos.ghat <- Gest(sismos)
plot(sismos.ghat,xlab="r",ylab="Ghat(r)")

Genv<-envelope(sismos,fun="Gest",nsim=999,nrank=5)
plot(Genv,xlab="r",ylab="Ghat(r)",cex.lab=1.6,cex.axis=1.5,main="G-Hat",cex.main=1.5)

#Test Hopkins-Skellam
#Ho: El patr??n espacial es completamente aleatorizado
#Ha: El patr??n espacial no es completamente aleatorizado
#Nivel de significancia (alpha=0.05)

hopskel.test(sismos, method = "MonteCarlo", nsim = 999)

#================================================
#Para calcular la funci??n F
#================================================
sismos.fhat <- Fest(sismos)    
plot(sismos.fhat,xlab="r",ylab="Fhat(r)")

Fenv<-envelope(sismos,fun="Fest",nsim=999,nrank=5)
plot(Fenv,xlab="r",ylab="Fhat(r)",cex.lab=1.6,cex.axis=1.5,main="F-Hat",cex.main=1.5)

#=================================================================
#Gr??ficas de las funciones K y L de Ripley 
#=================================================================

#================================================
#Para calcular la funci??n K
#================================================
sismos.khat <- Kest(sismos)
plot(sismos.khat$r,sismos.khat$iso,xlab="r",ylab="Ripley's K")
lines(sismos.khat$r,sismos.khat$theo,lty=8,lwd=2)
#================================================
#Para calcular la funci??n L
#================================================
sismos.lhat <- Lest(sismos)
plot(sismos.lhat$r,sismos.lhat$iso,xlab="r",ylab="Ripley's L")
lines(sismos.lhat$r,sismos.lhat$theo,lty=8,lwd=2)


#==========================================================================
########################Estimaci??n de la intensidad########################
#==========================================================================

#Ancho de banda ideal
x <- Datos$x
y <- Datos$y

coord <- coordinates(Datos)
coord
class(coord)

nndist(sismos)
k<-max(nndist(sismos))
d<- (1/6)*k

border <- readShapePoly("colombia")
bord <- border@polygons[[1]]@Polygons[[1]]@coords
str(border)

Mse2d <- mse2d(as.points(coord), bord, nsmse=1000, range=d)
x11()
#general
plot(Mse2d$h[50:1000], Mse2d$mse[50:1000], type="l")
#Apliado
plot(Mse2d$h[500:1000], Mse2d$mse[500:1000], type="l")

min(Mse2d$mse)
Mse2d$mse
Mse2d$h

#Ancho de banda ideal (m??nimo mse)
banda <- 18939.72936


#================================================
#Por medio de kernel Gaussiano
#================================================

dengaus=density(sismos,kernel="gaussian", bw = banda)
x11()
plot(dengaus)
plot(S,add = TRUE)
contour(dengaus, add = TRUE)
x11()
plot(dengaus)
persp(dengaus)
x11()
contour(dengaus)
plot(S,add = TRUE)

#Marcas seg??n la intensidad del sismo
#Menor a 3.5 en la escala de Richter
#y entre 3.5 y 5.5 en la escala de Richter
Datos1=read.table("Datos_Sismos_2009_1.txt",dec=",",sep="\t",header=T)
sismos1=ppp(Datos1$X,Datos1$Y,marks=Datos1$Marcas,window=owin(c(450433,1806815),c(22875.5,1870870.5)))
Datos1$cat <- as.factor(Datos1$Marcas)
mpp=ppp(Datos1$X,Datos1$Y,marks=Datos1$cat,window=owin(c(450433,1806815),c(22875.5,1870870.5)))
spp <- split(mpp)
dengaus1=density(spp[1:1],kernel="gaussian")
dengaus2=density(spp[2:2],kernel="gaussian")
x11()
plot(dengaus1)
plot(S,add = TRUE)
plot(dengaus2)
plot(S,add = TRUE)

