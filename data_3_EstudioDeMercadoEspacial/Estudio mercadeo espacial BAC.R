rm(list=ls())
library(openxlsx)
library(dplyr)
library(rgdal)
library(maptools)
library(GISTools)
library(spdep)
library(readr)
library(car)
library(readxl)
library(psych)
library(rgdal)
library(FactoClass)
require("GWmodel")
library("mapsRinteractive")
options(scipen = 999)
setwd("C:/Users/57320/Desktop/Unal/trabajo de grado/Regresión espacial/Bases")
# Lectura de Datos
BASE <- read_excel("BASE.xlsx")
# Lectura del Shape de Colombia por Departamentos
Colombia = readOGR(dsn = "Geodatabase Colombia", layer = "departamentos")
#Cruce de información con el shape cargado
Insumo = merge(Colombia, BASE, by.x="COD_DANE", by.y="Cod")
Insumo = subset(Insumo[c(1:31,33),])
# Conversión a Coordenadas UTM
Crs.geo = CRS("+proj=tmerc +lat_0=4.599047222222222 +lon_0=-74.08091666666667 +k=1 +x_0=1000000 +y_0=1000000 +ellps=intl +towgs84=307,304,-318,0,0,0,0 +units=m +no_defs")  
proj4string(Insumo) <- Crs.geo 
Insumo.utm = spTransform(Insumo, CRS("+init=epsg:3724 +units=km"))
#---
# MATRIZ DE VECINDADES (W)
#---
## Centroides de las Áreas
Centros = getSpPPolygonsLabptSlots(Insumo.utm)
Centroids <- SpatialPointsDataFrame(coords = Centros, data=Insumo.utm@data, 
                                    proj4string=CRS("+init=epsg:3724 +units=km"))
# Matriz de Distancias entre los Centriodes
Wdist = dist(Centros, up=T)
# Matriz W de vecindades
library(pgirmess)
library(HistogramTools)
library(strucchange)
library(spdep)
Insumo.nb = poly2nb(Insumo.utm, queen=T)
#n <- max(sapply(Insumo.nb, length))
#ll <- lapply(Insumo.nb, function(X) {
#  c(as.numeric(X), rep(0, times = n - length(X)))
#})
#out <- do.call(cbind, ll)
#Departamentos<-Insumo$Departamento
#MatW<-matrix(NA,32,32)
#for (i in 1:8) {
#  for (j in 1:32) {
#    if (out[i,j]!=0) {
#      MatW[out[i,j],j]<-1
#    } else{MatW[out[i,j],j]<-0}
#  }
#}
#for (i in 1:32) {
#  for (j in 1:32) {
#    if (is.na(MatW[i,j])) {
#      MatW[i,j]<-0
#    }
#  }
#}
#colnames(MatW)<-Departamentos
#rownames(MatW)<-Departamentos
#MatW1<-MatW[,1:16]
#MatW2<-MatW[,17:32]
# Martiz W (Estilos)
Insumo.lw = nb2listw(Insumo.nb)
Insumo.lwb = nb2listw(Insumo.nb, style="B")
Insumo.lwc = nb2listw(Insumo.nb, style="C")
Insumo.lwu = nb2listw(Insumo.nb, style="U")
Insumo.lww = nb2listw(Insumo.nb, style="W")
#  Mapa de Valores Observados
windows()
choropleth(Insumo, Insumo$CAP_BAC)
shad = auto.shading(Insumo$CAP_BAC, n=5, cols=(brewer.pal(5,"Reds")), cutter = quantileCuts)
choro.legend(1555874,535165.5, shad, fmt="%1.1f", title = "Valores Locales", cex=0.7, under = "Menos de", between = "a", over = "Mas de")
title("Valores Observados para las captaciones del banco agrario 
       en Colombia, cuarto trimestre 2020", cex.main=1)
map.scale(755874,335165.5, 250000, "km", 2, 50, sfcol='brown')
#----------------------------
#  PRUEBAS DE AUTOCORRELACION
#----------------------------

# Moran
moran.test(Insumo$CAP_BAC, Insumo.lw)
# Dispersograma de Moran
windows()
moran.plot(Insumo$CAP_BAC, Insumo.lw, labels=as.character(Insumo$Departamento), xlab="Captaciones BAC", ylab="Captaciones BAC rezagado", las=1, pch=16, cex=0.5)
legend("bottomright", legend=c("I de Moran: 0.1530", "Valor P:      0.02262"), cex=1,bg='lightgreen')
title("Dispersograma de Moran para las captaciones del banco agrario en 
los Departamentos de Colombia, cuarto trimestre 2020", cex.main=1)
# Local G
nearng = dnearneigh(coordinates(Insumo.utm), 0, 550)
Insumo.lw.g = nb2listw(nearng, style="B")

localG = localG(Insumo$CAP_BAC, Insumo.lw.g); localG


# Simulaci?n montecarlo
sim.G = matrix(0,1000,32)
for(i in 1:1000) sim.G[i,] = localG(sample(Insumo$CAP_BAC),Insumo.lw.g)
mc.pvalor.G = (colSums(sweep(sim.G,2,localG,">="))+1)/(nrow(sim.G)+1)
mc.pvalor.G


# Mapas
par(mfrow=c(1,2), mar=c(1,1,8,1)/2)
shadeg = auto.shading(localG, n=5, cols=(brewer.pal(5,"Purples")), cutter=quantileCuts)
windows()
choropleth(Insumo, localG, shading=shadeg)
choro.legend(1555874,535165.5, shadeg, fmt="%1.2f", title = "G", cex=0.7, under = "Menos de", between = "a", over = "Mas de")
title("G Getis Ord Local para las captaciones del banco agrario 
       en Colombia, cuarto trimestre 2020", cex.main=1)
map.scale(755874,335165.5, 250000, "km", 2, 50, sfcol='brown')


# Mapa de P-values
windows()
shadegp = shading(c(0.01,0.05,0.1), cols = (brewer.pal(4,"Spectral")))
choropleth(Insumo, mc.pvalor.G, shading=shadegp)
choro.legend(1555874,535165.5, shadegp, fmt="%1.2f", title = "P-valor de G", cex=0.7, under = "Menos de", between = "a", over = "Mas de")
title("P- Valor de G Getis Ord Local para las captaciones del banco agrario 
       en Colombia, cuarto trimestre 2020", cex.main=1)
map.scale(755874,335165.5, 250000, "km", 2, 50, sfcol='brown')
####Modelos SDEM, SDM, Manski, SARAR########
#reg.eq1=CAP_BAC ~ PIB + NBI + CAP_BOG + CAP_BC + CAP_OCC + CAP_CS + Población + IPM
reg.eq1=CAP_BAC ~ PIB + NBI + CAP_BOG+CAP_BC + CAP_OCC + CAP_CS+ Población
reg1=lm(reg.eq1,data=Insumo)                                     #OLS            y=XB+e,    
reg2=lmSLX(reg.eq1,data=Insumo, Insumo.lw)                       #SLX            y=XB+WxT+e
reg3=lagsarlm(reg.eq1,data= Insumo, Insumo.lw)                   #Lag Y          y=XB+WxT+u,   u=LWu+e
reg4=errorsarlm(reg.eq1,data=Insumo, Insumo.lw)                  #Spatial Error  y=pWy+XB+e   
reg5=errorsarlm(reg.eq1, data=Insumo, Insumo.lw, etype="emixed") #SDEM Spatial Durbin Error Model y=XB+WxT+u,   u=LWu+e
reg6=lagsarlm(reg.eq1, data=Insumo,Insumo.lw, type="mixed")      #SDM Spatial Durbin Model (add lag X to SAR) y=pWy+XB+WXT+e 
reg7=sacsarlm(reg.eq1,data=Insumo, Insumo.lw, type="sacmixed")   #Manski Model: y=pWy+XB+WXT+u,   u=LWu+e (no recomendado)
reg8=sacsarlm(reg.eq1,data=Insumo,Insumo.lw, type="sac")         #SARAR o Kelejian-Prucha, Cliff-Ord, o SAC If all T=0,y=pWy+XB+u, u=LWu+e
#Resumen de modelos
s=summary
s(reg1)#OLS
s(reg2)#SLX
s(reg3)#Lag Y
s(reg4)#Lag Error (SEM)
s(reg5)#Durbin Error (SDEM)
s(reg6)#Durbin (SDM)
s(reg7)#Manski
s(reg8)#SARAR lag Y and lag e (SAC)
#Calculo de variables signid¿ficativas
reg.eq2=CAP_BAC ~ PIB + CAP_BOG+CAP_BC + CAP_OCC + CAP_CS+ Población
reg4=errorsarlm(reg.eq2,data=Insumo, Insumo.lw)
s(reg4)#Lag Error (SEM)
reg.eq3=CAP_BAC ~ PIB + CAP_BOG + CAP_OCC + CAP_CS+ Población
reg4=errorsarlm(reg.eq3,data=Insumo, Insumo.lw)
s(reg4)#Lag Error (SEM)
reg.eq4=CAP_BAC ~ PIB + CAP_OCC + CAP_CS+ Población
reg4=errorsarlm(reg.eq4,data=Insumo, Insumo.lw)
s(reg4)#Lag Error (SEM)
reg.eq5=CAP_BAC ~ PIB + CAP_OCC + CAP_CS
reg4=errorsarlm(reg.eq5,data=Insumo, Insumo.lw)
s(reg4)#Lag Error (SEM)
###Mapa estimado
fit = reg4$fitted.values
windows()
shade.fit = shading(c(100,130,200,400), cols=(brewer.pal(5,"Reds")))
choropleth(Insumo, fit, shading=shade.fit)
choro.legend(1555874,535165.5, shade.fit, fmt="%1.2f", title = "Estimaciones", cex=0.7, under = "Menos de", between = "a", over = "Mas de")
title("Valores ajustados mediante el modelo SEM para las captaciones del banco agrario 
       en Colombia, cuarto trimestre 2020", cex.main=1)
map.scale(755874,335165.5, 250000, "km", 2, 50, sfcol='brown')
###R^2 Nagelkerke
summary.sarlm(reg4,Nagelkerke = TRUE)
###Test de moran residuales modelo SEM
moran.test(reg4$residuals, Insumo.lw)




#Municipal
ColombiaM = readOGR(dsn = "Geodatabase Colombia", layer = "municipios")