#Llamando las librer??as para realizar los an??lisis de patrones puntuales
library("maptools")
library("rgdal")
library("spatstat")
library("sp")
library("readxl")
library("pool")
library("pryr")
library("prettymapr")
#Configurando el espacio de trabajo


setwd("C:/Users/jeimy.aristizabal/Downloads/Datos-20220621T185717Z-001/Datos")
#Creando el patr??n puntual
CorOri <- readOGR("CordilleraOriental_planas.shp")
pdf(file="Graf1.pdf")
plot(CorOri , main="Cordilera Oriental de Colombia")
rango2=read.csv("sismos_mag4_cat1.csv")

#PATRON CON MARCAS
patron1= ppp(rango2$x, rango2$y, marks=rango2$mag, window=as(CorOri, "owin"))
patron2=ppp(rango2$x, rango2$y,marks=rango2$prof, window=as(CorOri, "owin"))
patron3=ppp(rango2$x, rango2$y,marks=as.factor(rango2$catprof), window=as(CorOri, "owin"))
patron4=ppp(rango2$x, rango2$y,marks=as.factor(rango2$catmag), window=as(CorOri, "owin"))
x11()

#INTENSIDAD POR CUDRANTES REGULARES
cuadrante <- quadratcount(patron1, nx = 6, ny = 6)
plot(patron1, cols=c("gray"), box=TRUE, leg.side=c("right"))
plot(cuadrante, add = TRUE, cex = 0.8, col="red")
axis(side = 1, at = c(730500,890000,1080000,1240000 ), pos=616000, cex.axis=0.7 )
axis(side = 2, at = c(616300,808000,1008000,1208000,1408000, 1519700), pos=730000, cex.axis=0.7 )
plot(intensity(cuadrante, image=TRUE), main="Intensidad estimada por metro cuadrado")
axis(side = 1, at = c(730500,890000,1080000,1240000 ), pos=616000, cex.axis=0.8 )
axis(side = 2, at = c(616300,808000,1008000,1208000,1408000, 1519700), pos=730000, cex.axis=0.8 )
addnortharrow(pos = "topleft", padin = c(2.1, 0.15), scale = 0.3,lwd = 1, border = "black", cols = c("white", "black"),text.col = "black")

# INTENSIDAD POR CUADROS USANDO DEPTOS
setwd("C:/Users/jeimy.aristizabal/Downloads/Datos-20220621T185717Z-001/Datos/shapes")

arauca_depto <- readOGR("arauca.shp")
bogota_depto<- readOGR("Bogota.shp")
boyaca_depto <- readOGR("boyaca.shp")
caqueta_depto <- readOGR("caqueta.shp")
casanare_depto <- readOGR("casanare.shp")
cauca_depto <- readOGR("cauca.shp")
cesar_depto <- readOGR("cesar.shp")
cundinamarca_depto <- readOGR("cundinamarca.shp")
huila_depto<- readOGR("huila.shp")
meta_depto <- readOGR("meta.shp")
norteSantander_depto <- readOGR("norteSantander.shp")
santander_depto <- readOGR("santander.shp")
tolima <- readOGR("tolima.shp")

DEPTOS <- list(Arauca = arauca_depto, Bogota = bogota_depto, Boyaca = boyaca_depto, 
               Caqueta = caqueta_depto, Casanare = casanare_depto, Cauca = cauca_depto, 
               Cesar = cesar_depto, Cundinamarca = cundinamarca_depto, Huila = huila_depto, 
               Meta= meta_depto, Norte_de_Santander = norteSantander_depto,  
               Santader = santander_depto, Tolima = tolima)

CorOriental_deptos <- as.tess(DEPTOS)
Conteo_deptos <- quadratcount(patron1, tess = CorOriental_deptos, image=TRUE)
intensidad_deptos <- intensity(Conteo_deptos, image=TRUE)
TestDepto<-quadrat.test(Conteo_deptos)

plot(intensidad_deptos, main = "Conteo por departamentos")
axis(side = 1, at = c(730500,890000,1080000,1240000 ), pos=616000, cex.axis=0.8 )
axis(side = 2, at = c(616300,808000,1008000,1208000,1408000, 1519700), pos=730000, cex.axis=0.8 )
addnortharrow(pos = "topleft", padin = c(2.1, 0.15), scale = 0.3,lwd = 1, border = "black", cols = c("white", "black"),text.col = "black")
addscalebar(plotunit = NULL, plotepsg = NULL, widthhint = 0.15,
            unitcategory = "metric", htin = 0.1, padin = c(2.6, 0.17),
            style = "ticks",lwd = 0.4,linecol = "black", tick.cex = 0.3, labelpadin = 0.08, label.cex = 0.5,
            label.col = "black", pos = "bottomright")

#Ripley
funcionK1 = envelope(patron1,Kest)
plot(funcionK1, main="Function K")
write.table(funcionK1,"FuncionK_mag4Ori.txt")

#Vecino mas cercano
Gs1<-envelope(patron1,Gest)
plot(Gs1, main="Function G")
write.table(Gs1,"Gs_mag4Ori.txt")

#Correlacion Par
par1 = envelope(patron1,pcf)
plot(par1, main="Functions pair")
write.table(par1,"Par_mag4Ori.txt")

#Mark correlation function
funcionMc1 = envelope(patron1,markcorr)
funcionMc2 = envelope(patron2,markcorr)
funcionMc3 = envelope(patron3,markcorr)
funcionMc4 = envelope(patron4,markcorr)

plot(funcionMc1, main="Function Mark correlation - magnitude")
plot(funcionMc2, main="Function Mark correlation - depth")
plot(funcionMc3, main="Function Mark correlation - depth category")
plot(funcionMc4, main="Function Mark correlation - magnitude category")

write.table(funcionMc1,"FuncionMc1_mag.txt")
write.table(funcionMc2,"FuncionMc2_prof.txt")
write.table(funcionMc3,"FuncionMc3_catprof.txt")
write.table(funcionMc4,"FuncionMc4_catmag.txt")

#Mark-weighted K-function
MarkWK1 = envelope(patron1,Kmark)
MarkWK2 = envelope(patron2,Kmark)
MarkWK3 = envelope(patron3,Kmark)
MarkWK4 = envelope(patron4,Kmark)

plot(MarkWK1, main="Mark-weight K- magnitude")
plot(MarkWK2, main="Mark-weight K- depth")
plot(MarkWK3, main="Mark-weight K- depth category")
plot(MarkWK4, main="Mark-weight K- magnitude category")


write.table(MarkWK1,"FuncionMrWk_mag.txt")
write.table(MarkWK2,"FuncionMrWk_prof.txt")
write.table(MarkWK3,"FuncionMrWk_catprof.txt")
write.table(MarkWK4,"FuncionMrWk_catmag.txt")


#Mark variogram
varmark1=markvario(patron1)
varmark2=markvario(patron2)

plot(varmark1, main ='Mark variogram - magnitude')
plot(varmark1, main ='Mark variogram - depth')

write.table(varmark1,"Varmark1_mag.txt")
write.table(varmark1,"Varmark2_prof.txt")
