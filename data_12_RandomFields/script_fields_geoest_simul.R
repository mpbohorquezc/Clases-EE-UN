library(fields)
library(geoR)
library(Matrix)
rm(list=ls())

x11()
plot(0:1.5, 0:1.5, type="n")
data(lennon)
add.image(0.5,0.5,lennon, col=grey((0:256)/256),image.width=1,image.heigth=1)
#add.image(0.5,0.5,lennon, col=tim.colors(256),image.width=1,image.heigth=1)
# reference lines
xline(0.5, col=3)
yline(0.5,col=4)

x11()
datos=cbind(x=rnorm(50),y=rnorm(50),z=rnorm(50))
datosIm<- as.image(datos[,3], x=datos[,1:2])
image.plot(datosIm)

#datos Chicago ozone test data
#ChicagoO3
#x tiene ID, este, norte
#y son los datos de ozono

#Thin plate spline regression
fit_TPS<- Tps(ChicagoO3$x, ChicagoO3$y)
x11()
par(mfrow=c(1,2))
surface.Krig(fit_TPS,type="p",extrap=F)
surface.Krig(fit_TPS,type="C",extrap=F)
surface.Krig(fit_TPS,type="I",extrap=F)

# fitting a surface to ozone measurements.
fit_Krig<- Krig(ChicagoO3$x, ChicagoO3$y, Covariance="Matern", aRange=100,smoothness=2,sigma=var(ChicagoO3$y))
data(ozone2)
surface.Krig(fit_Krig,type="C",extrap=F)
x<- ozone2$lon.lat
y<- ozone2$y[16,]
# Omit the NAs
good<- !is.na( y)
x<- x[good,]
y<- y[good]

#simulation with circulant embedding. Simula un GRF en una grilla regular con silla=1. El algoritmo circulant embedding usa FFT. 
#Usar con alcances no tan grandes o agrandando el spatial domain
grid<- list(x= seq(0,5,len=100), y= seq(0,5,len=150))
obj<- circulantEmbeddingSetup(grid, Covariance="Exponential", aRange=.5)
set.seed(223)
look<- circulantEmbedding(obj)
# Now simulate another ...
look2<- circulantEmbedding(obj)
# Suppose one requires an exponential, range = 2
# but marginal variance = 10 ( sigma in fields notation)
look3<- sqrt( 10)*circulantEmbedding(obj)
#Now with antique function
grid<- list( x= seq(0,5,length=100) , y= seq(0,5,len=150) )
obj<-Exp.image.cov(grid=grid, aRange=.75, setup=TRUE)
set.seed( 223)
look4<- sim.rf( obj)
# take a look at first 4
X11()
par(mfrow=c(2,2))
image.plot(grid[[1]], grid[[2]],look)
title("simulated gaussian fields")
image.plot( grid[[1]], grid[[2]],look2)
title("Second realization ...")
image.plot( grid[[1]], grid[[2]],look3)
title("Third realization ...")
image.plot(grid[[1]], grid[[2]],look4)
title("Fourth realization with sim.rf ...")
#calculando covarianzas entre conjuntos de puntos, esto sirve para las distancias
#entre observados y lugares a predecir por ej.
out2<- Exp.cov( ChicagoO3$x[6:20,],ChicagoO3$x[1:2,], aRange=100)

# an interesting 3D field
##el det de la matriz cov da casi cero
grid<- list(1:40, 1:40, 1:16)
obj<- circulantEmbeddingSetup(grid,cov.args=list(Covariance="Matern", aRange=2, smoothness=1.0))
# NOTE: choice of aRange is close to giving a negative weight array
set.seed(102)
look<- circulantEmbedding(obj)
# look at slices in the 3rd dimension
set.panel(4,4)
zr<- range(look)
par( mar=c(1,1,0,0))
for( k in 1:16){
  image(grid[[1]], grid[[2]], look[,,k], zlim= zr, col=tim.colors(256),
         axes=FALSE, xlab="", ylab="")
}

data(ozone2)
quilt.plot(ozone2$lon.lat, ozone2$y[16,],main="day 16")
bubblePlot(ozone2$lon.lat, ozone2$y[16,],main="day 16")
US( add=TRUE, col="magenta", lwd=2)

data(CO2)
#
# A quick look at the observations with world map
x11()
quilt.plot(CO2$lon.lat, CO2$y[20,])
bubblePlot(CO2$lon.lat, CO2$y[20,])
world(add=TRUE)
out<-fastTps(CO2$lon.lat, CO2$y, aRange=4, lambda=2.0)
#summary of fit note about 7300 degrees of freedom
# associated with fitted surface
print( out)
#image plot on a grid
surface( out, type="I", nx=300, ny=150)
#surface( out2, type="I", nx=300, ny=150)
out2<-fastTps(CO2$lon.lat, CO2$y, lon.lat=TRUE,lambda=1.5, aRange=4*68)
print(out2)
surface(out2, type="I", nx=300, ny=150)
#vector de colores
coltab<- two.colors( 256, middle="grey50")

#Wendland covariance 
##esta matriz da con determinante igual a cero, es de soporte compacto
dt<- seq( 0,1.5,, 200)
y<- Wendland(dt, k=2, dimension=2)
plot( dt, y, type="l")
y.test<- Wendland2.2( dt)
points( dt, y.test)
# these data are actually subsampled from a grid.
# create the image object that holds the data
#
temp<- matrix(NA, ncol=ncol(CO2.true$z), nrow=nrow(CO2.true$z))
temp[ CO2.true$mask] <- CO2$y
# look at gridded object.
image.plot(CO2.true$x,CO2.true$y, temp)
# to predict _exactly_ on this grid for the second fit;
# (this take a while)
data(CO2)
#surface( out2, type="I", nx=300, ny=150)
#Fits a thin plate spline surface and it uses a compactly supported Wendland covariance 
#is a special case of a Gaussian process estimate as the range parameter in the Matern family 
#increases to infinity. (Kriging).  GAM
#m-1 es el grado del polinomio para la tendencia, p la potencia polinomia para Wendland
out2<-fastTps(CO2$lon.lat, CO2$y, lon.lat=TRUE,lambda=1.5, aRange=4*68)
print(out2)
look<- predictSurface(out2, grid.list=list( x=CO2.true$x, y=CO2.true$y))
x11()
set.panel(2,1)
surface(out2, type="I", nx=300, ny=150)
image.plot(look)

#BD es un data.frame, la variable respuesta es BD$lnya
fit<- Tps(BD[,1:4], BD$lnya)  # fit surface to data 
# evaluate fitted surface for  first two variables holding other two fixed at median values
out.p<- predictSurface(fit)
surface(out.p, type="C") 
# plot surface for second and fourth variables on specific grid. 
glist<- list(KCL=29.77, MgCl2= seq(3,7,,25), KPO4=32.13, dNTP=seq(250,1500,,25))
out.p<- predictSurface(fit, glist)
surface(out.p, type="C")
#plot surface of standard deviation for second and fourth variables on specific grid. 
out.p<- predictSurfaceSE(fit, glist)
surface(out.p, type="C")

##la mas eficiente para m=2,k=2
look1<-predictSurface.fastTps(out2, grid.list=list( x=CO2.true$x, y=CO2.true$y))
image.plot(look1)
# second derivative
x11()
curve(Wendland(x, k=1, dimension=2, derivative=2),0,1.5,ylim=c(-25,12))
curve(Wendland(x, k=2, dimension=2, derivative=2),add=T,col=2)
curve(Wendland(x, k=3, dimension=2, derivative=2),add=T,col=3)
curve(Wendland(x, k=3, dimension=4, derivative=2),add=T,col=4)
#calculando la distancia a un knot
#El gráfico no funciona con matrices pequeñas, funciona a partir de matrices de dimensión 60x60
gl<- list( x= seq( -1,1,,250), y = seq( -1,1,,250) )
#matriz de distancias al knot
bigD<- rdist( make.surface.grid( gl), matrix( c(.25,.25), nrow=1))
#covMatrix
RBF<- matrix(Wendland( bigD, k=2, dimension=2), 250,250)
# perspective with some useful settings for shading.
persp( gl$x, gl$y, RBF, theta =30, phi=20, shade=.3, border=NA, col="grey90")

#ejemplo 2, dando el aRange
#ahora se guarda de forma compacta en un vector, solo los elementos del triáng inf
gl<- list( x= seq(-1,1,,65), y = seq(-1,1,,65))
#si no se le da ningún parámetro queda igual que distancia
dista=rdist(make.surface.grid(gl))
##para guardar solo el triángulo inferior
dista=rdist(make.surface.grid(gl),compact=TRUE)
diagVal = Exponential(0, range=0.3)#el range aquí no cumple ninguna función
compactCovMat = Exponential(dista, range=0.3)
upperCovMat = compactToMat(compactCovMat, diagVal)
lowerCovMat = compactToMat(compactCovMat, diagVal, lower.tri=TRUE, upper.tri=FALSE)
fullCovMat = compactToMat(compactCovMat, diagVal, lower.tri=TRUE, upper.tri=TRUE)

#con la Wendland , aRange hasta donde tiene en cuenta las distancias
diagVal = Wendland(0, aRange=0.3,k=2, dimension=2)#el range aquí no cumple ninguna función
compactCovMat = Wendland(dista,aRange=0.3,k=2,dimension=2)
upperCovMat = compactToMat(compactCovMat, diagVal)
lowerCovMat = compactToMat(compactCovMat, diagVal, lower.tri=TRUE, upper.tri=FALSE)
fullCovMatWendland=compactToMat(compactCovMat, diagVal, lower.tri=TRUE, upper.tri=TRUE)
#solo se puede graficar la superficie de covarianza con la distancia a un knot.

#evaluate Exponential covariance with range=1. Note that
#Covariance function is only evaluated over upper triangle
#so time is saved.

#Ejemplo suavizar para el mapa espacial
data(COmonthlyMet)
#Spatial plot of 1997 Spring average daily maximum temps
quilt.plot( CO.loc,CO.tmax.MAM[103,] )
US( add=TRUE)
title( "Recorded MAM max temperatures (1997)")
# min and max temperatures against elevation
matplot(CO.elev, cbind(CO.tmax.MAM[103,], CO.tmin.MAM[103,]),pch="o", type="p",
col=c("red", "blue"), xlab="Elevation (m)", ylab="Temperature (C)")
title("Recorded MAM max (red) and min (blue) temperatures 1997")
#Fitting a spatial model:
obj<- Tps(CO.loc,CO.tmax.MAM.climate,Z= CO.elev)
out<- spatialProcess(CO.loc,CO.tmax.MAM.climate,smoothness=1.0, Z= CO.elev)
surface(out)

#Opciones para la construcción de matrices de covarianza
ChicagoO3$x #Id y coordenadas planas
#matrices de covarianza
Cov_exp<- Exp.cov(ChicagoO3$x, aRange=100)
Cov_exp_dif_conjDpuntos<-Exp.cov(ChicagoO3$x[6:20,],ChicagoO3$x[1:2,], aRange=100)
Cov_matern<-stationary.cov(ChicagoO3$x, aRange=150, Covariance = "Matern",smoothness=1.5)
det(Cov_matern)
############################################################
###profundizar teoría
############################################################
Cov_RadCov<-Rad.cov(ChicagoO3$x, m=2, with.log = TRUE, with.constant = TRUE)
Cov_Taper<-stationary.taper.cov(ChicagoO3$x, Covariance="Exponential",Taper="Wendland",Taper.args=list(k=2,aRange=100,dimension=2)) 
##ver también https://search.r-#project.org/CRAN/refmans/CompRandFld/html/Covmatrix.html
##calculando directamente el producto para la stationary covar taper
Exponential(rdist(ChicagoO3$x)/100)*Wendland(rdist(ChicagoO3$x)/100,dimension=2,k=2)

##kriging, por defecto exponencial, m es el grado del polinomio de tendencia encontrado
#ajusta la superficie
fit <- Krig(ChicagoO3$x, ChicagoO3$y,m=2,aRange=20)  #tendencia cúbica
#fit1 <- Krig(ChicagoO3$x, ChicagoO3$y,m=1,aRange=20)  #tendencia cuadrática
#fit <- Krig(ChicagoO3$x, ChicagoO3$y,Covariance="Matern", aRange=10, smoothness=1.0)
summary(fit) # summary of fit
predictSE(fit)
set.panel(2,2)
plot(fit) # four diagnostic plots of fit
set.panel()
surface(fit, type="C") # look at the surface
#para predecir en los lugares observados, pero veo que hace lo mismo que la fun anterior
#predict(fit)
#out<- predictSurface( fit)
#surface( out, type="C") # option "C" our favorite
############################################################
#######
############################################################
# predict at arbitrary points (10,-10) and (20, 15)
xnew<- rbind(c( 10, -10), c( 20, 15))
predict(fit, xnew)
# standard errors of prediction based on covariance model.
predictSE(fit, xnew)
##out es idéntico a out1
out<- Krig(ChicagoO3$x, ChicagoO3$y,cov.function="Rad.cov",m=2,p=2,scale.type="range")
out1<-Tps(ChicagoO3$x, ChicagoO3$y)

# predict en una grilla de puntos
xnew<- cbind(seq(10,20,len=25), seq(-10,15,len=25))
predicc=predict(fit, xnew)
# standard errors of prediction based on covariance model.
SEpredicc=predictSE(fit, xnew)
set.panel(2,1)
plot(predicc)
plot(SEpredicc)
surface(predicc, type="C")
surface(SEpredicc, type="C")

############################################################
####
############################################################

# an example using a "Z" covariate and the Matern family, ver mKrigMLEGrid to choose parameters by MLE, 
##semiparamétrico, hace predicción en una grilla dispersa y luego hace el promedio de vecinos cercanos, eficiente
#una covariable en este ejemplo
data(COmonthlyMet)
yCO<- CO.tmin.MAM.climate
good<- !is.na( yCO)
yCO<-yCO[good]
xCO<- CO.loc[good,]
Z<- CO.elev[good]
out<- mKrig(xCO,yCO, Z=Z, cov.function="stationary.cov", Covariance="Matern",aRange=4.0, smoothness=1.0, lambda=.1)
##no corre el argumento grid.list
###como es  se le puede dar unos puntos de predicción
xnew<- cbind(seq(-109.4,-101,len=250), seq(37,41,len=250))
out<- mKrig(xCO,yCO, cov.function="stationary.cov", Covariance="Matern",aRange=4.0, smoothness=1.0, lambda=.1)
sim.out=predict(out,xnew=xnew)
set.panel(2,1)
# quilt.plot with elevations
quilt.plot(xCO, predict(out))
# Smooth surface without elevation linear term included
surface(out)
# a "Kriging" model. The covariance defaults to a Matern with smoothness 1.0.
# the nugget, sill and range parameters are found by maximum likelihood
# summary, plot, and surface also work for fit2 !
fit2<- spatialProcess(xCO,yCO)
surface(fit2)

############################################################
####Conditional simulation
##### función de simulación condicional restringida a fun de cov estacionarias
############################################################
#simulationGridList en este parámetro se le dan las ubicaciones de interés, si no se le da
#la función las crea dentro de los rangos 
data(ozone2)
set.seed(399)
# fit to day 16 from Midwest ozone data set.
##construir variando rango y parámetro de suavidad los datos originales por ejemplo con simulación no condicional
#o si se tienen datos construir esta superficie primero
outMat<- Krig(ozone2$lon.lat, ozone2$y[16,], Covariance="Matern",aRange=1.0,smoothness=1.0, na.rm=TRUE)
outWend<- Krig(ozone2$lon.lat, ozone2$y[16,], Covariance="Wendland",k=2, dimension=2, aRange=1.0, na.rm=TRUE)
outMat2<- Krig(ozone2$lon.lat, ozone2$y[16,], Covariance="Matern",aRange=0.8,smoothness=1.5, na.rm=TRUE)
# NOTE aRange =1.0 is not the best choice but
# allows the sim.rf circulant embedding algorithm to
# work without increasing the domain.
#six missing data locations
xp<- ozone2$lon.lat[is.na(ozone2$y[16,]),]
# M draws from process at xp given the data
# this is an exact calculation, una grilla de interés dada
##se podrían hacer varias simulaciones y tomarles media o mediana por punto
xp=matrix(c(seq(-82,-93,len=10),seq(37,44.4,len=10)),nrow=10,ncol=2,byrow=F)
sim.outMat<-sim.Krig(outMat,xp,M=1)
sim.outWen<-sim.Krig(outWend,xp,M=1)
sim.outMat2<-sim.Krig(outMat2,xp,M=1)
sim.outMat_table<-cbind(xp,t(sim.outMat))
colnames(sim.outMat_table)=c("x","y","z")
sim.outWen_table<-cbind(xp,t(sim.outWen))
colnames(sim.outWen_table)=c("x","y","z")
sim.outMat2_table<-cbind(xp,t(sim.outMat2))
colnames(sim.outMat2_table)=c("x","y","z")
set.panel(1,3)
boxplot(sim.outMat_table[,3])
boxplot(sim.outWen_table[,3])
boxplot(sim.outMat2_table[,3])
cvMat=sd(sim.outMat_table[,3])/mean(sim.outMat[,3])
cvWen=sd(sim.outWen_table[,3])/mean(sim.outWen[,3])
cvMat2=sd(sim.outMat2_table[,3])/mean(sim.outMat2[,3])

##por defecto usa 40 puntos y arroja la grilla en forma de matriz, cuadrícula, tridimensional
##si se simulan M>1  la simulaciones se van poniendo en capas
sim.out3<-sim.Krig.approx(out,nx=5,ny=4,gridRefinement=3,M=1)
sim.out3<-sim.Krig.approx(out,2,M=1)

data( ozone2)
set.seed( 399)
# fit to day 16 from Midwest ozone data set.
out<- Krig( ozone2$lon.lat, ozone2$y[16,], Covariance="Matern",
aRange=1.0,smoothness=1.0, na.rm=TRUE)
# NOTE aRange =1.0 is not the best choice but
# allows the sim.rf circulant embedding algorithm to
# work without increasing the domain.
#six missing data locations
xp<- ozone2$lon.lat[ is.na(ozone2$y[16,]),]
# 5 draws from process at xp given the data
# this is an exact calculation
sim.Krig( out,xp, M=5)-> sim.out

#Compare: stats(sim.out)[3,] to Exact: predictSE( out, xp)
#simulations on a grid NOTE this is approximate due to the bilinear interpolation for simulating the unconditional random field. also more grids points ( nx and ny) should be used
sim.Krig.approx(out,M=5, nx=20,ny=20)-> sim.out
# take a look at the ensemble members.
predictSurface( out, grid= list( x=sim.out$x, y=sim.out$y))-> look
zr<- c( 40, 200)
set.panel( 3,2)
image.plot( look, zlim=zr)
title("mean surface")
for (k in 1:5){image( sim.out$x, sim.out$y, sim.out$z[,,k], col=tim.colors(), zlim =zr)}

########################################################################################################################
##conditional simulation based on semiparametric micro kriging, combinación kriging y el vecino mas cercano
########################################################################################################################
# conditional simulation at missing data
xMissing<- ozone2$lon.lat[!good,]
O3.sim2<- sim.mKrig.approx( O3.fit, xMissing, nx=80, ny=80,gridRefinement=3, M=4 )
#################% Cinco
###simulaciones del mismo proceso 
data(ozone2)
y<- ozone2$y[16,]
good<- !is.na( y)
y<-y[good]
x<- ozone2$lon.lat[good,]
O3.fit<- mKrig( x,y, Covariance="Matern", aRange=.5,smoothness=1.0, lambda= .01 )
set.seed(122)
O3.sim<- sim.mKrig.approx( O3.fit, nx=100, ny=100, gridRefinement=3, M=5 )
set.panel(3,2)
surface( O3.fit)
for ( k in 1:5){
image.plot( as.surface( O3.sim$predictionPoints, O3.sim$Ensemble[,k]) )
}
# conditional simulation at missing data
xMissing<- ozone2$lon.lat[!good,]
O3.sim2<- sim.mKrig.approx( O3.fit, xMissing, nx=80, ny=80,gridRefinement=3, M=4 )

########################################################################################################################
###muy eficiente computacionalmente, simular condicionando sobre la superficie suavizada 
########################################################################################################################
data(ozone2)
y<- ozone2$y[16,]
good<- !is.na( y)
y<-y[good]
x<- ozone2$lon.lat[good,]
O3Obj<- fastTps( x,y, aRange=1.5 )
# creating a quick grid list based on ranges of locations
grid.list<- fields.x.to.grid( O3Obj$x, nx=10, ny=10)
# controlling the grids
xR<- range( x[,1], na.rm=TRUE)
yR<- range( x[,2], na.rm=TRUE)
simulationGridList<- list( x= seq(xR[1],xR[2],,20),
y= seq( yR[1],yR[2], ,20))
# very fine localized prediction grid
O3GridList<- list( x= seq( -90.5,-88.5,,40), y= seq( 38,40,,40))
O3Sim<- sim.mKrig.approx(O3Obj, M=1, predictionPointsList=O3GridList,simulationGridList = simulationGridList)












