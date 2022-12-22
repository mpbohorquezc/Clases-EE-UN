########################################################################
#paper 3
#########################################################################

rm(list=ls())

library(sp)
library(gstat)

#################simulación funciones periódicas con bases de Fourier#################
#The Fourier basis is an orthonormal basis of L2, over any interval of length 2pi#

#eigenfunctions1 Fourier basis [0,2pi]  o  [a,a+2pi]
en=function(x,n){((sqrt(2*pi))^(-1))*exp(1i*n*x)}
#Ejemplo 0.005*en(x,1)+5*en(x,2)
curve(0.005*en(x,1)+5*en(x,2),0,2*pi)

#eigenfunctions2 series Fourier [-L,L]
seno_fourier=function(x,n,L){sqrt(2/L)*sin(n*pi*x/L)}
coseno_Fourier=function(x,n,L){sqrt(2/L)*cos(n*pi*x/L)}
#series_Fourier=function(x,n,L){1/sqrt(L)+seno_fourier(x,1,L)+...+seno_fourier(x,n,L)+coseno_fourier(x,1,L)+...+coseno_fourier(x,n,L)}

##En L2[-1/2,1/2] n=1
integrand <- function(x) {(sqrt(2)*sin(2*1*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=2
integrand <- function(x) {(sqrt(2/1)*sin(2*2*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=3
integrand <- function(x) {(sqrt(2/1)*sin(2*3*x*pi))^2}
integrate(integrand, lower = 0, upper =1)

###coseno verificando que todos tienen norma 1
coseno_Fourier=function(x,n,L){sqrt(2/L)*cos(n*pi*x/L)}
##En L2[-1/2,1/2] n=1
integrand <- function(x) {(sqrt(2)*cos(2*1*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=2
integrand <- function(x) {(sqrt(2/1)*cos(2*2*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=3
integrand <- function(x) {(sqrt(2/1)*cos(2*3*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=4
integrand <- function(x) {(sqrt(2/1)*cos(2*4*x*pi))^2}
integrate(integrand, lower = 0, upper =1)

###seno verificando que todos tienen norma 1
seno_fourier=function(x,n,L){sin(n*pi*x/L)}
##En L2[0,1] n=1
integrand <- function(x) {(sqrt(2)*sin(2*1*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=2
integrand <- function(x) {(sqrt(2/1)*sin(2*2*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=3
integrand <- function(x) {(sqrt(2/1)*sin(2*3*x*pi))^2}
integrate(integrand, lower = 0, upper =1)

###coseno verificando que todos tienen norma 1
coseno_Fourier=function(x,n,L){sqrt(2/L)*cos(n*pi*x/L)}
##En L2[-1/2,1/2] n=1
integrand <- function(x) {(sqrt(2)*cos(2*1*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=2
integrand <- function(x) {(sqrt(2/1)*cos(2*2*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=3
integrand <- function(x) {(sqrt(2/1)*cos(2*3*x*pi))^2}
integrate(integrand, lower = 0, upper =1)
##En L2[-1/2,1/2] n=4
integrand <- function(x) {(sqrt(2/1)*cos(2*4*x*pi))^2}
integrate(integrand, lower = 0, upper =1)

### verificando que todos son ortogonales
integrand <- function(x) {(sqrt(2)*cos(2*1*x*pi))*(sqrt(2/1)*sin(2*5*x*pi))}
integrate(integrand, lower = 0, upper =1)
integrand <- function(x) {(sqrt(2)*cos(2*2*x*pi))*(sqrt(2/1)*cos(2*3*x*pi))}
integrate(integrand, lower = 0, upper =1)
integrand <- function(x) {(sqrt(2)*sin(2*1*x*pi))*(sqrt(2/1)*sin(2*5*x*pi))}
integrate(integrand, lower = 0, upper =1)

#Ejemplo 0.0005*1+0.005*seno_fourier(x,1,5)+5*coseno_Fourier(x,1,5) 

#eigenfunctions3 Legendre, ver conway pag18 ejercicio 6, L^2[-1,1]
v1=function(x){sqrt(1/2)}
v2=function(x){x*sqrt(3/2)}
v3=function(x){(3/2)*(sqrt(5/2))*(-(1/3)+x^2)}
v4=function(x){(5/2)*(sqrt(7/2))*(-(3*x/5)+x^3)}
#Ejemplo 0.005*5sqrt(1/2)+0.05*v2(x)+0.5*v3(x)+7*v4(x)

integrand=function(x){((3/2)*(sqrt(5/2))*(-(1/3)+x^2))*((5/2)*(sqrt(7/2))*(-(3*x/5)+x^3))}


#eigenfunctions4 Hermite, ver conway pag18 ejercicio 7

#eigenfunctions5 Laguerre, ver conway pag18 ejercicio 8

####################generación del proceso espacial multivariado de scores#######################

##Caso 1 Dos variables 
##Supongamos un solo vector propio completa mas del % requerido ejem. 90% para ambas funciones

#chi1=f11*v1+...+f1k*vk+...    
#chi2=f21*v1+...+f1k*vk+...    
#...
#chin=fn1*v1+...+fnk*vk+...    

#F1=c(f11,...,fn1)
#G11=c(g11,...,gn1)
#Cov_F1=exponencial, silla valor propio 1 (9), rango 1, no pepita
#Cov_G1=exponencial, silla valor propio 1 (4), rango 2, no pepita
#cross_covFG=exponencial, silla valor propio 1 (5), rango 2, no pepita

#to ensure the variogram model of the vector with two processes is valid is enough to verify that abs(sigma12)<=sqrt(sigma11*sigma22)
#Total variability 5.4
xi1=5*en(x,2)
x12=0.05*en(x,1)
#Total variability 8.6
zeta1=8*en(x,4)
zeta2=0.4*en(x,7)

xy <- expand.grid(1:3, 1:4)
names(xy) <- c("x","y")
g<- gstat(id="F1",formula = F1~1, dummy=T,locations = ~x+y,beta=5)
g<- gstat(g,id="G1",formula = G1~1, dummy=T,locations = ~x+y,beta=10)
g<-gstat(g,id="F1",model = vgm(9,"Mat",30, kappa=1,add.to=vgm(1,"Sph",10)), nmax = 20)
g<-gstat(g,id="G1",model = vgm(1,"Mat",30, kappa=1,add.to=vgm(4,"Sph",10)), nmax = 20)
g<- gstat(g,id=c("F1","G1"),model = vgm(2,"Mat",30,kappa=1,add.to=vgm(1.5,"Sph",10)), nmax = 20)
yy <- predict(g, newdata = xy, nsim = 2)

yydatos1=data.frame(x=yy[,1],y=yy[,2],Fsim1=yy[,3],Gsim1=yy[,5)
coordinates(yy)=~x+y
gsim1 <- gstat(id = "Fsim1", formula = Fsim1~1,  data=yy)
gsim1 <- gstat(gsim1, id = "Gsim1", formula = Gsim1~1,  data = yy)
plot(variogram(gsim1))

yydatos2=data.frame(x=yy[,1],y=yy[,2],Fsim2=yy[,4],Gsim2=yy[,6])
coordinates(yydatos2)=~x+y
gsim2 <- gstat(id = "Fsim2", formula = Fsim2~1, data=yy)
gsim2 <- gstat(gsim2, id = "Gsim2", formula = Gsim2~1,  data = yy)
x11()
plot(variogram(gsim2))

#Para graficar
plot(variogramLine(vgm(9,"Mat",30, kappa=1,add.to=vgm(0,"Sph",10)), maxdist=10),type="l")

chi_i=function(xi,f){f*xi}


#simulación condicional 
#datos=
#x=c(1,2,3,4)
#y=c(2,3,4)
#grilla=expand.grid(x,y)
#...

integrand <- function(x) {(1/(pi))*(sin(x*pi))^2}
integrate(integrand, lower = 0, upper = 2*pi)

integrand <- function(x){(sqrt(2/1)*sin(2*pi*x/1))^2}






