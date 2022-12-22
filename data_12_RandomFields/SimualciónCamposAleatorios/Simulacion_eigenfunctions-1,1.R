


u2=function(x) {(sqrt(2/2)*cos(2*2*x*pi/2))}
u3=function(x) {(sqrt(2/2)*cos(2*3*x*pi/2))}
curve(v1,-1,1,xlab=expression(italic(t)),ylab="",cex.lab=1.5,cex.axis=1.2)
curve(v2,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=2)
curve(v3,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=3)

integrand <- function(x) {(sin(pi*x/2))^2}
integrate(integrand, lower = 0, upper =1)
integrand <- function(x) {(cos(pi*x/2))^2}
integrate(integrand, lower = 0, upper =1)
integrand <- function(x) {(sin(pi*x/2))*(cos(pi*x/2))}
integrate(integrand, lower = 0, upper =1)

integrand <- function(x) {(sin(pi*x/2))*(cos(pi*x/2))}
integrate(integrand, lower = -1, upper =1)

curve(u1,-1,1,xlab=expression(italic(t)),ylab="",cex.lab=1.5,cex.axis=1.2)
curve(u2,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=5)
curve(u3,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=6)

##############En L2[-1,1] n=1
integrand <- function(x) {(sqrt(2/2)*sin(2*1*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)
##En L2[-1,1] n=2
integrand <- function(x) {(sqrt(2/2)*sin(2*2*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)
##En L2[-1,1] n=3
integrand <- function(x) {(sqrt(2/2)*sin(2*3*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)

###########coseno verificando que todos tienen norma 1
coseno_Fourier=function(x,n,L){sqrt(2/L)*cos(n*pi*x/L)}
##En L2[-1,1] n=1
integrand <- function(x) {(sqrt(2/2)*cos(2*1*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)
##En L2[-1,1] n=2
integrand <- function(x) {(sqrt(2/2)*cos(2*2*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)
##En L2[-1,1] n=3
integrand <- function(x) {(sqrt(2/2)*cos(2*3*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)
##En L2[-1,1] n=4
integrand <- function(x) {(sqrt(2/2)*cos(2*4*x*pi/2))^2}
integrate(integrand, lower = -1, upper =1)

###### verificando que todos son ortogonales
integrand <- function(x) {(sqrt(2/2)*cos(2*2*x*pi/2))*(sqrt(2/2)*sin(2*2*x*pi/2))}
integrate(integrand, lower = -1, upper =1)
integrand <- function(x) {(sqrt(2/2)*cos(2*2*x*pi/2))*(sqrt(2/2)*cos(2*3*x*pi/2))}
integrate(integrand, lower = -1, upper =1)
integrand <- function(x) {(sqrt(2/2)*sin(2*1*x*pi/2))*(sqrt(2/2)*sin(2*2*x*pi/2))}
integrate(integrand, lower = -1, upper =1)

##################Legendre ver conway pag18 ejercicio 6, L^2[-1,1]
v1=function(x){sqrt(1/2)}
v2=function(x){x*sqrt(3/2)}
v3=function(x){(sqrt(5/2))*(1/8)*(12*x^2-4)}
v4=function(x){(5/2)*(sqrt(7/2))*(-(3*x/5)+x^3)}
#Ejemplo 0.005*5sqrt(1/2)+0.05*v2(x)+0.5*v3(x)+7*v4(x)

curve(v2,-1,1,xlab=expression(italic(t)),ylab="",cex.lab=1.5,cex.axis=1.2)
curve(v3,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=2)
curve(v4,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=3)

##Verificando norma uno v2
integrand <- function(x){((5/2)*(sqrt(7/2))*(-(3*x/5)+x^3))^2}
integrate(integrand, lower = -1, upper =1)

integrand <- function(x){(x*sqrt(3/2))^2}
integrate(integrand, lower = -1, upper =1)

##Verificando norma uno v3
integrand <- function(x){((sqrt(5/2))*(1/8)*(12*x^2-4))^2}
integrate(integrand, lower = 0, upper =1)

##Verificando norma uno v4
integrand <- function(x){(x*sqrt(3/2))*((sqrt(5/2))*(1/8)*(12*x^2-4))}
integrate(integrand, lower = 0, upper =1)

##Verificando ortogonalidad v2 v3
integrand <- function(x){((x*sqrt(3/2)))*((sqrt(5/2))*(1/8)*(12*x^2-4)))}
integrate(integrand, lower = -1, upper =1)

##Verificando ortogonalidad v2 v4
integrand <- function(x){((x*sqrt(3/2)))*((5/2)*(sqrt(7/2))*(-(3*x/5)+x^3))}
integrate(integrand, lower = 0, upper =1)

##Verificando ortogonalidad v3 v4
integrand <- function(x){((sqrt(5/2))*(1/8)*(12*x^2-4))*((5/2)*(sqrt(7/2))*(-(3*x/5)+x^3))}
integrate(integrand, lower = -1, upper =1)

##Chebyshev
v1=function(x){(sqrt(3/2)*x}
v2=function(x){(sqrt(15/14))*cos(2*acos(x))}
v3=function(x){sqrt(1/0.9714286)*cos(3*acos(x))}
##Verificando ortogonalidad v1 v2

curve(v2,-1,1,xlab=expression(italic(t)),ylab="",cex.lab=1.5,cex.axis=1.2)
curve(v3,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=2)
curve(v4,-1,1,add=T,ylab="",cex.lab=1.5,cex.axis=1.2,col=3)

######norma v1
integrand <- function(x){((sqrt(3/2))*x)^2}
integrate(integrand, lower = -1, upper =1)

######norma v2
integrand <- function(x){((sqrt(15/14))*cos(2*acos(x)))^2}
integrate(integrand, lower = -1, upper =1)

######norma v3
integrand <- function(x){(sqrt(1/0.9714286)*cos(3*acos(x)))^2}
integrate(integrand, lower = -1, upper =1)

###### v1 v2
integrand <- function(x){(sqrt(3/2)*x)*((sqrt(15/14))*cos(2*acos(x)))}
integrate(integrand, lower = -1, upper =1)


###### v1 v3
integrand <- function(x){(sqrt(3/2)*x)*(sqrt(1/0.9714286)*cos(3*acos(x)))}
integrate(integrand, lower = -1, upper =1)

###### v2 v3
integrand <- function(x){(sqrt(1/0.9714286)*cos(3*acos(x)))*((sqrt(15/14))*cos(2*acos(x)))}
integrate(integrand, lower = -1, upper =1)

#############Simulaciones scores
library(MSBVAR)
x=runif(25,0,1)
y=runif(25,0,1)
grilla=data.frame(x,y)
plot(grilla,xlab=expression(italic(x)),ylab=expression(italic(y)),cex.lab=1.2,cex.axis=1.2)
sigma_t=function(t){0.1*t}
b_t=function(t){0.15*t}
exp_esp=function(sigma,h){((sigma)^2)*exp(-h/0.8)}
Gaus_esp=function(sigma,h){((sigma)^2)*exp(-h^2/0.8)}
Sph_esp1=function(sigma,h){ifelse(h<=0.9,((sigma)^2)*(1-1.5*(h/0.9)+0.5*h^3/(0.9^3)),0)}
Sph_esp2=function(sigma,h){ifelse(h<=0.5,((sigma)^2)*(1-1.5*(h/0.5)+0.5*h^3/(0.5^3)),0)}

grilla=read.table("grilla.txt")
#t el número total de tiempo a ser simulado se puede dar como parametro t=1:Nt, grilla las n ubicaciones a ser simuladas
mu=rep(0,25)
#f11
h=as.matrix(dist(grilla))
vmat=exp_esp(5,h)+Sph_esp2(3,h)
f11=rmultnorm(1,mu=mu,vmat=vmat)

#f22
h=as.matrix(dist(grilla))
vmat=exp_esp(4,h)+Sph_esp2(1,h)
f22=rmultnorm(1,mu=mu,vmat=vmat)

#f12
h=as.matrix(dist(grilla))
vmat=exp_esp(4,h)+Sph_esp2(1,h)
f12=rmultnorm(1,mu=mu,vmat=vmat)

#f23
h=as.matrix(dist(grilla))
vmat=exp_esp(4,h)+Sph_esp2(1,h)
f23=rmultnorm(1,mu=mu,vmat=vmat)

mu=rep(0,25)
curve(exp_esp(5,x),ylim=c(-2,40))
curve(Sph_esp2(2,x),add=T,col=2)
curve(exp_esp(5,x)+Sph_esp2(3,x),add=T,col=3)
curve(Sph_esp2(3,x),col=2,add=T)
Sim_Cov_t=function(modelo,grilla)
{simulados=data.frame()
 h=as.matrix(dist(grilla))
 simulados=rbind(simulados,data.frame(grilla,t(rmultnorm(1, mu=mu,vmat=modelo(10,h)))))
 colnames(simulados)=c("x","y","Z")
 simulados
} 

curve(f11[1]*v3(x)+f22[1]*v4(x),-1,1,ylim=c(-5,5))
curve(f11[i]*v4(x),add=T)
curve(f11[2]*(sqrt(7/2))*(-(3*x/5)+x^3),add=T)

(-(3*x/5)+x^3),add=T)
x=seq(-1,1,len=250)
#for(i in 1:25)
dato1f11=(f11[1]*v2(x)+f12[1]*v3(x)+f23[1]*v4(x))
dato2f11=(f11[2]*v2(x)+f12[2]*v3(x)+f23[2]*v4(x))
dato3f11=(f11[3]*v2(x)+f12[3]*v3(x)+f23[3]*v4(x))
dato4f11=(f11[4]*v2(x)+f12[4]*v3(x)+f23[4]*v4(x))
dato5f11=(f11[5]*v2(x)+f12[5]*v3(x)+f23[5]*v4(x))
dato6f11=(f11[6]*v2(x)+f12[6]*v3(x)+f23[6]*v4(x))
dato7f11=(f11[7]*v2(x)+f12[7]*v3(x)+f23[7]*v4(x))
dato8f11=(f11[8]*v2(x)+f12[8]*v3(x)+f23[8]*v4(x))
dato9f11=(f11[9]*v2(x)+f12[9]*v3(x)+f23[9]*v4(x))
dato10f11=(f11[10]*v2(x)+f12[10]*v3(x)+f23[10]*v4(x))
dato11f11=(f11[11]*v2(x)+f12[11]*v3(x)+f23[11]*v4(x))
dato12f11=(f11[12]*v2(x)+f12[12]*v3(x)+f23[12]*v4(x))
dato13f11=(f11[13]*v2(x)+f12[13]*v3(x)+f23[13]*v4(x))
dato14f11=(f11[14]*v2(x)+f12[14]*v3(x)+f23[14]*v4(x))
dato15f11=(f11[15]*v2(x)+f12[15]*v3(x)+f23[15]*v4(x))
dato16f11=(f11[16]*v2(x)+f12[16]*v3(x)+f23[16]*v4(x))
dato17f11=(f11[17]*v2(x)+f12[17]*v3(x)+f23[17]*v4(x))
dato18f11=(f11[18]*v2(x)+f12[18]*v3(x)+f23[18]*v4(x))
dato19f11=(f11[19]*v2(x)+f12[19]*v3(x)+f23[19]*v4(x))
dato20f11=(f11[20]*v2(x)+f12[20]*v3(x)+f23[20]*v4(x))
dato21f11=(f11[21]*v2(x)+f12[21]*v3(x)+f23[21]*v4(x))
dato22f11=(f11[22]*v2(x)+f12[22]*v3(x)+f23[22]*v4(x))
dato23f11=(f11[23]*v2(x)+f12[23]*v3(x)+f23[23]*v4(x))
dato24f11=(f11[24]*v2(x)+f12[24]*v3(x)+f23[24]*v4(x))
dato25f11=(f11[25]*v2(x)+f12[25]*v3(x)+f23[25]*v4(x))

datosf11=data.frame(dato1f11,dato2f11,dato3f11,dato4f11,dato5f11,dato6f11,dato7f11,dato8f11,dato9f11,dato10f11,
                    dato11f11,dato12f11,dato13f11,dato14f11,dato15f11,dato16f11,dato17f11,dato18f11,dato19f11,dato20f11,
                    dato21f11,dato22f11,dato23f11,dato24f11,dato25f11)

datosf11=data.frame(dato1f11+runif(250,-5,5),dato2f11+runif(250,-5,5),dato3f11+runif(250,5,5),dato4f11+runif(250,-5,5),dato5f11+runif(250,-5,5),
dato6f11+runif(250,-5,5),dato7f11+runif(250,-5,5),dato8f11+runif(250,-5,5),dato9f11+runif(250,-5,5),dato10f11+runif(250,-5,5),
                    dato11f11+runif(250,-5,5),dato12f11+runif(250,-5,5),dato13f11+runif(250,-5,5),dato14f11+runif(250,-5,5),dato15f11+runif(250,-5,5),
dato16f11+runif(250,-5,5),dato17f11+runif(250,-5,5),dato18f11+runif(250,-5,5),dato19f11+runif(250,-5,5),dato20f11+runif(250,-5,5),
                    dato21f11+runif(250,-5,5),dato22f11+runif(250,-5,5),dato23f11+runif(250,-5,5),dato24f11+runif(250,-5,5),dato25f11+runif(250,-5,5))


datosf11=data.frame(dato1f11+runif(250,-7,7),dato2f11+runif(250,-7,7),dato3f11+runif(250,-7,7),dato4f11+runif(250,-7,7),dato5f11+runif(250,-7,7),
dato6f11+runif(250,-7,7),dato7f11+runif(250,-7,7),dato8f11+runif(250,-7,7),dato9f11+runif(250,-7,7),dato10f11+runif(250,-7,7),
                    dato11f11+runif(250,-7,7),dato12f11+runif(250,-7,7),dato13f11+runif(250,-7,7),dato14f11+runif(250,-7,7),dato15f11+runif(250,-7,7),
dato16f11+runif(250,-7,7),dato17f11+runif(250,-7,7),dato18f11+runif(250,-7,7),dato19f11+runif(250,-7,7),dato20f11+runif(250,-7,7),
                    dato21f11+runif(250,-7,7),dato22f11+runif(250,-7,7),dato23f11+runif(250,-7,7),dato24f11+runif(250,-7,7),dato25f11+runif(250,-7,7))




datosTT=ts(datosf11)
plot(datosTT)
library(fda.usc)
Mdatosf11=as.matrix(datosf11,nrow=50,ncol=25)
#ID_estaciones=c("1","2","3","4","5","6","7","8","9")
datosf=fdata(Mdatosf11,argvals=1:nrow(Mdatosf11))
nbasis <-459
hourange <- c(1,nrow(Mdatosf11))
lambda=0,0000001
harmaccelLfd <- vec2Lfd(c(1,15), hourange)
hourbasis_Bsplines <- create.bspline.basis(hourange,nbasis)
PM10_fdPar_Bspline_sim<-fdPar(fdobj=hourbasis_Bsplines,Lfdobj=harmaccelLfd,lambda)
PM10_fd_Bspline_sim <- smooth.basis(argvals=1:nrow(Mdatosf11),Mdatosf11,PM10_fdPar_Bspline_sim)
PM10_fd_Bspl_sim=PM10_fd_Bspline_sim$fd
plot(PM10_fd_Bspl_sim[-3],ylim=c(-12,12),xlim=c(6,245),lty=1,lwd=2,cex=1,col=rainbow(25),ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Simulated data")
#lines(PM10_fd_Bspl,col=rainbow(25),lwd=2,lty=1)
#title(ylab=expression(chi[s]^1),cex.lab=1.2,mgp=c(2.5,0,0))
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi)[s]^1),xpd = T,cex=2)

x11()
Mdatosf11pred=as.matrix(datosf11Pred,nrow=50,ncol=25)
#ID_estaciones=c("1","2","3","4","5","6","7","8","9")
datosf=fdata(Mdatosf11pred,argvals=1:nrow(Mdatosf11pred))
nbasis <-99
hourange <- c(1,nrow(Mdatosf11pred))
lambda=0.001
harmaccelLfd <- vec2Lfd(c(1,10), hourange)
hourbasis_Bsplines <- create.bspline.basis(hourange,nbasis)
PM10_fdPar_Bspline<-fdPar(fdobj=hourbasis_Bsplines,Lfdobj=harmaccelLfd,lambda)
PM10_fd_Bspline <- smooth.basis(argvals=1:nrow(Mdatosf11pred),Mdatosf11pred,PM10_fdPar_Bspline)
PM10_fd_Bspl=PM10_fd_Bspline$fd
plot(PM10_fd_Bspl,ylim=c(-9,9),xlim=c(5,45),col=rainbow(25),lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Functional cokriging")
#lines(PM10_fd_Bspl,col=rainbow(25),lwd=2,lty=1)
#title(ylab=expression(bold(chi)[s]^1),cex.lab=1.2,mgp=c(2.5,0,0))
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi)[s]^1),xpd = T,cex=2)

x11()
plot(PM10_fd_Bspl[7]-PM10_fd_Bspl_sim[7],ylim=c(-1.5,1.5),xlim=c(5,45),col=7,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[2]-PM10_fd_Bspl_sim[2],ylim=c(-2,2),xlim=c(5,45),col=2,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[4]-PM10_fd_Bspl_sim[4],ylim=c(-2,2),xlim=c(5,45),col=4,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[5]-PM10_fd_Bspl_sim[5],ylim=c(-2,2),xlim=c(5,45),col=5,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[6]-PM10_fd_Bspl_sim[6],ylim=c(-2,2),xlim=c(5,45),col=6,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi[pred])[s]^1),xpd = T,cex=2)

x11()
plot(PM10_fd_Bspl[1]-PM10_fd_Bspl_sim[1],ylim=c(-4,4),xlim=c(5,45),col=8,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Residual functions")
lines(PM10_fd_Bspl[2]-PM10_fd_Bspl_sim[2],ylim=c(-2,2),xlim=c(5,45),col=2,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[3]-PM10_fd_Bspl_sim[3],ylim=c(-2,2),xlim=c(5,45),col=3,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[4]-PM10_fd_Bspl_sim[4],ylim=c(-2,2),xlim=c(5,45),col=4,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[5]-PM10_fd_Bspl_sim[5],ylim=c(-2,2),xlim=c(5,45),col=5,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[6]-PM10_fd_Bspl_sim[6],ylim=c(-2,2),xlim=c(5,45),col=6,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[7]-PM10_fd_Bspl_sim[7],ylim=c(-2,2),xlim=c(5,45),col=7,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[8]-PM10_fd_Bspl_sim[8],ylim=c(-2,2),xlim=c(5,45),col=8,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
#lines(PM10_fd_Bspl[9]-PM10_fd_Bspl_sim[9],ylim=c(-2,2),xlim=c(5,45),col="grey",lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi[pred])[s]^1),xpd = T,cex=2)
legend("topleft",legend=1:8,lwd=2.5,cex=1,col=1:8,horiz=T,seg.len=1.1)

x11()
plot(PM10_fd_Bspl_sim[15],ylim=c(-15,11),xlim=c(5,45),col=1,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
lines(PM10_fd_Bspl[15],ylim=c(-1,1),xlim=c(5,45),col=2,lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2)
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(1.6,4),expression(bold(chi)[s[15]]^1),xpd = T,cex=2)
legend("topleft",legend=c("Simulated", "Prediction"),lwd=2.5,cex=1.2,col=1:2,horiz=T,seg.len=2)

x11()
datosTT=ts(datosf11)
ts.plot(datosTT,col=rainbow(25),ylim=c(-20,20),xlim=c(6,45),type="l")
###########################################################################
f22
h=as.matrix(dist(grilla))
vmat=exp_esp(4,h)+Sph_esp2(1,h)
f22=rmultnorm(1,mu=mu,vmat=vmat)

curve(exp_esp(5,x),ylim=c(-2,40))
curve(Sph_esp2(2,x),add=T,col=2)
curve(exp_esp(5,x)+Sph_esp2(3,x),add=T,col=3)
curve(Sph_esp2(3,x),col=2,add=T)
Sim_Cov_t=function(modelo,grilla)
{simulados=data.frame()
 h=as.matrix(dist(grilla))
 simulados=rbind(simulados,data.frame(grilla,t(rmultnorm(1, mu=mu,vmat=modelo(10,h)))))
 colnames(simulados)=c("x","y","Z")
 simulados
} 

curve(f11[1]*v3(x)+f22[1]*v4(x),-1,1,ylim=c(-5,5))
curve(f11[i]*v4(x),add=T)
curve(f11[2]*(sqrt(7/2))*(-(3*x/5)+x^3),add=T)
x=seq(-1,1,len=250)
#for(i in 1:25)
dato1f22=(f22[1]*v2(x)+f12[1]*v3(x)+f23[1]*v4(x))*rnorm(250)
dato2f22=(f22[2]*v2(x)+f12[2]*v3(x)+f23[2]*v4(x))*rnorm(250)
dato3f22=(f22[3]*v2(x)+f12[3]*v3(x)+f23[3]*v4(x))*rnorm(250)
dato4f22=(f22[4]*v2(x)+f12[4]*v3(x)+f23[4]*v4(x))*rnorm(250)
dato5f22=(f22[5]*v2(x)+f12[5]*v3(x)+f23[5]*v4(x))*rnorm(250)
dato6f22=(f22[6]*v2(x)+f12[6]*v3(x)+f23[6]*v4(x))*rnorm(250)
dato7f22=(f22[7]*v2(x)+f12[7]*v3(x)+f23[7]*v4(x))*rnorm(250)
dato8f22=(f22[8]*v2(x)+f12[8]*v3(x)+f23[8]*v4(x))*rnorm(250)
dato9f22=(f22[9]*v2(x)+f12[9]*v3(x)+f23[9]*v4(x))*rnorm(250)
dato10f22=(f22[10]*v2(x)+f12[10]*v3(x)+f23[10]*v4(x))*rnorm(250)
dato11f22=(f22[11]*v2(x)+f12[11]*v3(x)+f23[11]*v4(x))*rnorm(250)
dato12f22=(f22[12]*v2(x)+f12[12]*v3(x)+f23[12]*v4(x))*rnorm(250)
dato13f22=(f22[13]*v2(x)+f12[13]*v3(x)+f23[13]*v4(x))*rnorm(250)
dato14f22=(f22[14]*v2(x)+f12[14]*v3(x)+f23[14]*v4(x))*rnorm(250)
dato15f22=(f22[15]*v2(x)+f12[15]*v3(x)+f23[15]*v4(x))*rnorm(250)
dato16f22=(f22[16]*v2(x)+f12[16]*v3(x)+f23[16]*v4(x))*rnorm(250)
dato17f22=(f22[17]*v2(x)+f12[17]*v3(x)+f23[17]*v4(x))*rnorm(250)
dato18f22=(f22[18]*v2(x)+f12[18]*v3(x)+f23[18]*v4(x))*rnorm(250)
dato19f22=(f22[19]*v2(x)+f12[19]*v3(x)+f23[19]*v4(x))*rnorm(250)
dato20f22=(f22[20]*v2(x)+f12[20]*v3(x)+f23[20]*v4(x))*rnorm(250)
dato21f22=(f22[21]*v2(x)+f12[21]*v3(x)+f23[21]*v4(x))*rnorm(250)
dato22f22=(f22[22]*v2(x)+f12[22]*v3(x)+f23[22]*v4(x))*rnorm(250)
dato23f22=(f22[23]*v2(x)+f12[23]*v3(x)+f23[23]*v4(x))*rnorm(250)
dato24f22=(f22[24]*v2(x)+f12[24]*v3(x)+f23[24]*v4(x))*rnorm(250)
dato25f22=(f22[25]*v2(x)+f12[25]*v3(x)+f23[25]*v4(x))*rnorm(250)

datosf22=data.frame(dato1f22,dato2f22,dato3f22,dato4f22,dato5f22,dato6f22,dato7f22,dato8f22,dato9f22,dato10f22,
                    dato11f22,dato12f22,dato13f22,dato14f22,dato15f22,dato16f22,dato17f22,dato18f22,dato19f22,dato20f22,
                    dato21f22,dato22f22,dato23f22,dato24f22,dato25f22)
datosTT=tseries(datosf22)

Mdatosf22=as.matrix(datosf22,nrow=50,ncol=25)
#ID_estaciones=c("1","2","3","4","5","6","7","8","9")
datosf=fdata(Mdatosf22,argvals=1:nrow(Mdatosf22))
nbasis <-1501
hourange <- c(1,nrow(Mdatosf22))
lambda=0.000001
harmaccelLfd <- vec2Lfd(c(1,10), hourange)
hourbasis_Bsplines <- create.bspline.basis(hourange,nbasis)
PM10_fdPar_Bspline<-fdPar(fdobj=hourbasis_Bsplines,Lfdobj=harmaccelLfd,lambda)
PM10_fd_Bspline <- smooth.basis(argvals=1:nrow(Mdatosf22),Mdatosf22,PM10_fdPar_Bspline)
PM10_fd_Bspl=PM10_fd_Bspline$fd
plot(PM10_fd_Bspl,ylim=c(-22,22),xlim=c(6,50),col=rainbow(25),lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Simulated data")
#lines(PM10_fd_Bspl,col=rainbow(25),lwd=2,lty=1)
#title(ylab=expression(chi[s]^1),cex.lab=1.2,mgp=c(2.5,0,0))
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi)[s]^2),xpd = T,cex=2)

x11()
datosTT=ts(datosf11)
ts.plot(datosTT,col=rainbow(25),ylim=c(-20,20),xlim=c(6,45),type="l")

dato1=(f22[1]*v2(x)+f12[1]*v4(x)+f11[1]*v3(x))*rnorm(250)
dato2=(f22[2]*v2(x)+f11[2]*v3(x)+f12[2]*v4(x))*rnorm(250)
dato3=(f22[3]*v2(x)+f11[3]*v3(x)+f12[3]*v4(x))*rnorm(250)
dato4=(f22[4]*v2(x)+f11[4]*v3(x)+f12[4]*v4(x))*rnorm(250)
dato5=(f22[5]*v2(x)+f11[5]*v3(x)+f12[5]*v4(x))*rnorm(250)
dato6=(f22[6]*v2(x)+f11[6]*v3(x)+f12[6]*v4(x))*rnorm(250)
dato7=(f22[7]*v2(x)+f11[7]*v3(x)+f12[7]*v4(x))*rnorm(250)
dato8=(f22[8]*v2(x)+f11[8]*v3(x)+f12[8]*v4(x))*rnorm(250)
dato9=(f22[9]*v2(x)+f11[9]*v3(x)+f12[9]*v4(x))*rnorm(250)
dato10=(f22[10]*v2(x)+f11[10]*v3(x)+f12[10]*v4(x))*rnorm(250)
dato11=(f22[11]*v2(x)+f11[11]*v3(x)+f12[11]*v4(x))*rnorm(250)
dato12=(f22[12]*v2(x)+f11[12]*v3(x)+f12[12]*v4(x))*rnorm(250)
dato13=(f22[13]*v2(x)+f11[13]*v3(x)+f12[13]*v4(x))*rnorm(250)
dato14=(f22[14]*v2(x)+f11[14]*v3(x)+f12[14]*v4(x))*rnorm(250)
dato15=(f22[15]*v2(x)+f11[15]*v3(x)+f12[15]*v4(x))*rnorm(250)
dato16=(f22[16]*v2(x)+f11[16]*v3(x)+f12[16]*v4(x))*rnorm(250)
dato17=(f22[17]*v2(x)+f11[17]*v3(x)+f12[17]*v4(x))*rnorm(250)
dato18=(f22[18]*v2(x)+f11[18]*v3(x)+f12[18]*v4(x))*rnorm(250)
dato19=(f22[19]*v2(x)+f11[19]*v3(x)+f12[19]*v4(x))*rnorm(250)
dato20=(f22[20]*v2(x)+f11[20]*v3(x)+f12[20]*v4(x))*rnorm(250)
dato21=(f22[21]*v2(x)+f11[21]*v3(x)+f12[21]*v4(x))*rnorm(250)
dato22=(f22[22]*v2(x)+f11[22]*v3(x)+f12[22]*v4(x))*rnorm(250)
dato23=(f22[23]*v2(x)+f11[23]*v3(x)+f12[23]*v4(x))*rnorm(250)
dato24=(f22[24]*v2(x)+f11[24]*v3(x)+f12[24]*v4(x))*rnorm(250)
dato25=(f22[25]*v2(x)+f11[25]*v3(x)+f12[25]*v4(x))*rnorm(250)


datos=data.frame(dato1,dato2,dato3,dato4,dato5,dato6,dato7,dato8,dato9,dato10,
                    dato11,dato12,dato13,dato14,dato15,dato16,dato17,dato18,dato19,dato20,
                    dato21,dato22,dato23,dato24,dato25)
datosTT=tseries(datos)

Mdatos=as.matrix(datos,nrow=50,ncol=25)
#ID_estaciones=c("1","2","3","4","5","6","7","8","9")
datosf=fdata(Mdatos,argvals=1:nrow(Mdatos))
nbasis <-250
hourange <- c(1,nrow(Mdatos))
lambda=0.000001
harmaccelLfd <- vec2Lfd(c(1,10), hourange)
hourbasis_Bsplines <- create.bspline.basis(hourange,nbasis)
PM10_fdPar_Bspline<-fdPar(fdobj=hourbasis_Bsplines,Lfdobj=harmaccelLfd,lambda)
PM10_fd_Bspline <- smooth.basis(argvals=1:nrow(Mdatosf22),Mdatosf22,PM10_fdPar_Bspline)
PM10_fd_Bspl=PM10_fd_Bspline$fd
plot(PM10_fd_Bspl,ylim=c(-29,24),xlim=c(6,50),col=rainbow(25),lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Functional cokriging")
#lines(PM10_fd_Bspl,col=rainbow(25),lwd=2,lty=1)
#title(ylab=expression(chi[s]^1),cex.lab=1.2,mgp=c(2.5,0,0))
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi)[s]^1),xpd = T,cex=2)

x11()
plot(PM10_fd_Bspl_sim-PM10_fd_Bspl,ylim=c(-22,30),xlim=c(5,45),col=rainbow(25),lty=1,lwd=2,cex=1,ylab="",xlab="Time",cex.lab=1.2,cex.axis=1.2,main="Residual functions")
#lines(PM10_fd_Bspl,col=rainbow(25),lwd=2,lty=1)
#title(ylab=expression(chi[s]^1),cex.lab=1.2,mgp=c(2.5,0,0))
loc <- par("usr")
text(loc[1.5], loc[4.3], adj = c(2.5,4),expression(bold(chi)[s]^1),xpd = T,cex=2)




