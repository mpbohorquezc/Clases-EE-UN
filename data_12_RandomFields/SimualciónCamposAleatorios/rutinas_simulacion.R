##Ejercicio 1. Graficar una distribución normal multivariada. Simular una muestra (n=3),
#a partir de la muestra estimar media y varianza usar el estimador de MCG

##Ejercicio 2. Graficar una distribución normal multivariada.Simular una muestra (n=300),
mu=5,use um modelo autoregresivo continuo
#a partir de la muestra estimar media y varianza usar el estimador de MCG

x=seq(1,5,by=1)
y=x
grilla_1=expand.grid(x,y)
distancias_1=as.matrix(dist(grilla_1))
simulacion_1=rmultnorm(1, mu=rep(24,25), vmat=cov.spatial(distancias_1,cov.model="exp",cov.pars=c(2,3.5)))
simu_1=cbind(grilla_1,t(simulacion_1))
#Simulacion cholesky normal multivariada
#mu+L*x
coordenadas=matrix(c(1,2,3,2,5,7),nrow=3,byrow=T)
distancias=as.matrix(dist(coordenadas))
sigma=exp(-(distancias/4)^2)
#sigma=matrix(c(1,0.2,0.3,0.2,4,5,0.3,5,9),nrow=3,byrow=T)
U=chol(sigma)    #upper tambien llamada la raiz de Cholesky de sigmas
L=t(U)           #lower
#sigma=LU
#x=matrix(c(rnorm(3),rnorm(3),rnorm(3)),byrow=T,nrow=3)
#x=matrix(rnorm(6),byrow=T,nrow=3) #2 realizaciones normal de tres VA
x=matrix(rnorm(48),byrow=T,nrow=3)  #16 realizaciones de 3 variables q distribuyen conjuntamente normal con media mu y varianza sigma ''
x=matrix(rnorm(3000),byrow=T,nrow=3)  #1000 realizaciones ''
mu=c(3,4,39)
z=mu+L%*%x
apply(z,1,mean)
apply(z,1,var)

#simulacion descomposision espectral normal multivariada

#sigma=P*delta*P'    si sigma es simetrica existen P y delta, del a diagonal y P'P=I
#P'*sigma*P=delta
#P'*delta^0.5*P=sigma^0.5

vecprop=eigen(sigma)
P=vecprop$vectors
delta=t(P)%*%sigma%*%P    #la diagonal de delta son los valores propios
raiz_sigma=t(P)%*%round(delta,2)^(1/2)%*%P  #me toca redondear por aparecen unos valores -1.55*10E-15 que es aprox 0 pero -
z=mu+L%*%raiz_sigma

#descomposicion en valor singular, es igual al espectrral solo q trabaja con los eigen de sigma'sigma porq es general para cualquier matriz nxm y los valores propios de sigma'sigma son el cuadrado de los de sigma
singular=svd(sigma)
s=diag(singular$d)
(singular$u)%*%s%*%t(singular$v)
#sigma=(singular$u)%*%s%*%(t(singular$v))
library(mvtnorm)
norm_m <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
??mvtnorm

#simulacion condicional
#secuencial gaussiana
library(gstat)
data(meuse)
meuse1=meuse[1:5,]  #tome los primeros para ver la salida y comparar
coordinates(meuse1) = ~x+y 
data(meuse.grid)
meuse.grid1=meuse.grid[1:5]
meuse.grid1=meuse.grid[1:5,]
coordinates(meuse.grid1)=~x + y
m=vgm(1,"Gau",150)
x <- krige(log(zinc)~1, meuse1, meuse.grid1, model = m)
x1 <- krige(log(zinc)~1, meuse1, meuse.grid1, model = m, block = c(40,40))
x3 <- krige(log(zinc)~1, meuse1, meuse.grid1, model = m,nsim=2)  #simulacion secuencial gaussiana
#si se usa el gstat para la simulacion gaussiana y se quiere hacer uno por uno, solo es simular el primer lugar,
 incluirlo en los datos observados y luego simular el segundo y asi...

#simular datos de un proceso con cov gaussiana para ver como son las ponderaciones
#grilla en F:/Patricia/Escritorio/revisar
potasio1=read.table("potasio.txt",head=T)
coorde=cbind(potasio1$x,potasio1$y) #solo las coordenadas
distancias=as.matrix(dist(coorde))  #matriz de distancias 
sigma=exp(-(distancias/8)^2)      #matriz de covarianzas    
U=chol(sigma)    #upper tambien llamada la raiz de Cholesky de sigmas
L=t(U)
x=matrix(rnorm(95),byrow=F,nrow=95)
z_sim=t(U)%*%x          #supongamos mu=0
potasio2=data.frame(potasio1$x,potasio1$y,z_sim)
potasio3=potasio2
pot2g=as.geodata(potasio2)
v1=variog(pot2g)
plot(v1)
v2=variofit(pot2g,ini=inic)
coordinates(potasio3)=~potasio1.x+potasio1.y
loci <- expand.grid(seq(5,10,len=5), seq(0,30,len=4))
coordinates(loci)=~Var1+Var2
v3=as.vgm.variomodel(v2)
krige(z_sim~1, potasio3, loci, model = v3)
kc <- krige.conv(pot2g,locations=loci,krige=modelo)

modelo<-krige.control(obj.model=v2)
