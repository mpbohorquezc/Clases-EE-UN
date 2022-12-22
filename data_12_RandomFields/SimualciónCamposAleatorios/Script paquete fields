library(fields)

#Imagen referenciada en un plano cartesiano
plot(0:1.5, 0:1.5, type="n")
data(lennon)
add.image(0.5,0.5,lennon, col=grey((0:256)/256),image.width=1,image.heigth=1)
#add.image(0.5,0.5,lennon, col=tim.colors(256),image.width=1,image.heigth=1)
# reference lines
xline(0.5, col=3)
yline(0.5,col=4)

#Flechas
x<- runif( 20)
y<- runif( 20)
u<- rnorm( 20)
v<- rnorm( 20)
plot( x,y)
arrow.plot(x,y,u,v) # a default that is unattractive
x11()
plot( x,y, type="n")
arrow.plot( x,y,u,v, arrow.ex=seq(0.1,0.3,len=20),  col=rainbow(20), lwd=2)
x11()
plot( x,y, type="n")
arrow.plot( x,y,u,v, arrow.ex=seq(0.1,0.3,len=20),length=0.1, col=tim.colors(20), lwd=3)

#crear una imagen a partir de una grilla irregular
#no me gusta que no se puede dejar muy fina porque lo que hace es volver muy pequeño el pixel
#así que si se pone nx y ny muy alto el punto se vuelve tan diminuto que es invisible
look2<- as.image( RMprecip$y, x= RMprecip$x,grid=gridList)
image.plot( look2)
look<- as.image(RMprecip$y, x= RMprecip$x, nx=500, ny=500)
image.plot(look)
# reduced grid extent compared to the domain
gridList<- list(x = seq(-125,-110,length.out=100),
y = seq( 38, 42,length.out=10) )
look2<- as.image( RMprecip$y, x= RMprecip$x,grid=gridList)
image.plot( look2)
# number of obs in each cell -- in this case equal to the
# aggregated weights because each obs had equal weight in the call
image.plot( look$x ,look$y, look$weights, col=terrain.colors(50))
# hot spot is around Denver

#ejemplo suponiendo R2 y una variable continua
datos=cbind(x=rnorm(50),y=rnorm(50),z=rnorm(50))
datosIm<- as.image(datos[,3], x=datos[,1:2])
image.plot(datosIm)
m
