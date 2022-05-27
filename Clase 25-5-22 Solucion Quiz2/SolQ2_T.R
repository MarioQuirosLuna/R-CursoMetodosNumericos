
# Solución del Examen corto No. 1

# ejercicio 1

install.packages("Deriv")
library(Deriv) # Nesesaria para hacer derivadas en R con la función Deriv

options(digits = 10)

f <- function(x) x^3+4*x^2+4*x # x*(x+2)^2

a <- -3
b <- 1
x <- seq(a, b, length=100)
plot(x, f(x), type="l", col="blue", lwd=3, 
     main="f(x)", xlab="x", ylab="f(x)",
     las=1, col.axis="red")
ejeX <- function(x) return(0*x)
lines(x, ejeX(x), col="black", lwd=3)

f(0)
f(-2)

fprima <- Deriv(f,"x")
fprima

Newton <- function(p0, tol, No){
  tabla <- matrix(0, No, 4, dimnames=
                    list(1:No,c('pn-1','f(pn-1)','fprima(pn-1)','pn')))
  i <- 1
  while (i <= No) {
    p <- p0-(f(p0)/fprima(p0))
    tabla[i,] <- c(p0,f(p0),fprima(p0),p)  
    if ( abs(p-p0) < tol) return(list(p=p,tabla=tabla))
    i <- i + 1
    p0 <- p
  }
  return(paste('El método falló después de',i-1,'iteraciones.'))
}

salida <- Newton(-3, 10^(-2), 10)
salida$p
tabla2.3.2 <- salida$tabla


f2prima <- Deriv(fprima,"x")

NewtonMejorado211 <- function(p0, tol, No){
  tabla <- matrix(0, No, 5, dimnames=
                    list(1:No,c('pn-1','f(pn-1)','fprima(pn-1)','f2prima(pn-1)','pn')))
  i <- 1
  while (i <= No) {
    p <- p0 - ( f(p0)*fprima(p0) / ( (fprima(p0))^2-f(p0)*f2prima(p0) ) )
    tabla[i,] <- c(p0,f(p0),fprima(p0),f2prima(p0),p)  
    if ( abs(p-p0) < tol) return(list(p=p,tabla=tabla))
    i <- i + 1
    p0 <- p
  }
  return(paste('El método falló después de',i-1,'iteraciones.'))
}

salida <- NewtonMejorado211(-3, 10^(-2), 10)
salida$p
tablaNewMejorado <- salida$tabla

# Respuesta: p = -2, y si mejora en tres iteraciones

#-------------------------------------------------------

# Ejercicio 2 

options(digits = 10)

g <- function(x) 2^{-x}

a <- 0
b <- 1
x <- seq(a, b, length=100)
plot(x, g(x), type="l", col="blue", lwd=3, 
     main="g(x)", xlab="x", ylab="g(x)",
     las=1, col.axis="red")
yx <- function(x) return(x)
lines(x, yx(x), col="green", lwd=3)

Steffensen <- function(p0, tol, No){
  tabla <- matrix(0, No, 3, 
                  dimnames = list(0:(No-1), c('p_0^k','p_1^k','p_2^k')) )
  i <- 1
  while (i <= No) {
    p1 <- g(p0)
    p2 <- g(p1)
    tabla[i, ] <- c(p0, p1, p2)
    p <- p0 - (p1-p0)^2/(p2-2*p1+p0)
    if ( abs(p-p0) < tol ) return(list(p=p, tabla=tabla))
    i <- i + 1
    p0 <- p
  }
  return(paste('El método falló en', i-1,'iteraciones'))
}

salida <- Steffensen(0.5, 10^(-2), 4)
p <- salida$p
tabla2.5.3 <- salida$tabla

p
g(p)

# Respuesta p = 0.6411857921 y g(p) = 0.6411857234

#-------------------------------------------------------

f <- function(x) x^3-2*x+4

a <- -3
b <- 3
x <- seq(a, b, length=100)
plot(x, f(x), type="l", col="blue", lwd=3, 
     main="f(x)", xlab="x", ylab="f(x)",
     las=1, col.axis="red")
ejeX <- function(x) return(0*x)
lines(x, ejeX(x), col="black", lwd=3)

f(-2)

fprima <- Deriv(f,"x")
fprima

Newton <- function(p0, tol, No){
  tabla <- matrix(0, No, 4, dimnames=
                    list(1:No,c('pn-1','f(pn-1)','fprima(pn-1)','pn')))
  i <- 1
  while (i <= No) {
    p <- p0-(f(p0)/fprima(p0))
    tabla[i,] <- c(p0,f(p0),fprima(p0),p)  
    if ( abs(p-p0) < tol) return(list(p=p,tabla=tabla))
    i <- i + 1
    p0 <- p
  }
  return(paste('El método falló después de',i-1,'iteraciones.'))
}

salida <- Newton(-2.5, 10^(-2), 20)
salida$p
tablaE3 <- salida$tabla

# Respuesta: p = -2

#     1  0 -2  4
# -2    -2  4 -4
#     1 -2  2  0

# Resolver x^2-2*x+2 = 0

polyroot(c(2,-2,1))

# Respuesta: 1+1i 1-1i