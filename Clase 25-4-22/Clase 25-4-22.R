
# Graficar cos(x)

a <- -pi
b <- pi
x=seq(a,b,length=100)
f <- function(x) cos(x)

plot(x,f(x),type="l",col="blue",lwd=3,main="cos(x)",xlab="x",ylab="f(x)",
     las=1,col.axis="red")

# método de Delta cuadrado de Aitken

options(digits = 5)

Delta2Aitken <- function(N){
  p <- matrix(0,N)
  for (n in 1:N) p[n] <- f(1/n)
  pt <- matrix(0,N)
  for (n in (1:(N-2)) ) pt[n] <- p[n] - (p[n+1]-p[n])^2/(p[n+2]-2*p[n+1]+p[n])
  tabla <- matrix(0, nrow = 7, ncol = 3, 
                  dimnames = list( c(1:N), c("n","p_n","pt_n") ) )
  tabla[ , 1] <- 1:N
  tabla[ , 2] <- p
  tabla[ , 3] <- pt
  return(tabla)
}

tabla210 <- Delta2Aitken(7)

# Steffensen

options(digits = 10)

g <- function(x) sqrt( 10/(x+4) )

Steffensen <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p1 <- g(p0)
    p2 <- g(p1)
    p <- p0 - (p1-p0)^2/(p2-2*p1+p0)
    if ( abs(p-p0) < tol) return(p)
    i <- i+1
    p0 <- p
  }
  return(paste('El método falló despues de',i-1,'iteraciones'))
}

Steffensen(1.5,10^(-9),2)

# ejercicios

# 2.5.1(a)


a <- -1
b <- 1
x=seq(a,b,length=100)
f <- function(x) (2-exp(x)+x^2)/3

plot(x,f(x),type="l",col="blue",lwd=3,main="f(x)",xlab="x",ylab="f(x)",
     las=1,col.axis="red")
lines(x,x,col="green",lwd=3)
# método de Delta cuadrado de Aitken

options(digits = 5)

Delta2Aitken <- function(p0,N){
  p <- matrix(0,N)
  p[1] <- p0
  for (n in 2:N) p[n] <- f(p[n-1])
  pt <- matrix(0,N)
  for (n in (1:(N-2)) ) pt[n] <- p[n] - (p[n+1]-p[n])^2/(p[n+2]-2*p[n+1]+p[n])
  tabla <- matrix(0, nrow = N, ncol = 3, 
                  dimnames = list( c(1:N), c("n","p_n","pt_n") ) )
  tabla[ , 1] <- 0:(N-1)
  tabla[ , 2] <- p
  tabla[ , 3] <- pt
  return(tabla)
}

tablaEj1a <- Delta2Aitken(0.5, 5)

# Steffensen

options(digits = 10)

g <- function(x) sqrt( 10/(x+4) )

Steffensen <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p1 <- g(p0)
    p2 <- g(p1)
    p <- p0 - (p1-p0)^2/(p2-2*p1+p0)
    if ( abs(p-p0) < tol) return(p)
    i <- i+1
    p0 <- p
  }
  return(paste('El método falló despues de',i-1,'iteraciones'))
}

Steffensen(1.5,10^(-9),2)
