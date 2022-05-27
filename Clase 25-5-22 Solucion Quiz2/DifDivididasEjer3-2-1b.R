
# Ejercicio 1b sección 3.2
N <- 4
puntos <- c(0.6, 0.7, 0.8, 1)
F <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
F[,1] <- c(-0.17694460, 0.01375227, 0.22363362, 0.65809197)

DifDivididas <- function(x, F){
  p <- as.numeric( row.names(F) )
  for (i in 2:N)
    for (j in 2:i) F[i,j] <- (F[i,j-1]-F[i-1,j-1])/(p[i]-p[i-j+1])
  return(F)
}

tabla1b <- DifDivididas(0.9, F)

#--------------------------------------------------

# Ejercicio 1.a sección 3.2
N <- 4
puntos <- c(8.1, 8.3, 8.6, 8.7)
F <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
F[,1] <- c(16.94410, 17.56492, 18.50515, 18.82091)

DifDivididas <- function(x, F){
  p <- as.numeric( row.names(F) )
  for (i in 2:N)
    for (j in 2:i) F[i,j] <- (F[i,j-1]-F[i-1,j-1])/(p[i]-p[i-j+1])
  return(F)
}

tabla311 <- DifDivididas(8.4, F)
F <- tabla311

P3 <- function(x, F, p){
  return( F[1,1] + F[2,2]*(x-p[1]) + F[3,3]*(x-p[1])*(x-p[2]) + 
           F[4,4]*(x-p[1])*(x-p[2])*(x-p[3]) )
}

P3(8.4, F, puntos)

#-----------------------------------

# Ejercicio 3a sección 3.2
N <- 4
puntos <- seq(-0.75, 0, length = N)
F <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
F[,1] <- c(-0.0781250, -0.02475, 0.3349375, 1.101)

DifDivididas <- function(x, F){
  p <- as.numeric( row.names(F) )
  for (i in 2:N)
    for (j in 2:i) F[i,j] <- (F[i,j-1]-F[i-1,j-1])/(p[i]-p[i-j+1])
  return(F)
}

tabla3a <- DifDivididas(-1/3, F)


x <- -1/3
h <- puntos[2]-puntos[1]
s <- -(puntos[N]-x)/h
aprox <- tabla3a[N,1] + 
  s*h*tabla3a[N,2] +
  s*(s+1)*h^2*tabla3a[N,3] +
  s*(s+1)*(s+2)*h^3*tabla3a[N,4]
