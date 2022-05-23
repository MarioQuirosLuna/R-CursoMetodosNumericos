
# Ejemplo 1 sección 3.2
N <- 5
puntos <- seq(1, 2.2, length = N)
F <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
F[,1] <- c(0.7651977, 0.6200860, 0.4554022, 0.2818186, 0.1103623)

DifDivididas <- function(x, F){
  p <- as.numeric( row.names(F) )
  for (i in 2:N)
    for (j in 2:i) F[i,j] <- (F[i,j-1]-F[i-1,j-1])/(p[i]-p[i-j+1])
  return(F)
}

tabla311 <- DifDivididas(1.5, F)

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
