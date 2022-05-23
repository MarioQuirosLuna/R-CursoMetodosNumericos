
# Ejercicio 5 sección 3.1
N <- 5
puntos <- seq(-2, 2, length = N)
Q <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
Q[ , 1] <- 3^puntos

Neville <- function(x, Q){
  p <- as.numeric( row.names(Q) )
  for (i in 2:N)
    for (j in 2:i) Q[i,j] <- 
        ((x-p[i-j+1])*Q[i,j-1]-(x-p[i])*Q[i-1,j-1])/(p[i]-p[i-j+1])
  return(Q)
}

tablaE5 <- Neville(0.5, Q)

# Respuesta: 3^(1/2) = 1.732051



#-----------------------------------------

# Ejercicio 3.1.3b sección 3.1
N <- 4
puntos <- seq(-0.75, 0, length = N)
Q <- matrix(0, N, N, dimnames = list(puntos, rep(' ', N) ))
Q[ , 1] <- c(-0.0718125, -0.02475, 0.33499375, 1.101)

# haciendo el cálculo paso a paso para los primeros tres términos 
x <- -1/3
Q22 <- ((-1/3 - -0.75)*(-0.02475) - (-1/3 - -0.50)*(-0.0718125))/(-0.50 - -0.75)
Q32 <- ((-1/3 - -0.50)*0.33499375 - (-1/3 - -0.25)*(-0.02475))/(-0.25 - -0.50)
Q33 <- ((-1/3 - -0.75)*Q32 - (-1/3 - -0.25)*Q22)/(-0.25 - -0.75)

# ahora es aplicando el algoritmo

Neville <- function(x, Q){
  p <- as.numeric( row.names(Q) )
  for (i in 2:N)
    for (j in 2:i) Q[i,j] <- 
        ((x-p[i-j+1])*Q[i,j-1]-(x-p[i])*Q[i-1,j-1])/(p[i]-p[i-j+1])
  return(Q)
}

tabla3.1.3b <- Neville(-1/3, Q)



