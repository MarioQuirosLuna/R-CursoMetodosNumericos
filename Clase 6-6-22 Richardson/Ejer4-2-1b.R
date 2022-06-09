
# Ejemplo 4.2.1 pág. 181

f <- function(x) return( x*exp(x) )
x0 <- 2
h <- 0.2

fp <- function(x0, h) return( (f(x0+h)-f(x0-h))/(2*h) ) # fórmula 4.5

options(digits = 8)
n <- 3
N <- matrix(0,n,n)
for (i in 1:n) N[i, 1] <- fp(x0, h/2^(i-1))

Richardson <- function(N, n){
  for (i in 2:n) 
    for (j in 2:i) N[i, j] <- N[i, j-1] + (N[i, j-1]-N[i-1, j-1])/(4^(j-1)-1)
  return(N)
}
  
tabla <- Richardson(N, n)

#------------------------------------------------------

# Ejercicio 4.2.1b

f <- function(x) x + exp(x)
x0 <- 0
h <- 0.4

fp <- function(x0, h) return( (f(x0+h)-f(x0-h))/(2*h) )

options(digits = 8)
n <- 3
N <- matrix(0,n,n)
for (i in 1:n) N[i, 1] <- fp(x0, h/2^(i-1))

Richardson <- function(N, n){
  for (i in 2:n) 
    for (j in 2:i) N[i, j] <- N[i, j-1] + (N[i, j-1]-N[i-1, j-1])/(4^(j-1)-1)
  return(N)
}

tabla <- Richardson(N, n)

library(Deriv) # Nesesaria para hacer derivadas en R con la funci?n Deriv
fprima <- Deriv(f,"x")
fprima(x0)

#------------------------------------------------------

# Ejercicio 4.2.2b

f <- function(x) x + exp(x)
x0 <- 0
h <- 0.4

fp <- function(x0, h) return( (f(x0+h)-f(x0-h))/(2*h) )

options(digits = 8)
n <- 4
N <- matrix(0,n,n)
for (i in 1:n) N[i, 1] <- fp(x0, h/2^(i-1))

Richardson <- function(N, n){
  for (i in 2:n) 
    for (j in 2:i) N[i, j] <- N[i, j-1] + (N[i, j-1]-N[i-1, j-1])/(4^(j-1)-1)
  return(N)
}

tabla <- Richardson(N, n)

library(Deriv) # Nesesaria para hacer derivadas en R con la funci?n Deriv
fprima <- Deriv(f,"x")
fprima(x0)
