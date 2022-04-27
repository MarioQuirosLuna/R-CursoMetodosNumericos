
install.packages("Deriv")
library(Deriv) # Nesesaria para hacer derivadas en R con la funci�n Deriv


f <- function(x) (x-3)^2*(x^2+1) 
f(3)
fprima <- Deriv(f,"x")
fprima

Newton <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p <- p0-(f(p0)/fprima(p0))
    if ( abs(p-p0) < tol) return(p)
    i <- i + 1
    p0 <- p
  }
  return('El método falló')
}

options(digits = 10)
Newton(2, 10^(-4), 12)

# Newton mejorado

f <- function(x) (x-3)^2*(x^2+1) 
f(3)
fprima <- Deriv(f,"x")
fprima
fsegunda <- Deriv(fprima,"x")
fsegunda

NewtonMejorado <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p <- p0 - (f(p0)*fprima(p0)) / ( (fprima(p0))^2 - f(p0)*fsegunda(p0) )
    if ( abs(p-p0) < tol) return(p)
    i <- i + 1
    p0 <- p
  }
  return('El método falló')
}

options(digits = 10)
NewtonMejorado(2, 10^(-4), 5)
