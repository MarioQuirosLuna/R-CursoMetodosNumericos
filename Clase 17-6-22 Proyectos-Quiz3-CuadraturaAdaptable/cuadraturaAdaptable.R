
# Cuadratura adaptable

# ejemplo p??gina 165

f <- function(x) sin(x) 

a <- 0
b <- pi/2
pm <- (a+b)/2

# ecuaci??n (4.35) p??gina 163
S <- function(a,b) {
  h <- (b-a)/2
  return( (h/3)*(f(a)+4*f(a+h)+f(b)) )
}

error <- abs(S(a, b)-S(a, pm)-S(pm, b))/15

# Exacto

exacto <- integrate(f, a, b)$value

errorReal <- abs( exacto-(S(a, pm)+S(pm, b)) )

#---------------------------------------------------

# ejercicio 4.6.1b

f <- function(x) x^2*exp(-x)

a <- 0
b <- 1
pm <- (a+b)/2

# gr??fica
x <- seq(a, b, length=100)
plot(x, f(x), type="l", col="orange", lwd=3, 
     main="f(x)", xlab="x", ylab="f(x)",
     las=1, col.axis="blue")
ejeX <- function(x) return(0*x)
lines(x, ejeX(x), col="black", lwd=3)

S <- function(a,b) { # ecuaci??n (4.35) p??gina 163
  h <- (b-a)/2
  return( (h/3)*(f(a)+4*f(a+h)+f(b)) )
}

S(a, b)
S(a, pm)
S(pm, b)

error <- abs(S(a, b)-S(a, pm)-S(pm, b))/15

# Exacto

exacto <- integrate(f, a, b)$value

errorReal <- abs( exacto-(S(a, pm)+S(pm, b)) )

# Ejercicio 4.6.3b

# dividir el segundo subintervalo

pm2 <- (pm+b)/2

error2 <- abs( S(a, b)-S(a, pm)-S(pm, pm2)-S(pm2, b) )/15

# Exacto

exacto <- integrate(f, a, b)$value

options(digits= 20)

errorReal2 <- abs( exacto-S(a, pm)-S(pm, pm2)-S(pm2, b) )

#---------------------------------------------

# ejercicio 4.6.1b

f <- function(x) 2/(x^2-4)

a <- 0
b <- 0.35
pm <- (a+b)/2

# gr??fica
x <- seq(a, b, length=100)
plot(x, f(x), type="l", col="orange", lwd=3, 
     main="f(x)", xlab="x", ylab="f(x)",
     las=1, col.axis="blue")
ejeX <- function(x) return(0*x)
lines(x, ejeX(x), col="black", lwd=3)

S <- function(a,b) { # ecuaci??n (4.35) p??gina 163
  h <- (b-a)/2
  return( (h/3)*(f(a)+4*f(a+h)+f(b)) )
}

S(a, b)
S(a, pm)
S(pm, b)

error <- abs(S(a, b)-S(a, pm)-S(pm, b))/15


# Exacto

exacto <- integrate(f, a, b)$value

errorReal <- abs( exacto-(S(a, pm)+S(pm, b)) )

#------------------------------------------------------

# Ejercicio 4.7.1b

f471B <- function(t) (t+1)^2*exp((t+1)/2) 

(1/8)*( f471B(-0.573502)+f471B(0.573502) )

# Exacto

foriginal <- function(x) x^2*exp(x)

integrate(foriginal, 0, 1)

# Ejercicio 4.7.3b con n=3

(1/8)*( 0.55555*f471B(-0.774569)+0.88888*f471B(0)+0.55555*f471B(0.774569) )

