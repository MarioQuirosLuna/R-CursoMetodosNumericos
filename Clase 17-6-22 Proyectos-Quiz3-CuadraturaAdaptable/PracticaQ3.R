
f <- function(x) x^2*log(x^2+1)

a <- 0
b <- 2
paste('El valor exacto es',integrate(f,a, b)$value)

3 "El valor exacto es 3.1092908435171"

h <- 0.25
n <- 8

# a) Compuesta de Trapecio

suma <- 0
for (i in 1:(n-1)) suma <- suma + f(a+i*h)

trapecio <- (h/2)*(f(a)+2*suma+f(2))

# Respuesta trapecio = 3.159476

# b) compuesta de Simpson

sumPares <- 0
for (j in 1:((n/2)-1)) sumPares <- sumPares+f(a+2*j*h)

sumImpares <- 0
for (j in 1:(n/2))  sumImpares <- sumImpares+f(a+(2*j-1)*h)
  
simpson <- (h/3)*(f(0)+2*sumPares+4*sumImpares+f(2))

# c) compuesta punto medio

sumPuntoMedio <- 0
for (j in 0:(n/2)) sumPuntoMedio <- sumPuntoMedio+f(a+(2*j-1)*h)

vec <- rep(0,4)
for (j in 0:(n/2)) vec[j] <- a+(2*j-1)*h
vec

puntoMedio <- 2*h*sumPuntoMedio
