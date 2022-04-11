
f <- function(x){
  return((x+4)*x^2-10)
}

biseccion <- function(a, b, tol, No){
  i <- 1
  fa <- f(a)
  while (i <= No) {
    p <- (a+b)/2
    fp <- f(p)
    if ( (fp == 0) || ((b-a)/2 < tol) ) return(p)
    i <- i+1
    if (fa*fp > 0){
      a <- p
      fa <- fp
    } else b <- p
  }
  return('El método fracasó')
}

# Ejemplo 1  f(x) = 0, [1, 2], 10^(-4)

options(digits = 15)
biseccion(1, 2, 10^(-12), 50)

# implementamos la (d)
g <- function(x){
  return( (10/(4+x))^(1/2) )
  return( sqrt(10/(4+x)) )
}

puntoFijo <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p <- g(p0)
    if ( abs(p-p0) < tol ) return(p)
    i <- i+1
    p0 <- p
  }
  return('El método falló')
}

# Ejemplo 1  g4(x)=x, [1, 2], 10^(-4), p0 =1.5
puntoFijo(1.5, 10^(-9), 15)

fnewton <- function(x){
  return(cos(x)-x)
}

dfnewton <- function(x){
  return(-sin(x)-1)
}

Newton <- function(p0, tol, No){
  i <- 1
  while (i <= No) {
    p <- p0 - fnewton(p0)/dfnewton(p0)
    if ( abs(p-p0) < tol ) return(p)
    i <- i+1
    p0 <- p
  }
  return('El método falló')
}

# Ejemplo 1  cos(x)-x=0, 10^(-4), p0 = pi/4
Newton(pi/4, 10^(-4), 15)

posicionFalsa <- function(p0, p1, tol, No){
  i <- 2
  q0 <- fnewton(p0)
  q1 <- fnewton(p1)
  while (i <= No) {
    p <- p1 - q1*(p1-p0)/(q1-q0)
    if ( abs(p-p1) < tol ) return(list(iter=i, p=p) )
    i <- i+1
    q <- fnewton(p)
    if (q*q1 < 0){
      p0 <- p1
      q0 <- q1
    }
    p1 <- p
    q1 <- q
  }
  return('El método falló')
}

# Ejemplo 3  cos(x)-x=0, 10^(-4), p0=0.5, p1=pi/4
resultado <- posicionFalsa(1.5, pi/4, 10^(-7), 15)
resultado$iter
resultado$p

