
# ejemplo pág. 22

x <- c(7,5,2,6,3,1,4)

sumVector <- function(x) {
 
  N <- length(x)
  
  # Paso 1
  sum <- 0
  
  # Paso 2
  for (i in 1:N) {
    sum <- sum + x[i]
  }
  
  # Paso 3
  return(sum)
}

sumVector(x)

sum(x)


# ejemplo pág 23

taylorLn <- function(x, tol, M) {
  # Paso 1
  N <- 1
  y <- x-1
  sum <- 0
  potencia <- y
  term <- y
  sig <- -1
  
  # Paso 2
  while (N <= M) {
    # Paso 3
    sig <- -sig
    sum <- sum + sig * term
    potencia <- potencia * y
    term <- potencia/(N+1)
     
    # Paso 4
    if (abs(term < tol)) {
      return(N)
    }
    
    # Paso 5
    N <- N+1
  }
  return('El método falló')
}

taylorLn(1.5, 10^(-7), 25)

polTaylor <- function(N, x) {
  sum <- 0
  for (i in 1:N) {
    sum <- sum + (-1)^(i+1) * (x-1)^(i) / i
  }
  return(sum)
}

polTaylor(19, 1.5)
