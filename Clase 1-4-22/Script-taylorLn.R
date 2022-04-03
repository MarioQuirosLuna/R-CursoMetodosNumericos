#Ejercicio Pag 23
#Construya un algoritmo para determinr el valor minimo de N requerido para 
# |ln 1.5 − PN (1.5)| < 10^(−5)

#Sin polinomio taylor

taylorLn <- function(x, TOL, M){
  N <- 1
  y <- x - 1
  sum <- 0
  potencia <- y
  term <- y
  sign <- -1
  while (N <= M) {
    sign <- -sign
    sum <- sum + sign * term
    potencia <- potencia * y
    term <- potencia/(N + 1)
    if(abs(term) < TOL){
      return(N)
    }
    N <- N + 1
  }
  return('El metodo fallo')
}

taylorLn(1.5, 10^(-5), 15)

#Con polinomio taylor

poltaylor <- function(N,x){
  sum <- 0
  for (i in 1:N) {
    sum <- sum + (-1)^(i + 1) * (x - 1)^(i) / i
  }
  return(sum)
}

poltaylor(12, 1.5)

