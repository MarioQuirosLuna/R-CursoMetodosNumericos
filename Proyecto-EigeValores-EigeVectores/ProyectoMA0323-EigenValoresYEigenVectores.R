n <- 3
m <- 3

matriz <- matrix(data=0, n,  m)

data.entry(matriz) 

eigen(matriz, symmetric = isSymmetric(matriz))

