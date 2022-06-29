## Ejemplo 1 (2 , -1) : Resultado => Valores Propios L=3, L=1 
##           (-1 , 2)             => Vectores Propios V1(-1,1), V2(1,1)

## Ejemplo 2 (0 , 0.5): Resultado => Valores Propios L=0.5, L=-0.5 
##           (0.5 , 0)            => Vectores Propios V1(1,1), V2(1,-1)

## Ejemplo 3 (3 , 4)  : Resultado => Valores Propios L=4, L=-1 
##           (1 , 0)              => Vectores Propios V1(4,1), V2(1,-1)

n <- 2
m <- 2

matriz <- matrix(data=0, n,  m)

data.entry(matriz) 

eigen(matriz, symmetric = isSymmetric(matriz))


0.2425356 * 4


4 * 0.1767767
-4 * 0.1767767

