## Ejemplo 1 (3 , 4)
##           (1 , 0)              

## Ejemplo 2 (2, 1, 0)
##           (1, 2, 0)
##           (0, 0, 3)

## Ejemplo 3 (2, 1, 1)
##           (2, 3, 2)
##           (1, 1, 2)

filas <- 3
columnas <- 3

print(paste("En una matriz ",filas,"x",columnas))

matriz <- matrix(data=0, filas,  columnas)

data.entry(matriz) 

print("Con valores")
print(matriz)

valoresPropios <- eigen(matriz, symmetric = isSymmetric(matriz))$values
vectoresPropios <- eigen(matriz, symmetric = isSymmetric(matriz))$vectors

print("Tiene como valores propios: ")
valoresPropios
print("Y sus vectores propios son: ")
vectoresPropios


## Verificar con datos de R
## Matriz * Vector1 = ValorPropio * vector1

print("Esto es correcto si:  Matriz * Vector1 = ValorPropio * vector1")

print("Usando el vector ")
vectoresPropios[,1]
print("Y el valor ")
valoresPropios[1]


matriz_Por_Vector <- matriz%*%vectoresPropios[,1]
valor_Por_Vector <- valoresPropios[1]*vectoresPropios[,1]

matriz_Por_Vector
valor_Por_Vector



## Verificar con datos propios
## Matriz * Vector1 = ValorPropio * vector1

vec1 <- matrix(data=0, filas)
data.entry(vec1)

matriz%*%vec1
valoresPropios[1]*vec1
