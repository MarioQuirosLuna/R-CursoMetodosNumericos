x <- c(7, 5, 2, 6, 3, 1, 4)

sumVector <- function(x){
  N <- length(x)
  sum <- 0
  for (i in 1:N) {
    sum <- sum + x[i]
  }
  return(sum)
}

sumVector(x)