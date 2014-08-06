mind <- function(d){
  n <- nrow(d)
  dd <- cbind(d, 1:n)
  wmins <- apply(dd[-n,], 1, imin)
  i <- which.min(wmins[1,])
  j <- wmins[2,i]
  return(c(d[i,j], i, j))
}

imin <- function(x){
  n <- length(x)
  i <- x[n]
  j <- which.min(x[(i+1):(n-1)])
  return(c(j,x[j]))
}