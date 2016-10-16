computeCostMulti <- function(X, y, theta) {
  m <- length(y) 
  J <- 0
  dif <- X %*% theta - y
  J <- (t(dif) %*% dif) / (2 * m)
  J
  
}
