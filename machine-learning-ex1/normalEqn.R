normalEqn <- function(X, y) {
  library(MASS)
  theta <- rep(0,length(y))
  theta <- pinv(t(X) %*% X) %*% t(X) %*% y
  theta
}
