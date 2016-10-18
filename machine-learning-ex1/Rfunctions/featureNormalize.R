featureNormalize <- function(X) {
  X_norm <- X
  mu <- rep(0,dim(X)[2])
  sigma <- rep(0,dim(X)[2])
  
 
  for (p in 1:dim(X)[2]) {
    mu[p] <- mean(X[,p])
  }
  
  for (p in 1:dim(X)[2]) {
    sigma[p] <- sd(X[,p])
  }
  
  for (p in 1:dim(X)[2]) {
    if (sigma[p] != 0)
      for (i in 1:dim(X)[1])
        X_norm[i, p] <- (X[i, p] - mu[p]) / sigma[p]
      else
        
        X_norm[, p] <- t(rep(0,dim(X)[1]))
  }
  
  list(X_norm = X_norm, mu = mu, sigma = sigma)
  
}
