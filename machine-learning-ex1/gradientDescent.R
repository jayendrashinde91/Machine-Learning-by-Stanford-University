gradientDescent <- function(X, y, theta, alpha, num_iters) {
  m <- length(y) 
  J_history = rep(0,num_iters + 1)
  theta_history = matrix(0,num_iters + 1,length(theta))
  
  theta_history[1,] = t(theta)
  J_history[1] = computeCost(X, y, theta)
  
  for (iter in 2:(num_iters + 1)) {
    theta_prev = theta
    
     
    for (j in 1:dim(X)[2]) {
      deriv = (t(X %*% theta_prev - y) %*% X[, j]) / m
      
    
      theta[j] = theta_prev[j] - (alpha * deriv)
    }
    
    J_history[iter] = computeCost(X, y, theta)
    theta_history[iter,] = t(theta)
  }
  
  list(theta = theta, J_history = J_history, theta_history = theta_history)
}
