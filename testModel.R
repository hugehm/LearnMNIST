testModel <- function(all_theta, X){
  # Calculate predictions
  X <- cbind(1, X)
  p <- max.col(sigmoid(X%*%t(all_theta)))
  p <- p-1
  
  return(p)
}