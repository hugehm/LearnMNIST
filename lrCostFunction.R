lrCostFunction <- function(theta, X, y, lambda){
  # Calculate Cost Funcrtion
  m<-length(y);
  # Error term
  r1 <- 1/m 
  r2 <- sum(-y*log(sigmoid(X%*%theta))-(1-y)*log(1-sigmoid(X%*%theta)))
  # Regularization term
  r3 <- lambda/(2*m)
  r4 <- sum(sum(theta[2:length(theta)]^2))
  res <- r1*r2+r3*r4
  return(res)
}