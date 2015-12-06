grlrCostFunction <- function(theta, X, y, lambda){
  # Calculate Cost Function Gradient
  m<-length(y);
  res <- 1/m * colSums((sigmoid(X%*%theta)-y)%*%rep(1,length(theta))*X)
  res[2:length(res)] <- res[2:length(res)]+lambda/m*theta[2:length(theta)]

  return(res)
}