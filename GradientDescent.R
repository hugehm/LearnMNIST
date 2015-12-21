GradientDescent <- function(theta, X, y, lambda){
  # Find optimum of the function
  # Realisation of speedest gradient descent
  eps <- 0.0000001 

  while(TRUE){
    buff <- theta

    grF <- grlrCostFunction(buff, X, y, lambda)
    alpha <- GSS(buff, X, y, lambda, grF)
    
    theta <- buff - alpha*grF
    
    if(sum((buff-theta)^2)<eps){
      break
    }
  }
  return(theta)
}