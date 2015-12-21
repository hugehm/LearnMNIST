lrCostFunction <- function(theta, X, y, lambda){
  # Calculate Cost Funcrtion
  m<-length(y);
  # Error term
  temp <- sigmoid(X%*%theta)
  r1 <- 1/m
  r2 <- sum(-y*log(temp*0.99999999999+0.0000000000000000001)-(1-y)*log(1-temp*0.99999999999+0.0000000000000000001))
  
#   print("r1")
#   print(r1)
#   print("r2")
#   print(r2)
  
  # Regularization term
  r3 <- lambda/(2*m)
  r4 <- sum(sum(theta[2:length(theta)]^2))
  
#   print("r3")
#   print(r3)
#   print("r4")
#   print(r4)
  
  res <- r1*r2+r3*r4
  
#   print("res in lrCostFunction")
#   print(res)
  
  return(res)
}