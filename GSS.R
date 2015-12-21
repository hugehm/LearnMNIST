GSS <- function(theta, X, y, lambda, grF){
  #Find optimum of one-dimensional function
  #Realisation of Golden section search (https://en.wikipedia.org/wiki/Golden_section_search)
  a <- 0
  b <- 1
  phi <- (1+sqrt(5))/2
  eps <- 0.0000000000001
  
  while(TRUE){
    x1 <- b-(b-a)/phi
    x2 <- a+(b-a)/phi
    
    y1 <- lrCostFunction((theta-x1*grF), X, y, lambda) 
    y2 <- lrCostFunction((theta-x2*grF), X, y, lambda)
    
    if(y1>=y2){
      a <- x1
    } else{
      b <- x2
    }
    
    if(abs(b-a)<eps){
      x <- (a+b)/2
      break
    }
  }

  return(x)
}
  