learnModel <- function(data, labels){
  # Learn Logit Regression model
  lambda <- 0.1
  X <- data
  y <- labels
  
  m <- nrow(X)
  n <- ncol(X)
  
  num_labels <- 10
  
  all_theta <- matrix(data = 0, nrow = num_labels, ncol = n+1, byrow = T)

  X <- cbind(1, X)
  
  initial_theta <- 0*1:(n+1)
  print("Start learning loop for each digit")
  for (i in 1:num_labels){
    temp <- i-1
    print(i)
    buff <- optim(initial_theta, lrCostFunction, gr = grlrCostFunction, X=X, y=(y==temp), 
                  lambda=lambda, method = "CG")
    all_theta[i,] <- buff$par
  }
  
  
  return(all_theta)
  
}