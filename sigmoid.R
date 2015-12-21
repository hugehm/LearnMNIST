sigmoid <- function(z){
  
#   print("SIGMOID z")
#   print(sum(z^2))
  # Calculate Sigmoid Function 
  res <- 1/(1+exp(-z))
  # res <- res*0.99999999999999+0.0000000000000000001
  return(res)
}