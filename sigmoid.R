sigmoid <- function(z){
  # Calculate Sigmoid Function 
  rez <- 1/(1+exp(-z))
  return(rez)
}