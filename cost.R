#
#   Defines the cost function c(x;theta) for Rust (1997).
#

cost <- function(x,theta){
  result <- theta[1] * x + theta[2] * x^2
  return(result) 
}