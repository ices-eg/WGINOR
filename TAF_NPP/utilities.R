# utility functions for TAF_NPP -------------------------------------------------------
sigm <- function(x,k,x0,xmax){
  y <- (xmax/(1+exp(-k*(x-x0))))
  return(y)
}

dsigm <- function(x,k,x0,xmax){
  y <- xmax*sigm(x,k,x0,1)*(1-sigm(x,k,x0,1))
}
