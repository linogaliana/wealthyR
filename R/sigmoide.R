sigmoide <- function(x){
  return(1/(1+exp(-x)))
}
logit <- function(x){
  return(log(x/(1-x)))
}