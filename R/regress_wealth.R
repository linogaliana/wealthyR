#' First OLS model
#' 
#' @param data Estimation dataset
#' 

regress_wealth <- function(data){
  
  formula <- "log_K0 ~ factor(scol) + AGE + age2 + factor(ETAMATRI) + factor(ind_rminterc) + log_rminterc + rminterc2 + factor(SEXE)"
  
}