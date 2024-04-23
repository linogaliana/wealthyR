
#' Import price index used in \code{Destinie} model
#' 
#' @param path_destinie Path to Destinie data
#' 
#' @export

import_price <- function(path_destinie = "./inst/dataINSEE/Destinie"){
  macro<-NULL #Avoid CRAN check notes
  
  load(paste0(path_destinie,
              '/macro.Rda'))  
  return(macro)
}

