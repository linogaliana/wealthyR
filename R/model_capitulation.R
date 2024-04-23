


#' A wrapper to use capitulation package within GMM framework
#' @param theta A named vector of structural parameters
#' @param ... Additional arguments provided

model_capitulation <- function(theta = c('r' = 0.03, 'gamma' = 0.5, 'beta' = 0.92),
                               ...){
  
  
  args <- list(...)
  
  
  # COMPUTE MOMENT GIVEN THETA PARAMETER ----------------------------
  
  if (!is.null(args[['df_moment_simulated']]) && !is.null(args[['df_moment_theory']])){

    message("Using moments provided")
    
    df_moment <- data.table::data.table('epsilon' = args[['df_moment_simulated']]-args[['df_moment_theory']])

  } else{
    
    df_moment <- capitulation_wrapper(theta, ...)
    
  }
  
  return(df_moment)
  
}


