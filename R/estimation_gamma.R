#' 
#' estimation_gamma <- function(gamma_0, r = 0.016,
#'                              beta = 1,
#'                              data_microsimulated,
#'                              EP_2015,
#'                              EP_lon, 
#'                              normalize = FALSE,
#'                              scale = 'level',
#'                              exclude_negative = FALSE){
#'   
#'   NM_gamma <- optim(fn = loss_dim1_gamma,
#'                     par = gamma_0,
#'                     r = r,
#'                     beta = beta,
#'                     data_microsimulated = data_microsimulated,
#'                     EP_2015 = EP_2015,
#'                     EP_lon = EP_lon, 
#'                     method = "Nelder-Mead")
#'   
#'   return(NM_gamma)
#' }
#' 
#' 
#' estimation_r <- function(r_0, gamma = 0.5,
#'                          beta = 1,
#'                          data_microsimulated,
#'                          EP_2015,
#'                          EP_lon, 
#'                          normalize = FALSE,
#'                          scale = 'level',
#'                          exclude_negative = FALSE){
#'   
#'   NM_r <- optim(fn = loss_dim1_gamma,
#'                 par = r_0,
#'                 gamma = gamma,
#'                 beta = beta,
#'                 data_microsimulated = data_microsimulated,
#'                 EP_2015 = EP_2015,
#'                 EP_lon = EP_lon, 
#'                 method = "Nelder-Mead")
#'   
#'   return(NM_r)
#' }