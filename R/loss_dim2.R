#' #' Wrappers for \link{loss_function} with a 2-dimensional theta vector
#' #' 
#' #' @details 
#' #' \describe{
#' #'   \item{\code{loss_dim2}}{No transformation to constraint parameters value}
#' #'   \item{loss_dim2_log}{Exponential transformation to ensure positive values}
#' #'   \item{loss_dim2_logit}{Logit transformation to ensure positive values for gamma
#' #'    and values between 0 and 1 for r}
#' #' }
#' #' 
#' #' @inheritParams loss_function
#' #' @param theta \eqn{\theta = (r, \gamma)} parameter
#' #' 
#' #' @seealso \link{loss_function}
#' #' 
#' #' @export
#' 
#' loss_dim2 <- function(theta = c(0.02, .5),
#'                       r = NULL,
#'                       gamma = NULL,
#'                       beta = NULL,
#'                       data_microsimulated,
#'                       EP_2015,
#'                       EP_lon,
#'                       EP_2018 = NULL,
#'                       N_moments = 2L,
#'                       normalize = FALSE,
#'                       scale = c('level','log'),
#'                       exclude_negative = FALSE,
#'                       verbose = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   r <- ifelse('r' %in% names(theta['r']),
#'               yes = theta['r'],
#'               no = r)
#'   gamma <- ifelse('gamma' %in% names(theta['gamma']),
#'                   yes = theta['gamma'],
#'                   no = gamma)
#'   beta <- ifelse('beta' %in% names(theta['beta']),
#'                  yes = theta['beta'],
#'                  no = beta)
#'   theta2 <- c(r,gamma,beta)
#'   
#'   # if (sum(theta2<=0)) return(NA)
#'   
#'   if (verbose){
#'     cat(
#'       paste0("\n======= Parameter values: ========== \n ",
#'              paste(c('r: ', 'gamma: ', 'beta: '),
#'                    theta2, collapse = "; ")
#'       )
#'     )
#'   }
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_2018 = EP_2018,
#'                      EP_lon = EP_lon,
#'                      N_moments = N_moments,
#'                      r = r,
#'                      gamma = gamma,
#'                      beta = beta,
#'                      normalize = normalize,
#'                      scale = scale,
#'                      exclude_negative = exclude_negative,
#'                      dim = 2,
#'                      verbose = verbose)
#'   
#'   
#'   cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#'   
#' }
#' 
#' #' @rdname loss_dim2
#' #' @export
#' 
#' loss_dim2_log <- function(theta = log(c(0.02, .5)),
#'                           beta = 1,
#'                           data_microsimulated,
#'                           EP_2015,
#'                           EP_lon,
#'                           normalize = FALSE,
#'                           scale = c('level','log'),
#'                           exclude_negative = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   r = exp(theta[1])
#'   gamma = exp(theta[2])
#'   beta = beta
#'   theta2 <- c(r,gamma,beta)
#'   
#'   cat(
#'     paste0("\n======= Parameter values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: '), theta2, collapse = "; "))
#'   )
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_lon = EP_lon,
#'                      N_moments = 2,
#'                      r = r,
#'                      beta = beta,
#'                      gamma = gamma,
#'                      normalize = normalize,
#'                      scale = scale,
#'                      exclude_negative = exclude_negative,
#'                      dim = 2)
#'   
#'   
#'   cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#'   
#' }
#' 
#' #' Logit function
#' #' 
#' #' @param p A probability between 0 and 1
#' #' @return A value in R
#' 
#' logit <- function(p) log(p/(1-p))
#' 
#' #' Inverse logit (or sigmoid) function
#' #' 
#' #' @param x A real value in R
#' #' @return A value between 0 and 1
#' 
#' sigmoid <- function(x) 1/(1+exp(-x))
#' 
#' #' @rdname loss_dim2
#' #' @export
#' 
#' loss_dim2_R <- function(theta = c(logit(0.02), log(.5)),
#'                         beta = 1,
#'                         data_microsimulated,
#'                         EP_2015,
#'                         EP_lon,
#'                         normalize = FALSE,
#'                         scale = c('level','log'),
#'                         exclude_negative = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   cat(
#'     paste0("\n======= Optimization values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: '), theta, collapse = "; "))
#'   )
#'   
#'   
#'   r = sigmoid(theta[1])
#'   gamma = exp(theta[2])
#'   beta = beta
#'   theta2 <- c(r,gamma,beta)
#'   
#'   cat(
#'     paste0("\n======= Parameter values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: '), theta2, collapse = "; "))
#'   )
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_lon = EP_lon,
#'                      N_moments = 2,
#'                      r = r,
#'                      beta = beta,
#'                      gamma = gamma,
#'                      normalize = normalize,
#'                      scale = scale,
#'                      exclude_negative = exclude_negative,
#'                      dim = 2)
#'   
#'   
#'   cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#'   
#' }
#' 
#' 
#' #' Wrappers for loss_function with a 1-dimensional parameter
#' #'
#' #' @details 
#' #' \describe{
#' #'   \item{\code{loss_dim1_gamma}}{Take r and \eqn{beta} values, \eqn{gamma} parameter}
#' #'   \item{\code{loss_dim1_R}}{Take \eqn{gamma} and \eqn{beta} values, r parameter}
#' #' }
#' #' 
#' #' @inheritParams loss_function
#' #' @export
#' 
#' loss_dim1_gamma <- function(gamma, r = 0.02, beta = 1,
#'                             normalize = FALSE,
#'                             data_microsimulated,
#'                             EP_2015,
#'                             EP_lon,
#'                             scale = c('level','log'),
#'                             exclude_negative = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   r = r
#'   beta = beta
#'   theta <- c(r,gamma,beta)
#'   
#'   cat(
#'     paste0("\n======= Parameter values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: '), theta, collapse = "; "))
#'   )
#'   
#'   if (sum(theta<=0)>0) return(NA)
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_lon = EP_lon,
#'                      N_moments = 2,
#'                      r = r,
#'                      beta = beta,
#'                      gamma = gamma,
#'                      normalize = normalize,
#'                      scale = scale,
#'                      select_moments = 2,
#'                      exclude_negative = exclude_negative)
#'   
#'   
#'   cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#' }
#' 
#' #' @rdname loss_dim1_gamma
#' #' @export
#' 
#' loss_dim1_r <- function(r, gamma = 0.5, beta = 1,
#'                         normalize = FALSE,
#'                         data_microsimulated,
#'                         EP_2015,
#'                         EP_lon,
#'                         scale = c('level','log'),
#'                         exclude_negative = FALSE){
#'   
#'   beta = 1
#'   theta <- c(r,gamma,beta)
#'   
#'   cat(
#'     paste0("\n======= Parameter values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: '), theta, collapse = "; "))
#'   )
#'   
#'   if (sum(theta<=0)>0) return(NA)
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_lon = EP_lon,
#'                      N_moments = 1,
#'                      r = r,
#'                      beta = beta,
#'                      gamma = gamma,
#'                      normalize = normalize,
#'                      scale = scale,
#'                      exclude_negative = exclude_negative)
#'   
#'   cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#'   
#'   
#' }
#' 
#' 
#' 
#' #' Loss function for bidimensional case
#' #' 
#' #' @inheritParams loss_dim2
#' #' @param weights A two dimensional vector for weights between
#' #'  parameters dimensions
#' 
#' loss_dim1 <- function(theta,
#'                       r = NULL,
#'                       gamma = NULL,
#'                       beta = NULL,
#'                       data_microsimulated,
#'                       EP_2015,
#'                       EP_lon,
#'                       N_moments = 2L,
#'                       weights = 1L,
#'                       normalize = FALSE,
#'                       scale = 'level',
#'                       exclude_negative = FALSE,
#'                       verbose = FALSE){
#'   
#'   l <- loss_dim2(theta,
#'                  r = r,
#'                  gamma = gamma,
#'                  beta = beta,
#'                  data_microsimulated = data_microsimulated,
#'                  EP_2015 = EP_2015,
#'                  EP_lon = EP_lon,
#'                  N_moments = N_moments,
#'                  verbose = verbose)/weights
#'   return(
#'     sum(abs(l))
#'   )
#' }
