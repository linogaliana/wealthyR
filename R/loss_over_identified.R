
#' loss_over_identified <- function(
#'   theta = c(0.02, .5, 1),
#'   r = NULL,
#'   gamma = NULL,
#'   beta = NULL,
#'   r_low = NULL,
#'   r_high = NULL,
#'   data_microsimulated,
#'   EP_2015,
#'   EP_2018 = NULL,
#'   EP_lon,
#'   df_moment_theory = NULL,
#'   df_moment_simulated = NULL,
#'   N_moments = 8L,
#'   weights = 1L,
#'   normalize = FALSE,
#'   scale = c('log','level'),
#'   exclude_negative = FALSE,
#'   select_moments = NULL,
#'   loss_scale = c("log","level","square"),
#'   verbose = FALSE,
#'   return_moment = FALSE, ...
#' ){
#'   
#'   scale <- match.arg(scale)
#'   
#'   if (N_moments<=sum(!is.na(theta))) message("System is not overidentified, using the appropriate loss_dim1 function")
#'   
#'   
#'   # NAME THETA DIMENSIONS
#'   if (is.null(names(theta))){
#'     message("theta dimensions are not named: assuming order is (r,gamma,beta)")
#'     names(theta) <- c("r","gamma","beta")[1:length(theta)]
#'   }  
#'   
#'   r <- ifelse('r' %in% names(theta),
#'               yes = theta['r'],
#'               no = r)
#'   gamma <- ifelse('gamma' %in% names(theta),
#'                   yes = theta['gamma'],
#'                   no = gamma)
#'   beta <- ifelse('beta' %in% names(theta),
#'                  yes = theta['beta'],
#'                  no = beta)
#'   
#'   # HOMOGENEOUS OR HETEROGENEOUS INTEREST RATE DEPENDING ON CASES
#'   if (is.null(r_low)) r_low <- r
#'   if (is.null(r_high)) r_high <- r
#'   
#'   r_low <- ifelse('r_low' %in% names(theta),
#'                  yes = theta['r_low'],
#'                  no = r_low)
#'   r_high <- ifelse('r_high' %in% names(theta),
#'                  yes = theta['r_high'],
#'                  no = r_high)
#'   
#'   
#'   theta2 <- c(r,gamma,beta, r_low, r_high)
#'   
#'   # if (sum(theta2<=0)) return(NA)
#'   
#'   dim_return <- ifelse(N_moments == sum(!is.na(theta)), N_moments, 1L)
#'   
#'   if (verbose) cat(
#'     paste0("\n======= Parameter values: ========== \n ", paste(c('r: ', 'gamma: ', 'beta: ', 'r_low: ', 'r_high: '), theta2, collapse = "; "))
#'   )
#'   
#'   
#'   l <- loss_function(data_microsimulated = data_microsimulated,
#'                      EP_2015 = EP_2015,
#'                      EP_2018 = EP_2018,
#'                      EP_lon = EP_lon,
#'                      df_moment_simulated = df_moment_simulated,
#'                      df_moment_theory = df_moment_theory,
#'                      r = r,
#'                      beta = beta,
#'                      gamma = gamma,
#'                      r_low = r_low,
#'                      r_high = r_high,
#'                      N_moments = N_moments,
#'                      weights = weights,
#'                      age_var_simulations = 'age',
#'                      ages = c(35,50),
#'                      normalize = normalize,
#'                      select_moments = select_moments,
#'                      scale = scale,
#'                      exclude_negative = exclude_negative,
#'                      dim = dim_return,
#'                      loss_scale = loss_scale,
#'                      verbose = verbose,
#'                      return_moment = return_moment)
#'   
#'   
#'   
#'   if (verbose) cat(paste0("\nLoss function value: ", l))
#'   
#'   return(l)
#'   
#'   
#' }
