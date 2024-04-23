#' @rdname create_moment
#'  
#' @inheritParams capitulation::life_cycle_model
#' 
#' @seealso \link[capitulation]{life_cycle_model}
#' @importFrom capitulation life_cycle_model
#' 
#' @export


create_moment_data <- function(EP_2015, EP_2018 = NULL,
                               EP_lon, data_microsimulated,
                               observed_moment_data = NULL,
                               probability_survival_var = NULL,
                               r = 0.03,
                               gamma = 0.5,
                               beta = 0.98,
                               r_low = NULL,
                               r_high = NULL,
                               r.parameters = NULL,
                               gamma.parameters = NULL,
                               beta.parameters = NULL,
                               non_ricardian = FALSE,
                               non_ricardian_var = NULL,
                               N_moments = 3,
                               ages = c(30,65),
                               stats = c("mean","sd"),
                               quantiles = c(0.1,0.5,0.9),                               
                               age_var_simulations = "age",
                               by = NULL,
                               normalize = TRUE,
                               scale_model = c("level","log","loglog"),
                               scale_variable_moment1 = c("level","log",'asinh'),
                               scale_moment1 = c("level","log", 'asinh'),                               
                               moment1 = c("level","share"),
                               scale_variable_moment2 = c("level","log",'asinh'),
                               stat_moment2 = c('proportion','difference','growth'),
                               wealth_var = 'PATFISOM',
                               exclude_negative = FALSE,
                               Hgiven_var = "H_given",
                               Hreceived_var = "H_received",
                               additional_vars = NULL){
  
  moment1 <- match.arg(moment1)
  
  
  # CREATE MOMENTS FOR SURVEY DATA
  # ----------------------------------  
  
  if (is.null(observed_moment_data)){
  
  EP_2015_moments <- create_moment(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                   EP_lon = EP_lon, N_moments = N_moments,
                                   moment_var = "moment_data",
                                   by = by,
                                   wealth_var = wealth_var,                                   
                                   normalize = normalize,
                                   scale_variable_moment1 = scale_variable_moment1,
                                   scale_variable_moment2 = scale_variable_moment2,
                                   scale_moment1 = scale_moment1,                               
                                   moment1 = moment1,
                                   stat_moment2 = stat_moment2,
                                   ages = ages,
                                   stats = stats,
                                   quantiles = quantiles,                                   
                                   exclude_negative = exclude_negative)
  
  
  } else{
    EP_2015_moments <- data.table::copy(observed_moment_data)
  }
  
  
  # CREATE MOMENTS FOR SIMULATED DATA
  # ----------------------------------  
  
  if (!is.null(r_low) &&  !is.null(r_high)){
    #    function_life_cycle <- capitulation::life_cycle_model_heterogeneity
  } else{
    function_life_cycle <- capitulation::life_cycle_model
  }
  
  vars_to_add <- unique(c(additional_vars, by))
  

  
  simulations <- life_cycle_model(data = data_microsimulated,
                                  r = r,
                                  gamma = gamma,
                                  beta = beta,
                                  r_low = r_low,
                                  r_high = r_high,
                                  r.parameters = r.parameters,
                                  gamma.parameters = gamma.parameters,
                                  beta.parameters = beta.parameters,
                                  probability_survival_var = probability_survival_var,
                                  scale_model = scale_model,
                                  income_var = "y_indiv",
                                  weight_var = NULL,
                                  wealthvar_survey = "K_observed",
                                  Hgiven_var = Hgiven_var,
                                  Hreceived_var = Hreceived_var,
                                  additional_vars = vars_to_add,
                                  non_ricardian = non_ricardian,
                                  non_ricardian_var = non_ricardian_var)
  

  if (scale_model != "level"){
    simulations[,'wealth' := exp(get("wealth"))]
  }
  
  if (is.null(EP_2018)){
    simulations_2018 <- NULL
  } else{
    simulations_2018 <- simulations 
  }
  
  
  simulations_moment <- create_moment(EP_2015 = simulations,
                                      EP_2018 = simulations_2018,
                                      by = by,
                                      wealth_var = 'wealth',
                                      age_var = age_var_simulations,
                                      N_moments = N_moments,
                                      moment_var = "moment_simulations",
                                      normalize = normalize,
                                      scale_variable_moment1 = scale_variable_moment1,
                                      scale_variable_moment2 = scale_variable_moment2,
                                      scale_moment1 = scale_moment1,                               
                                      moment1 = moment1,
                                      stat_moment2 = stat_moment2,
                                      ages = ages,
                                      stats = stats,
                                      quantiles = quantiles,                                   
                                      exclude_negative = exclude_negative)
  

  
  # MERGE
  # -------------------
  
  simulations_moment[,'weight' := NULL]
  
  moments_data <- merge(simulations_moment, EP_2015_moments,
                        by = "Nmoment")
  
  return(moments_data)
  
}



#' #' @rdname create_moment
#' #' @inheritParams 
#' 
#' create_quantile_data <- function(EP_2015, EP_lon, menages_structural2,
#'                                  r = 0.03, gamma = 0.5,
#'                                  beta = 0.98,
#'                                  N_moments = 3,
#'                                  ages = c(35,50),
#'                                  age_var_simulations = "age",
#'                                  stat_dW = "proportion",
#'                                  normalize = TRUE,
#'                                  scale = c("level","log"),
#'                                  exclude_negative = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   
#'   # CREATE MOMENTS FOR SURVEY DATA
#'   # ----------------------------------  
#'   
#'   EP_2015_moments <- create_quantile(EP_2015, EP_lon, N_moments = N_moments,
#'                                      moment_var = "moment_data",
#'                                      normalize = normalize,
#'                                      scale = scale,
#'                                      ages = ages,
#'                                      exclude_negative = exclude_negative)
#'   
#'   
#'   # CREATE MOMENTS FOR SIMULATED DATA
#'   # ----------------------------------  
#'   
#'   # simulations <- menages_structural2
#'   
#'   simulations <- capitulation::life_cycle_model(data = menages_structural2,
#'                                                 r = r,
#'                                                 gamma = gamma,
#'                                                 beta = beta,
#'                                                 income_var = "y_indiv",
#'                                                 weight_var = NULL,
#'                                                 wealthvar_survey = "K_observed")
#'   
#'   
#'   simulations_moment <- create_quantile(simulations,
#'                                         wealth_var = 'wealth',
#'                                         age_var = age_var_simulations,
#'                                         N_moments = N_moments,
#'                                         moment_var = "moment_simulations",
#'                                         normalize = normalize,
#'                                         scale = scale,
#'                                         ages = ages,
#'                                         exclude_negative = exclude_negative)
#'   
#'   
#'   # MERGE
#'   # -------------------
#'   
#'   
#'   moments_data <- merge(simulations_moment, EP_2015_moments,
#'                         by = c("Nmoment","q"))
#'   
#'   return(moments_data)
#'   
#' }
