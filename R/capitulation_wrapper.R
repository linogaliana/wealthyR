
#' A wrapper to use capitulation function with the GMM formalism
#' 
#' @param theta Structural parameters
#' @param ... Additional parameters provided

capitulation_wrapper <- function(theta, ...){
  
  args <- list(...)
  
  if (!('EP_2015' %in% names(args))){
    stop("argument EP_2015 missing")
  } else{
    EP_2015 <- args[['EP_2015']]
  }
  
  if (!('EP_lon' %in% names(args))){
    stop("argument EP_lon missing")
  } else{
    EP_lon <- args[['EP_lon']]
  }
  
  if (!('data_microsimulated' %in% names(args))){
    stop("argument data_microsimulated missing")
  }else{
    data_microsimulated <- args[['data_microsimulated']]
  }
  
  if (!('observed_moment_data' %in% names(args))){
    observed_moment_data <- NULL
  } else{
    observed_moment_data <- args[['observed_moment_data']]
  }
  
  
  if (!('EP_2018' %in% names(args))){
    EP_2018 <- NULL
  } else{
    EP_2018 <- args[['EP_2018']]
  }
  
  
  if (!('df_moment_theory' %in% names(args))){
    df_moment_theory <- NULL
  } else{
    df_moment_theory <- args[['df_moment_theory']]
  }
  
  if (!('df_moment_simulated' %in% names(args))){
    df_moment_simulated <- NULL
  } else{
    df_moment_simulated <- args[['df_moment_simulated']]
  }
  
  
  if (!('N_moments' %in% names(args))){
    N_moments <- 3L
  } else{
    N_moments <- args[['N_moments']]
  }
  
  
  if (!('age_var_simulations' %in% names(args))){
    age_var_simulations <- "age"
  } else{
    age_var_simulations <- args[['age_var_simulations']]
  }
  
  if (!('ages' %in% names(args))){
    ages <- c(30,65)
  } else{
    ages <- args[['ages']]
  }
  
  if (!('wealth_var' %in% names(args))){
    wealth_var <- "PATFISOM"
  } else{
    wealth_var <- args[['wealth_var']]
  }
  
  
  if (!('normalize' %in% names(args))){
    normalize <- FALSE
  } else{
    normalize <- args[['normalize']]
  }
  
  if (!('select_moments' %in% names(args))){
    select_moments <- NULL
  } else{
    select_moments <- args[['select_moments']]
  }
  
  if (!('scale_model' %in% names(args))){
    scale_model <- "level"
  } else{
    scale_model <- args[['scale_model']]
  }
  
  # Moment 1: default to E(log(.))  
  if (!('scale_variable_moment1' %in% names(args))){
    scale_variable_moment1 <- "log"
  } else{
    scale_variable_moment1 <- args[['scale_variable_moment1']]
  } 
  if (!('scale_moment1' %in% names(args))){
    scale_moment1 <- "level"
  } else{
    scale_moment1 <- args[['scale_moment1']]
  }
  

  if (!('moment1' %in% names(args))){
    moment1 <- "level"
  } else{
    moment1 <- args[['moment1']]
  }
  
  # Moment 2: default to log(w_2018) - log(w_2015))  
  if (!('scale_variable_moment2' %in% names(args))){
    scale_variable_moment2 <- "log"
  } else{
    scale_variable_moment2 <- args[['scale_variable_moment2']]
  } 
  if (!('stat_moment2' %in% names(args))){
    stat_moment2 <- "difference"
  } else{
    stat_moment2 <- args[['stat_moment2']]
  }  
  
  if (!('exclude_negative' %in% names(args))){
    exclude_negative <- FALSE
  } else{
    exclude_negative <- args[['exclude_negative']]
  }
  
  
  if (!('Hgiven_var' %in% names(args))){
    Hgiven_var <- "H_given"
  } else{
    Hgiven_var <- args[['Hgiven_var']]
  }
  
  if (!('Hreceived_var' %in% names(args))){
    Hreceived_var <- "H_received"
  } else{
    Hreceived_var <- args[['Hreceived_var']]
  }
  
  if (!('probability_survival_var' %in% names(args))){
    probability_survival_var <- NULL
  } else{
    probability_survival_var <- args[['probability_survival_var']]
  }
  
  
  
  if (!('additional_vars' %in% names(args))){
    additional_vars <- NULL
  } else{
    additional_vars <- args[['additional_vars']]
  } 
  if (!('by' %in% names(args))){
    by <- NULL
  } else{
    by <- args[['by']]
  } 


  # if (is.null(trans_theta)){
  #   trans_theta <- rep("identity", length(trans_theta))
  # }
  # 
  # inv_trans_theta <- trans_theta
  # inv_trans_theta[inv_trans_theta != "sigmoide"] <- "logit"
  # 
  # change_space <- function(theta, inv_trans_theta, i){
  #   if (inv_trans_theta[i] == "identity") return(theta[i])
  #   print("Transforming back in [0,1] scale")
  #   return(logit(theta[i]))
  # }
  # 
  # names_theta <- names(theta)
  # 
  # theta <- as.numeric(
  #   lapply(seq_along(theta), function(i) change_space(theta, trans_theta, i))
  # )
  # names(theta) <- names_theta
  
    
  if ('gamma' %in% names(theta)){
    gamma <- unlist(theta['gamma'])
  } else if ('gamma' %in% names(args)){
    gamma <- args[['gamma']]
  } else{
    gamma <- 0.5
  }
  
  if ('beta' %in% names(theta)){
    beta <- unlist(theta['beta'])
  } else if ('beta' %in% names(args)){
    beta <- args[['beta']]
  } else{
    beta <- 0.9
  }
  

  
  
    
  if ('r' %in% names(theta)){
    r <- unlist(theta['r'])
  } else if ('r' %in% names(args)){
    r <- args[['r']]
  } else{
    r <- 0.03
  }
  
  
  if ('r_low' %in% names(theta)){
    r_low <- theta['r_low']
  } else if ('r_low' %in% names(args)){
    r_low <- args[['r_low']]
  } else{
    r_low <- r
  }
  
  if ('r_high' %in% names(theta)){
    r_high <- theta['r_high']
  } else if ('r_high' %in% names(args)){
    r_high <- args[['r_high']]
  } else{
    r_high <- r
  }
  
  
  if ('r.parameters' %in% names(theta)){
    r.parameters <- as.numeric(unlist(theta[['r.parameters']]))
  } else if ('r.parameters' %in% names(args)){
    r.parameters <- args[['r.parameters']]
  } else if (sum(grepl("r.parameters", x = names(theta)))>0){
    r.parameters <- as.numeric(unlist(
      theta[grepl("r.parameters", x = names(theta))]
    ))
  } else{
    r.parameters <- NULL
  }  
  

  if ('gamma.parameters' %in% names(theta)){
    gamma.parameters <- as.numeric(unlist(theta['gamma.parameters']))
  } else if ('gamma.parameters' %in% names(args)){
    gamma.parameters <- args[['gamma.parameters']]
  } else if (sum(grepl("gamma.parameters", x = names(theta)))>0){
    gamma.parameters <- as.numeric(unlist(
      theta[grepl("gamma.parameters", x = names(theta))]
    ))
  } else{
    gamma.parameters <- NULL
  }  

  if ('beta.parameters' %in% names(theta)){
    beta.parameters <- as.numeric(unlist(theta['beta.parameters']))
  } else if ('r.parameters' %in% names(args)){
    beta.parameters <- args[['beta.parameters']]
  } else if (sum(grepl("beta.parameters", x = names(theta)))>0){
    beta.parameters <- as.numeric(unlist(
      theta[grepl("beta.parameters", x = names(theta))]
    ))
  } else{
    beta.parameters <- NULL
  }  
  if (!('non_ricardian' %in% names(args))){
    non_ricardian <- FALSE
  } else{
    non_ricardian <- args[['non_ricardian']]
  }
  if (!('non_ricardian_var' %in% names(args))){
    non_ricardian_var <- NULL
  } else{
    non_ricardian_var <- args[['non_ricardian_var']]
  }

  if ('trans_beta' %in% names(args)){
    print("Changing beta back to [0,1] scale")
    beta <- sigmoide(beta)
  }
    
  df_moment <- create_moment_data(EP_2015 = EP_2015, EP_2018 = EP_2018,
                                  EP_lon = EP_lon, 
                                  data_microsimulated = data_microsimulated,
                                  observed_moment_data = observed_moment_data,
                                  probability_survival_var = probability_survival_var,
                                  r = r,
                                  gamma = gamma,
                                  beta = beta,
                                  r.parameters = r.parameters,
                                  gamma.parameters = gamma.parameters,
                                  beta.parameters = beta.parameters,
                                  r_low = r_low,
                                  r_high = r_high,
                                  non_ricardian = non_ricardian,
                                  non_ricardian_var = non_ricardian_var,
                                  N_moments = N_moments,
                                  wealth_var = wealth_var,
                                  age_var_simulations = age_var_simulations,
                                  normalize = normalize,
                                  scale_model = scale_model,
                                  scale_variable_moment1 = scale_variable_moment1,
                                  scale_variable_moment2 = scale_variable_moment2,
                                  scale_moment1 = scale_moment1,                               
                                  moment1 = moment1,
                                  stat_moment2 = stat_moment2,
                                  ages = ages,
                                  exclude_negative = exclude_negative,
                                  Hgiven_var = Hgiven_var,
                                  Hreceived_var = Hreceived_var,
                                  additional_vars = additional_vars,
                                  by = by)

  if (!is.null(select_moments)){
    df_moment <- df_moment[get('Nmoment') %in% select_moments]
  }
  
  df_moment[,'epsilon' := get('moment_simulations')-get('moment_data')]
  
  
  return(df_moment) 
}
