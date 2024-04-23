
#' Cross sectional moments for wealth distribution
#' 
#' Compute population or simulated moments
#'  for wealth distribution
#' 
#' @inheritParams moment_pic
#' 
#' @param quantiles Quantiles that should be computed
#' @param min_moment Minimal number for moment identifier
#' 
#' @return A \link[data.table]{data.table} with three columns:
#'  \code{moment_var} (function argument), \code{p} (quantile considered)
#'  and \code{Nmoment}

stat_distribution_K <- function(EP_data,
                                  quantiles = c(0.1,0.5,0.9),
                                  wealth_var = 'PATFISOM',
                                  moment_var = "moment_data",
                                  normalize = TRUE,
                                  scale = c('log','level'),
                                  exclude_negative = TRUE,
                                  survey_year = 2015,
                                  min_moment = 1L){
  
  # Avoid NSE notes
  . <- NULL
  
  scale <- match.arg(scale)
  
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]
  
  # EXCLUDE NEGATIVE VALUES IF exclude_negative is TRUE
  if (exclude_negative){
    EP_data <- EP_data[get(wealth_var)>0]
  }
  
  # PUT IN LOG IF NECESSARY
  if (scale == 'log'){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  }
  
  # NORMALIZE DATA
  if (normalize) EP_data[, c(wealth_var) := get(wealth_var)/stats::sd(get(wealth_var),na.rm = TRUE)]

  
  moments <- EP_data[,.(
    "moment_var" = stats::quantile(get(wealth_var), na.rm = TRUE,
                   probs = quantiles),
    'p' = paste0("p", quantiles*100)
  )]
  
  data.table::setnames(moments, old = "moment_var",
                       new = moment_var)
  
  moments <- moments[,'Nmoment' := 1:.N + min_moment]
  
  return(moments)
}


#' @rdname stat_distribution_K
#' @param stats Moment that should be used. This must be a
#'  data summary statistics that should match a base R function

moment_distribution_K <- function(EP_data,
                                stats = c("mean","sd"),
                                wealth_var = 'PATFISOM',
                                moment_var = "moment_data",
                                normalize = TRUE,
                                scale = c('log','level'),
                                exclude_negative = TRUE,
                                survey_year = 2015,
                                min_moment = 1L){
  
  # Avoid NSE notes
  . <- NULL
  
  # Ensure variance is considered as var function
  stats[stats == "variance"] <- "var"  
  
  scale <- match.arg(scale)
  
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]
  
  # EXCLUDE NEGATIVE VALUES IF exclude_negative is TRUE
  if (exclude_negative){
    EP_data <- EP_data[get(wealth_var)>0]
  }
  
  # PUT IN LOG IF NECESSARY
  if (scale == 'log'){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  }
  
  # NORMALIZE DATA
  if (normalize) EP_data[, c(wealth_var) := get(wealth_var)/stats::sd(get(wealth_var),na.rm = TRUE)]
  
  
  moments = EP_data[,.(
    "moment_var" = as.numeric(
      lapply(stats, function(f) do.call(f, list(get(wealth_var))))
      ),
    "m" = stats
    )]

  data.table::setnames(moments, old = "moment_var",
                       new = moment_var)
  
  moments[,'Nmoment' := 1:.N + min_moment]
  
  return(moments)
}

