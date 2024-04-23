#' Determine age where wealth is at its acme
#' 
#' @param EP_data Wealth survey data or microsimulated data
#' @param wealth_var Wealth variable name
#' @param age_var Age variable
#' @param ages Thresholds (lower, upper) that should be considered
#'  as sample
#' @param moment_var Variable name
#' @param moments_before Number of moments computed before
#' @param normalize Logical value indicating whether we want
#'  to normalize wealth variable to get variance equal to 1
#' @param scale Should values be considered in \emph{level} (default) or \emph{log}
#' @param survey_year Year that should be used to compute moments
#' @return A \link[data.table]{data.table} with 1 row and 2 columns
#' \describe{
#'   \item{\code{moment_var}}{Median varia tion of wealth, given \code{stat} and a \code{scale} arguments}
#'   \item{Nmoment}{Column equal to \code{moments_before}+1: moment number}
#' }
#' @export
#' @importFrom data.table %between%
#' @importFrom data.table .N


moment_age <- function(EP_data, ages = c(35,50),
                       wealth_var = 'PATFISOM',
                       age_var = 'AGEPR',
                       moment_var = "moment_data",
                       weight_var = "POND",
                       moments_before = 2,
                       normalize = TRUE,
                       scale_variable = c('level','log','asinh'),
                       scale_moment = c('level','log','asinh'),
                       survey_year = 2015,
                       by = NULL){
  
  # Avoid NSE notes
  . <- NULL
  
  # Ensure NAs by is considered as NULL
  if (is.null(by) || is.na(by)) by <- NULL
  
  # Ensure weight is 1 if not provided
  if (!(weight_var %in% colnames(EP_data))) EP_data[,c(weight_var) := 1L]
  
  scale_variable <- match.arg(scale_variable)
  scale_moment <- match.arg(scale_moment)
  
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]  
  
  # CREATE AGE VARIABLE BY 5 YEAR THRESHOLDS
  if (is.null(by)){
    EP_data[,'age_tranche' := floor(get(age_var)/5)]
  } else{
    EP_data[,'age_tranche' := get(by)]
  }
  
  # TRANSFORM IN LOG IF NECESSARY
  if (scale_variable == "log"){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  } else if (scale_variable == "asinh"){
    EP_data[, c(wealth_var) := asinh(get(wealth_var))]
  }
  
  # NORMALIZE IF NEEDED
  if (normalize){
    EP_data[,c(wealth_var) := get(wealth_var)/stats::sd(get(wealth_var), na.rm = TRUE)]
  }
  
  # MEDIAN WEALTH BY AGE CATEGORY
  EP_data <- EP_data[,.('W' = stats::median(get(wealth_var)),
                        'weight' = sum(get(weight_var))),
                     by = c('age_tranche')][order(get('age_tranche'))]
  
  # TRANSFORM IN LOG IF NECESSARY
  if (scale_moment == "log"){
    EP_data[, c("W") := log(get("W"))]
  } else if (scale_moment == "asinh"){
    EP_data[, c("W") := asinh(get("W"))]
  }
  
  # GET AGE
  if (is.null(by)){
    EP_2015_moments <- EP_data[get('age_tranche') %in% floor(ages/5)]
  } else{
    EP_2015_moments <- EP_data[get('age_tranche') %between% ages]
  }
  EP_2015_moments <- EP_2015_moments[,'W_obs' := get('W')][,.SD,.SDcols = c('age_tranche','W_obs','weight')]
  
  # ADD MOMENT COLUMN
  EP_2015_moments[,'Nmoment' := moments_before + seq_len(nrow(EP_2015_moments))]
  
  data.table::setnames(EP_2015_moments, old = 'W_obs',
                       new = moment_var)  
  
  EP_2015_moments[,'age_tranche' := NULL]
  
  EP_2015_moments[,c('weight') := get('weight')/sum(get('weight'))]
  
  return(EP_2015_moments)
}

moment_age_share <- function(EP_data, ages = c(35,50),
                             wealth_var = 'PATFISOM',
                             age_var = 'AGEPR',
                             moment_var = "moment_data",
                             moments_before = 2,
                             normalize = TRUE,
                             scale_variable = c('level','log', 'asinh'),
                             scale_moment = c('level','log', 'asinh'),
                             survey_year = 2015,
                             by = NULL){
  EP_moments <- moment_age(
    EP_data = EP_data, ages = ages,
    wealth_var = wealth_var,
    age_var = age_var,
    moment_var = moment_var,
    moments_before = moments_before,
    normalize = normalize,
    scale_variable = scale_variable,
    scale_moment = scale_moment,
    survey_year = survey_year,
    by = by)
  EP_moments <-  EP_moments[!is.na(get(moment_var)),
                            c(moment_var) := get(moment_var)/sum(get(moment_var))]
  return(EP_moments)
}


#' Function to fit moment based on multiple ages
#' @inheritParams moment_age
moment_age_multiple <- function(EP_data, ages = c(20,80),
                                wealth_var = 'PATFISOM',
                                age_var = 'AGEPR',
                                moment_var = "moment_data",
                                moments_before = 2,
                                normalize = TRUE,
                                scale = c('level','log', 'asinh'),
                                survey_year = 2015){
  
  # Avoid NSE notes
  . <- NULL
  
  
  scale <- match.arg(scale)
  
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]  
  
  # CREATE AGE VARIABLE BY 5 YEAR THRESHOLDS
  EP_data[,'age_tranche' := floor(get(age_var)/5)]
  
  # TRANSFORM IN LOG IF NECESSARY
  if (scale == "log"){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  } else if (scale == "asinh"){
    EP_data[,c(wealth_var) := asinh(get(wealth_var))]
  }
  
  # NORMALIZE IF NEEDED
  if (normalize){
    EP_data[,c(wealth_var) := get(wealth_var)/stats::sd(get(wealth_var), na.rm = TRUE)]
  }
  
  # MEDIAN WEALTH BY AGE CATEGORY
  EP_data <- EP_data[,.('W' = stats::median(get(wealth_var))),
                     by = c('age_tranche')][order(get('age_tranche'))]
  
  
  # GET AGE
  EP_2015_moments <- EP_data[get('age_tranche') %between% floor(c(min(ages)/5,max(ages)/5))]
  EP_2015_moments <- EP_2015_moments[,'W_obs' := get('W')][,.SD,.SDcols = c('age_tranche','W_obs')]
  
  # ADD MOMENT COLUMN
  EP_2015_moments[,'Nmoment' := moments_before + seq_len( 1 + floor(max(ages)/5) - floor(min(ages)/5))]
  
  data.table::setnames(EP_2015_moments, old = 'W_obs',
                       new = moment_var)  
  
  EP_2015_moments[,'age_tranche' := NULL]
  
  return(EP_2015_moments)
}
