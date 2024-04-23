# FIRST MOMENT
# ------------------

#' Acme of median wealth function by age (5 years cohort)
#' 
#' @param EP_data Wealth survey
#' @param wealth_var Wealth variable in survey
#' @param age_var Age variable in survey
#' @param moment_var Variable name for moment result
#' @param scale Scale for the variable to consider. Can be level (default)
#'  or log
#' @param normalize Logical value indicating whether we want to normalize
#'  variable
#' @param exclude_negative Logical value indicating whether we should
#'  exclude negative wealth values
#' @inheritParams moment_age
#' @param moment_number Moment number that should be generated
#' 
#' @return A \code{data.table} with 1 row with format
#' presented in \code{example} section
#' @examples \dontrun{
#' moment1 <- moment_pic(EP_2015)
#' moment1
#' # age_tranche       W
#' # 1:          14 47533.5
#' }
#' @importFrom stats quantile median sd
#' @export

moment_pic <- function(EP_data, wealth_var = 'PATFISOM',
                       age_var = 'AGEPR',
                       moment_var = "moment_data",
                       normalize = FALSE,
                       scale = c('log', 'level', "asinh"),
                       exclude_negative = FALSE,
                       survey_year = 2015,
                       moment_number = 1L,
                       scale_moment1 = c('level', "share"),
                       by = NULL){
  
  . <- NULL
  
  scale <- match.arg(scale)
  scale_moment1 <- match.arg(scale_moment1)

    
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]
  
  # EXCLUDE NEGATIVE VALUES IF exclude_negative is TRUE
  if (exclude_negative){
    EP_data <- EP_data[get(wealth_var)>0]
  }
  
  # CREATE AGE VARIABLE BY 5 YEAR THRESHOLDS
  EP_data[,'age_tranche' := floor(get(age_var)/5)]
  
  # PUT IN LOG IF NECESSARY
  if (scale == 'log'){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  } else if (scale == "asinh"){
    EP_data[,c(wealth_var) := asinh(get(wealth_var))]
  }
  
  # NORMALIZE DATA
  if (normalize) EP_data[, c(wealth_var) := get(wealth_var)/sd(get(wealth_var),na.rm = TRUE)]
  
  # MEDIAN WEALTH BY AGE CATEGORY (KEEP ONLY PEOPLE BETWEEN 25 AND 75)
  if (is.null(by)){
    byvar <- "tempby"
    EP_data[,c(byvar) := 1L]
  } else{
    byvar <- by
  }
  
  EP_2015_pic <- EP_data[,.('W' = median(get(wealth_var))),
                         by = c('age_tranche', byvar)][order(get('age_tranche'))]
  EP_2015_pic <- EP_2015_pic[get('age_tranche') %between% c(5, 15)]
  
  
  
  # RETURN MAXIMUM
  if (scale_moment1 == "share"){
    EP_2015_pic[, `:=`(c('W'), get('W')/sum(get('W'), 
                                                 na.rm = TRUE))]
  }
  EP_2015_pic <- EP_2015_pic[get('W')==max(get('W'), na.rm = TRUE)]
  
  # KEEP ONE VALUE IF TIES
  EP_2015_pic <- EP_2015_pic[sample(seq_len(nrow(EP_2015_pic)),1),]
  
  data.table::setnames(EP_2015_pic, old = 'W',
                       new = moment_var)
  
  EP_2015_pic[,'age_tranche' := NULL]
  
  EP_2015_pic[,'weight' := 1]
  
  if (byvar == "tempby") EP_2015_pic[, c(byvar) := NULL]
  
  return(EP_2015_pic[,'Nmoment' := moment_number])
}

#' Extract acme for wealth
#' 
#' @inheritParams moment_pic
#' @param probs Probabilities to consider for quantile
#'  computations

quantile_pic  <- function(EP_data, wealth_var = 'PATFISOM',
                          age_var = 'AGEPR',
                          moment_var = "moment_data",
                          normalize = TRUE,
                          scale = c('level','log', 'asinh'),
                          probs = seq(.1,.9,by = .1),
                          exclude_negative = FALSE,
                          survey_year = 2015){
  
  # Avoid NSE notes
  .<-NULL
  
  scale <- match.arg(scale)
  
  # COPY DATA.TABLE TO AVOID MODIFYING INITIAL OBJECT
  EP_data <- data.table::copy(EP_data)
  
  # KEEP ONLY OBSERVATIONS FOR DEFINED SURVEY YEAR
  EP_data <- EP_data[get('annee') == survey_year]
  
  # EXCLUDE NEGATIVE VALUES IF exclude_negative is TRUE
  if (exclude_negative){
    EP_data <- EP_data[get(wealth_var)>0]
  }
  
  # CREATE AGE VARIABLE BY 5 YEAR THRESHOLDS
  EP_data[,'age_tranche' := floor(get(age_var)/5)]
  
  # PUT IN LOG IF NECESSARY
  if (scale == 'log'){
    EP_data <- EP_data[get(wealth_var)>0]
    EP_data[,c(wealth_var) := log(get(wealth_var))]
  } else if (scale == "asinh"){
    EP_data[,c(wealth_var) := asinh(get(wealth_var))]
  }
  
  # NORMALIZE DATA
  if (normalize) EP_data[, c(wealth_var) := get(wealth_var)/sd(get(wealth_var),na.rm = TRUE)]
  
  # MEDIAN WEALTH BY AGE CATEGORY
  EP_2015_pic <- EP_data[,.('W' = c(stats::quantile(get(wealth_var), probs = probs, na.rm = TRUE),
                                    mean(get(wealth_var), na.rm = TRUE)),
                            'q' = c(as.character(probs),'mean')),
                         by = c('age_tranche')]
  
  # KEEP AGES BETWEEN 25 AND 75  
  EP_2015_pic <- EP_2015_pic[order(get('age_tranche'))]
  EP_2015_pic <- EP_2015_pic[get('age_tranche') %between% c(5, 15)]
  
  # RETURN MAXIMUM
  EP_2015_pic2 <- EP_2015_pic[get('q') == 0.5]
  EP_2015_pic2 <- EP_2015_pic2[get('W')==max(get('W'), na.rm = TRUE)]
  
  # KEEP ONE VALUE IF TIES
  EP_2015_pic2 <- EP_2015_pic2[sample(seq_len(nrow(EP_2015_pic2)),1),]
  
  # MERGE BACK TO QUANTILES DATAFRAME
  EP_2015_pic2 <- EP_2015_pic2[,.SD,.SDcols = 'age_tranche']
  EP_2015_pic2[,'max' := TRUE]
  
  EP_2015_pic <- merge(EP_2015_pic,EP_2015_pic2, by = 'age_tranche')
  EP_2015_pic[,'max' := NULL]
  
  data.table::setnames(EP_2015_pic, old = 'W',
                       new = moment_var)
  
  return(
    EP_2015_pic[,'Nmoment' := 1]
  )
  
}
