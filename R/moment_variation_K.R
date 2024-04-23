#' Get wealth variation between too survey years in microsimulated data
#' 
#' @param simulations Simulated longitudinal data
#' @param ages Ages conserved to compute moments
#' @param survey_years Survey data
#' @param wealth_var Wealth variable name
#' @param scale Scale for \code{wealth_var} that should
#'  be applied
#' @param exclude_negative Should we exclude negative values
#'  for wealth. Automatic if \code{scale} = \code{log}. 
#' 
#' @return Wide formatted wealth and age for survey_years for 
#'  people aged in ages brackets


deltaK_simulations <- function(simulations,ages = c(35,50),
                               survey_years = c(2015,2018),
                               by = NULL,
                               wealth_var = "wealth",
                               scale = c('level','log','asinh'),
                               exclude_negative = FALSE){
  
  # Ensure NAs by is considered as NULL
  if (is.null(by) || is.na(by)) by <- NULL


  scale <- match.arg(scale)
  
  # TEMPORARY COPY TO AVOID CHANGNING INITIAL DATA  
  tempdf <- data.table::copy(simulations)
  
  # EXCLUDE NEGATIVE VALUES IF exclude_negative is TRUE
  if (exclude_negative){
    tempdf <- tempdf[get(wealth_var)>0]
  }
  
  # KEEP YEARS RELATED TO SURVEY YEARS
  tempdf <- tempdf[get('annee') %in% survey_years]
  
  cols_to_keep <- c('Id','annee',
                    wealth_var,'age')
  if (!is.null(by)) cols_to_keep <- c(cols_to_keep, by)
  
  # KEEP ONLY RELEVENT COLUMNS
  tempdf <- tempdf[,.SD,
                   .SDcols = cols_to_keep]
  
  # KEEP PEOPLE WITH AGE BETWEEN ages IN 2015  
  EP_lon2 <- tempdf[(get('annee') == survey_years[2]) | ((get('annee')==survey_years[1]) & (get('age') %between% ages))]
  
  # PUT IN LOG IF NECESSARY
  if (scale == 'log'){
    EP_lon2 <- EP_lon2[get(wealth_var)>0]
    EP_lon2[,c(wealth_var) := log(get(wealth_var))]
  }
  
  # RESHAPE DATA INTO WIDE
  pivot_var <- "annee"
  value_var <- ifelse(is.null(by), "age", by)
  #if (!is.null(by)) pivot_var <- paste(c(pivot_var,by), collapse = " + ")
  if (nrow(EP_lon2) == 0) return(data.table::data.table())
  
  EP_lon2 <- data.table::dcast(EP_lon2, as.formula(paste0("Id ~", pivot_var)),
                               value.var = c(value_var, wealth_var))
  
  if (!is.null(by)){
    EP_lon2 <- na.omit(EP_lon2)
    pattern_vars <- paste0("(",paste(by, collapse = "|"), ")")
    cols_to_rename <- colnames(EP_lon2)[grepl(x = colnames(EP_lon2),
                                              pattern = paste0(
                                                pattern_vars, "_(2015|2018)$"
                                                )
    )]
    data.table::setnames(EP_lon2, old = cols_to_rename,
                         new = gsub(
                           x = cols_to_rename,
                           pattern = "_(2015|2018)_",
                           replacement = "_"
                         )
    )
  }
  
  # RENAME wealth_* INTO w_real_*  
  data.table::setnames(EP_lon2, old = colnames(EP_lon2),
                       new = gsub(paste0(wealth_var,"_"), "w_real_", colnames(EP_lon2)
                       )
  )
  
  
  # REMOVE NEGATIVE VALUES
  # EP_lon2 <- EP_lon2[get("w_real_2015")>0 & get("w_real_2018")>0]  
  
  return(
    EP_lon2
  )
  
}


#' Compute wealth evolution between two wealth survey waves 
#' 
#' Use panel wealth data to derive median wealth growth over 3 years
#' 
#' @param EP_lon Longitudinal data, either microsimulated or
#'  wealth survey created by \link{longitudinal_survey}
#' @param format Data structure. Either \emph{wide} (default)
#'  or \emph{long}
#' @param statfunc Statistical function (moment, quantile, etc. ) that should be used
#' @param wealth_var Wealth variable
#' @param deltaK_var Name for the growth variable that is temporarily created
#' @param moment_var Name for the moment variable that is created
#' @param age_2015_var Age variable storing age at first occurrence
#' @param ages Ages considered to restrict sample 
#' @param survey_years Collection years for the two survey waves
#' @param stat Should we collect $W_{1}/W_{0}$ (\emph{proportion}, default)
#'  or $W_{1}-W_{0}$ (\emph{difference})
#' @param normalize Logical value indicating whether values are normalized
#'  to compare distributions with variance equal to 1
#' @param scale Should values be considered in \emph{level} (default) or \emph{log}
#' @param exclude_negative Should we exclude negative values? Default to \code{FALSE}
#' @param ... Additional argument that should be provided to \code{statfunc}
#' @param min_moment Moment numbering from which that output should start
#' @return A \link[data.table]{data.table} with 1 row and 2 columns
#' \describe{
#'   \item{\code{moment_var}}{Median variation of wealth, given \code{stat} and a \code{scale} arguments}
#'   \item{Nmoment}{Column equal to 2: second moment}
#' }
#' @export


moment_variation_K <- function(EP_lon,
                               format = c('wide','long'),
                               by = NULL,
                               wealth_var_prefix = "w_real",
                               statfunc = "median",
                               wealth_var = "wealth",
                               deltaK_var = "dW",
                               moment_var = "moment_data",
                               age_2015_var = "AGEPR_2015",
                               weight_var = "POND",
                               ages = c(30,65),
                               survey_years = c(2015,2018),
                               stat = c('proportion','difference','growth'),
                               normalize = TRUE,
                               scale_variable = c("level","log",'asinh'),
                               exclude_negative = FALSE,
                               min_moment = 1L,
                               add_label = FALSE,
                               return_N = FALSE,
                               ...){
  
  # Avoid NSE notes
  . <- NULL
  
  # Ensure NAs by is considered as NULL
  if (is.null(by) || is.na(by)) by <- NULL
  
  
  # MATCH ARGUMENT NAMES
  scale_variable <- match.arg(scale_variable)
  format <- match.arg(format)
  stat <- match.arg(stat)
  
  EP_lon <- data.table::copy(EP_lon)
  
  # Ensure weight is 1 if not provided
  if (!(weight_var %in% colnames(EP_lon))) EP_lon[,c(weight_var) := 1L]
  
  
  col1 <- paste0(wealth_var_prefix,"_",survey_years[1])
  col2 <- paste0(wealth_var_prefix,"_",survey_years[2])
  
  
  if (format == 'long'){
    
    EP_lon[,'age_tranche' := FALSE]
    EP_lon[(get('annee') == survey_years[1] & (get('age') %between% ages)), c('age_tranche') := TRUE]
    EP_lon[is.na(get('age_tranche')), c('age_tranche') := FALSE]
    EP_lon[, c('age_tranche2') := sum(get('age_tranche')), by = "Id"]
    EP_lon <- EP_lon[(get("age_tranche2")>0)]
    EP_lon[,c('age_tranche', "age_tranche2") := NULL]
    EP_lon <- deltaK_simulations(EP_lon, ages = ages,
                                 by = by,
                                 survey_years = survey_years,
                                 scale = scale_variable,
                                 wealth_var = wealth_var,
                                 exclude_negative = exclude_negative
    )
    
    if (nrow(EP_lon) == 0) return(data.table::data.table())
    
    # long = Destinie -> don't care about creating weight_var
    EP_lon[,c(weight_var) := 1L]
    # data.table::setnames(EP_lon, old = colnames(EP_lon),
    #                      new = gsub("age",
    #                                 gsub(paste0("_",survey_years[1]),
    #                                      "", age_2015_var),
    #                                 colnames(EP_lon))
    # )
    
    # REPERER LES GENS QUI ONT ENTRE 35 ET 50 ANS EN 2015
    # EP_lon[,'age_tranche' := (get(paste0("age_",survey_years[1])) %between% ages)]
    # EP_lon <- EP_lon[(get('age_tranche'))]
    
    
  } else{
    
    # REPERER LES GENS QUI ONT ENTRE 35 ET 50 ANS EN 2015
    EP_lon[,'age_tranche' := (get(age_2015_var) %between% ages)]
    EP_lon <- EP_lon[(get('age_tranche'))]
    
    if (exclude_negative){
      EP_lon <- EP_lon[(get(col1)>0) & (get(col2)>0)]
    }
    
    
    # PUT IN LOG IF NECESSARY
    if (scale_variable == 'log'){
      EP_lon <- EP_lon[(get(col1)>0) & (get(col2)>0)]
      EP_lon[, (col1) := log(get(col1))]
      EP_lon[, (col2) := log(get(col2))]
    } else if (scale_variable == "asinh"){
      EP_lon[, (col1) := asinh(get(col1))]
      EP_lon[, (col2) := asinh(get(col2))]
      
    }
    
  }
  
  if (is.null(by) || is.na(by)) EP_lon[, 'tempvar' := "temp"]
  byvar <- ifelse(is.null(by), "tempvar", by)
  
  if (normalize){
    EP_lon[, (col1) := get(col1)/sd(get(col1), na.rm = TRUE),
           by = byvar]
    EP_lon[, (col2) := get(col2)/sd(get(col2), na.rm = TRUE),
           by = byvar]
  }
  
  if (stat %in% c("proportion")){
    EP_lon[,c(deltaK_var):= get(col2)/get(col1)]
  } else {
    EP_lon[,c(deltaK_var):= get(col2) - get(col1)]
    if (stat == "growth") EP_lon[,c(deltaK_var):= get(deltaK_var)/get(col1)]
  }
  
  
  moment_2 <- EP_lon[,.(
    "W" = as.numeric(
      lapply(statfunc, function(f) do.call(f, list(get(deltaK_var), na.rm = TRUE)))
    ),
    "m" = statfunc,
    'weight' = sum(get(weight_var))
  ),
  by = byvar]
  
  
  

  if (length(statfunc)==1) moment_2[,'m' := NULL]
  
  data.table::setnames(moment_2, old = 'W',
                       new = moment_var)
  
  moment_2[,'Nmoment' := 1:.N + min_moment]
  
  if (isFALSE(is.null(by)) && isTRUE(add_label)) moment_2[,'label' := unique(moment_2[[byvar]])]
  moment_2[, c(byvar) := NULL]
  
  if (isFALSE(return_N)) moment_2[,c('weight') := get('weight')/sum(get('weight'))]
  
  
  return(moment_2)
}

#' Get wealth variation between too survey years in microsimulated data
#' 
#' @inheritParams moment_variation_K
#' @param probs Probabilities to consider for quantile computations

quantile_variation_K <- function(EP_lon,
                                 format = 'wide',
                                 wealth_var = "wealth",
                                 deltaK_var = "dW",
                                 moment_var = "moment_data",
                                 age_2015_var = "AGEPR_2015",
                                 ages = c(35,50),
                                 survey_years = c(2015,2018),
                                 stat = 'proportion',
                                 normalize = TRUE,
                                 scale = c("level","log","asinh"),
                                 probs = seq(.1,.9,by = .1),
                                 exclude_negative = FALSE){
  
  
  # Avoid NSE notes
  . <- NULL
  
  scale <- match.arg(scale)
  
  EP_lon <- data.table::copy(EP_lon)
  
  
  if (format == 'long'){
    
    EP_lon <- deltaK_simulations(EP_lon, ages = ages,
                                 survey_years = survey_years,
                                 scale = scale,
                                 wealth_var = wealth_var,
                                 exclude_negative = exclude_negative
    )
    data.table::setnames(EP_lon, old = colnames(EP_lon),
                         new = gsub("age",gsub("_2015","",age_2015_var),colnames(EP_lon))
    )
    
    
  } else{
    
    # REPERER LES GENS QUI ONT ENTRE 35 ET 50 ANS EN 2015
    EP_lon[,'age_tranche' := (get(age_2015_var) %between% ages)]
    EP_lon <- EP_lon[(get('age_tranche'))]
    
    if (exclude_negative){
      EP_lon <- EP_lon[(get('w_real_2015')>0) & (get('w_real_2018')>0)]
    }
    
    # PUT IN LOG IF NECESSARY
    if (scale == 'log'){
      EP_lon <- EP_lon[(get('w_real_2015')>0) & (get('w_real_2018')>0)]
      EP_lon[, 'w_real_2015' := log(get('w_real_2015'))]
      EP_lon[, 'w_real_2018' := log(get('w_real_2018'))]
    } else if (scale == "asinh"){
      EP_lon[, 'w_real_2015' := asinh(get('w_real_2015'))]
      EP_lon[, 'w_real_2018' := asinh(get('w_real_2018'))]
    }
    
  }
  
  if (normalize){
    EP_lon[,`:=` ('w_real_2015' = get('w_real_2015')/sd(get('w_real_2015'), na.rm = TRUE),
                  'w_real_2018' = get('w_real_2018')/sd(get('w_real_2018'), na.rm = TRUE))]
  }
  
  if (stat == "proportion"){
    EP_lon[,c(deltaK_var):= get('w_real_2018')/get('w_real_2015')]
  } else{
    EP_lon[,c(deltaK_var):= get('w_real_2018') - get('w_real_2015')]
  }
  
  moment_2 <- EP_lon[!is.infinite(get(deltaK_var))
                     ,.('W' = c(quantile(get(deltaK_var),na.rm=TRUE, probs = probs),
                                mean(get(deltaK_var),na.rm=TRUE)),
                        'q' = c(as.character(probs),"mean"))]
  
  moment_2[, 'Nmoment' := 2]
  
  data.table::setnames(moment_2, old = 'W',
                       new = moment_var)
  
  return(moment_2)
}
