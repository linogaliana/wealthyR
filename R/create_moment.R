
#' Create a dataframe storing moments regarding wealth distribution
#' 
#' Create a series of moments for indirect inference, either using
#'  wealth survey or microsimulated population
#'  
#' @details 
#' \describe{
#'   \item{\code{create_moment}}{Either use wealth survey or microsimulated population}
#'   \item{create_moment_data}{Compute moments for both wealth survey and microsimulated population}
#' }
#'  
#' @param EP_2015 Wealth survey used for its cross-sectional dimension
#'  or microsimulated population
#' @param EP_lon Longitudinal wealth survey used for its time dimension
#' @param EP_2018 (Optional). Wealth survey used for its cross-sectional dimension
#'  or microsimulated population
#' @param data_microsimulated Microsimulated data that follows \link[capitulation]{life_cycle_model}
#'  expected structured
#' @param N_moments Number of moments needed. See \code{details} section 
#' for the ordered list of moments
#' @inheritParams moment_pic
#' @inheritParams moment_distribution_K
#' @inheritParams moment_variation_K
#' @inheritParams moment_age
#' @param age_var_simulations Age variable in simulated data
#' @param r_low Low interest rate if heterogeneity is introduced
#' @param r_high High interest rate if heterogeneity is introduced
#' @param Hgiven_var Variable name for inheritance given
#' @param Hreceived_var Variable name for inheritance received
#' 
#' @details Order is the following:
#' \enumerate{
#'   \item Acme for wealth distribution by age (\link{moment_pic})
#'   \item Wealth evolution between two periods  (\link{moment_variation_K})
#'   \item Age where wealth is at its acme (\link{moment_age})
#' }
#' @return A \link[data.table]{data.table} object
#' @seealso 
#' 
#' @export

create_moment <- function(EP_2015,
                          EP_2018 = NULL,
                          EP_lon = NULL,
                          by = NULL,
                          N_moments = 1,
                          ages_cross_section = c(20,80),
                          ages = c(30,65),
                          stats = c("mean","sd"),
                          quantiles = c(0.1,0.5,0.9),
                          scale_variable_moment1 = c("level","log", "asinh"),
                          scale_variable_moment2 = c("level","log", "asinh"),
                          scale_moment1 = c("level","log", "asinh"),
                          moment1 = c("level","share"),
                          stat_moment2 = c('proportion','difference','growth'),
                          wealth_var = 'PATFISOM',
                          age_var = 'AGEPR',
                          deltaK_var = "dW",
                          age_2015_var = "AGEPR_2015",
                          moment_var = "moment_data",
                          normalize = FALSE,
                          survey_years = c(2015, 2018),
                          exclude_negative = FALSE){
  
  # Avoid NSE notes
  . <- NULL  
  
  scale_variable_moment1 <- match.arg(scale_variable_moment1, several.ok = TRUE)
  scale_variable_moment2 <- match.arg(scale_variable_moment2, several.ok = TRUE)
  moment1 <- match.arg(moment1)
  scale_moment1 <- match.arg(scale_moment1)
  stat_moment2 <- match.arg(stat_moment2)

  
  if (!is.null(by)){
    if (length(unique(by))>1){
      bys <- by
    } else{
      bys <- rep(by, 3)
      if (length(by)>3) message("For the moment, only a three dimensional by variable is implemented")
    }
  } else{
    bys <- NULL
  }
  
  
  # FIRST MOMENT: WEALTH ACME ---------------------------
  
  
  if (is.null(by) || is.na(bys[1])){
    m1 <- moment_pic(EP_2015, wealth_var = wealth_var,
                          age_var = age_var,
                          moment_var = moment_var,
                          normalize = normalize,
                          scale = scale_variable_moment1,
                          survey_year = survey_years[1],
                          exclude_negative = exclude_negative,
                     scale_moment1 = moment1)
  } else{
    m1 <- moment_age(
      EP_2015, ages = ages_cross_section,
      wealth_var = wealth_var,
      age_var = age_var,
      moment_var = moment_var,
      moments_before = 0,
      normalize = normalize,
      scale_variable = scale_variable_moment1,
      survey_year = survey_years[1],
      by = bys[1])
    if (moment1 == "share"){
      m1[, c(moment_var) := get(moment_var)/sum(get(moment_var),na.rm = TRUE)]
    }
  }
  

  if (N_moments <= max(m1$Nmoment)) return(
    m1[get('Nmoment') <= N_moments]
  )
  
  # ENSURE USE GOOD DATA STRUCTURE FOR LONGITUDINAL PANEL --------------------
  
  if (is.null(EP_lon)){
    
    message("Assuming data are long formatted")
    EP_lon <- EP_2015
    format <- "long"
    
  } else{
    format <- "wide"
  }
  
  # SECOND MOMENT: WEALTH GROWTH PANEL ---------------------------
  
  # scale2 <-  ifelse(N_moments<=2, yes = "level",
  #                   no = scale)
  
  
  moment2 <- moment_variation_K(EP_lon = EP_lon,
                                format = format,
                                by = bys[2],
                                statfunc = "median",
                                deltaK_var = deltaK_var,
                                moment_var = moment_var,
                                age_2015_var = age_2015_var,
                                ages = ages,
                                normalize = normalize,
                                scale_variable = scale_variable_moment2,
                                stat = stat_moment2,
                                survey_years = survey_years,
                                exclude_negative = exclude_negative,
                                min_moment = max(max(m1$Nmoment)))
  
  
  
  moments <- data.table::rbindlist(list(m1,moment2),
                                   use.names=TRUE
  )
  
  if (N_moments <= max(moments$Nmoment)) return(
    moments[get('Nmoment') <= N_moments]
  )
  
  return(moments)
  
  # THIRD MOMENT: WEALTH ACME 2018 ---------------------------
  
  
  # if (!is.null(EP_2018)) moment1bis <- moment_pic(EP_2018, wealth_var = wealth_var,
  #                                                 age_var = age_var,
  #                                                 moment_var = moment_var,
  #                                                 normalize = normalize,
  #                                                 scale = scale,
  #                                                 survey_year = survey_years[2],
  #                                                 exclude_negative = exclude_negative,
  #                                                 moment_number = max(moments$Nmoment)+1)
  
  # moment1bis <- moment_age_share(
  #   EP_2015, ages = ages_cross_section,
  #   wealth_var = wealth_var,
  #   age_var = age_var,
  #   moment_var = moment_var,
  #   moments_before = max(moments$Nmoment),
  #   normalize = normalize,
  #   scale = scale,
  #   survey_year = survey_years[1],
  #   by = bys[3]) 
  # 
  # moments <- data.table::rbindlist(list(moments,moment1bis),
  #                                  use.names = TRUE)
  # 
  # if (N_moments <= max(moments$Nmoment)) return(
  #   moments[get('Nmoment') <= N_moments]
  # )
  # 
  # 
  # # 4th-?? MOMENTS: CROSS SECTIONAL MOMENTS 2015  --------------------------
  # 
  # 
  # moments_wave1 <- moment_distribution_K(
  #   EP_2015,
  #   stats = stats,
  #   wealth_var = wealth_var,
  #   moment_var = moment_var,
  #   normalize = normalize,
  #   scale = scale,
  #   exclude_negative = exclude_negative,
  #   survey_year = survey_years[1],
  #   min_moment = max(moments$Nmoment))
  # 
  # moments_wave1[,'m' := NULL]
  # 
  # moments <- data.table::rbindlist(list(moments,
  #                                       moments_wave1))
  # 
  # 
  # # ??-?? MOMENTS: PANEL MOMENTS 2015 TO 2018  --------------------------
  # 
  # moments_distribution_dK <- moment_variation_K(EP_lon = EP_lon,
  #                                               format = format,
  #                                               statfunc = stats,
  #                                               deltaK_var = deltaK_var,
  #                                               moment_var = moment_var,
  #                                               age_2015_var = age_2015_var,
  #                                               ages = ages,
  #                                               stat = "difference",
  #                                               normalize = normalize,
  #                                               scale = scale,
  #                                               survey_years = survey_years,
  #                                               exclude_negative = exclude_negative,
  #                                               min_moment = max(moments$Nmoment))
  # 
  # 
  # moments_distribution_dK[,'m' := NULL]
  # 
  # 
  # moments <- data.table::rbindlist(list(moments,
  #                                       moments_distribution_dK))
  # 
  # 
  # # ??-?? MOMENTS: CROSS SECTIONAL MOMENTS 2018  --------------------------
  # 
  # if (!is.null(EP_2018)){
  #   
  #   moments_wave2 <- moment_distribution_K(
  #     EP_2018,
  #     stats = stats,
  #     wealth_var = wealth_var,
  #     moment_var = moment_var,
  #     normalize = normalize,
  #     scale = scale,
  #     exclude_negative = exclude_negative,
  #     survey_year = survey_years[2],
  #     min_moment = max(moments$Nmoment))
  #   
  #   moments_wave2[,'m' := NULL]
  #   
  #   moments <- data.table::rbindlist(list(moments,
  #                                         moments_wave2))
  #   
  # }
  # 
  # if (N_moments <= max(moments$Nmoment)) return(
  #   moments[get('Nmoment') <= N_moments]
  # )
  # 
  # 
  # # MOMENT ??: FIT AGE (2015) ---------------------------------
  # 
  # moment34 <- moment_age_multiple(EP_2015, wealth_var = wealth_var,
  #                                 ages = c(20,80),
  #                                 age_var = age_var,
  #                                 moment_var = moment_var,
  #                                 normalize = TRUE,
  #                                 scale = scale,
  #                                 moments_before = max(moments$Nmoment),
  #                                 survey_year = survey_years[1])
  # 
  # moments <- data.table::rbindlist(
  #   list(moments,moment34)
  # )
  # 
  # if (N_moments <= max(moments$Nmoment)) return(
  #   moments[get('Nmoment') <= N_moments]
  # )
  # 
  # # MOMENT ??: FIT AGE (2018) ---------------------------------
  # 
  # moment45 <- moment_age_multiple(EP_2018, wealth_var = wealth_var,
  #                                 ages = c(20,80),
  #                                 age_var = age_var,
  #                                 moment_var = moment_var,
  #                                 normalize = TRUE,
  #                                 scale = scale,
  #                                 moments_before = max(moments$Nmoment),
  #                                 survey_year = survey_years[2])
  # 
  # moments <- data.table::rbindlist(
  #   list(moments,moment45)
  # )
  # 
  # if (N_moments <= max(moments$Nmoment)) return(
  #   moments[get('Nmoment') <= N_moments]
  # )
  # 
  # 
  # # ??-?? MOMENTS: CROSS SECTIONAL QUANTILES ---------------------------
  # 
  # distribution_wave1 <- stat_distribution_K(
  #   EP_2015,
  #   quantiles = quantiles,
  #   wealth_var = wealth_var,
  #   moment_var = moment_var,
  #   normalize = normalize,
  #   scale = scale,
  #   exclude_negative = exclude_negative,
  #   survey_year = survey_years[1],
  #   min_moment = max(moments$Nmoment))
  # 
  # distribution_wave1[,'p' := NULL]
  # 
  # moments <- data.table::rbindlist(list(moments,
  #                                       distribution_wave1))
  # 
  # 
  # 
  # if (!is.null(EP_2018)){
  #   
  #   distribution_wave2 <- stat_distribution_K(
  #     EP_2018,
  #     quantiles = quantiles,
  #     wealth_var = wealth_var,
  #     moment_var = moment_var,
  #     normalize = normalize,
  #     scale = scale,
  #     exclude_negative = exclude_negative,
  #     survey_year = survey_years[2],
  #     min_moment = max(moments$Nmoment))
  #   
  #   distribution_wave2[,'p' := NULL]
  #   
  #   moments <- data.table::rbindlist(list(moments,
  #                                         distribution_wave2))
  #   
  # }
  # 
  # if (N_moments <= max(moments$Nmoment)) return(
  #   moments[get('Nmoment') <= N_moments]
  # )
  # 
  # 
  # message(sprintf("Too many moments. Maximum number of moments possible: %s",
  #                 max(moments$Nmoment))
  # )
  # 
  # return(moments)
  
}

#' #' @rdname create_moment
#' 
#' create_quantile <- function(EP_2015, EP_lon = NULL, N_moments = 1,
#'                             ages = c(35,50),
#'                             wealth_var = 'PATFISOM',
#'                             age_var = 'AGEPR',
#'                             deltaK_var = "dW",
#'                             age_2015_var = "AGEPR_2015",
#'                             moment_var = "moment_data",
#'                             normalize = TRUE,
#'                             scale = c("level","log"),
#'                             exclude_negative = FALSE){
#'   
#'   scale <- match.arg(scale)
#'   
#'   moment1 <- quantile_pic(EP_2015, wealth_var = wealth_var,
#'                           age_var = age_var,
#'                           moment_var = moment_var,
#'                           normalize = normalize,
#'                           scale = scale,
#'                           exclude_negative = exclude_negative)
#'   
#'   
#'   
#'   if (N_moments<=1) return(moment1)
#'   
#'   moment1[,'age_tranche' := NULL]
#'   
#'   if (is.null(EP_lon)){
#'     
#'     message("Assuming data are long formatted")
#'     EP_lon <- EP_2015
#'     format <- "long"
#'     
#'   } else{
#'     format <- "wide"
#'   }
#'   
#'   moment2 <- quantile_variation_K(EP_lon = EP_lon,
#'                                   format = format,
#'                                   deltaK_var = deltaK_var,
#'                                   moment_var = moment_var,
#'                                   age_2015_var = age_2015_var,
#'                                   ages = ages,
#'                                   stat = "proportion",
#'                                   normalize = normalize,
#'                                   scale = scale,
#'                                   exclude_negative = exclude_negative)
#'   
#'   
#'   moments <- data.table::rbindlist(list(moment1,moment2))
#'   
#'   if (N_moments==2) return(moments)
#'   
#'   moment34 <- moment_age(EP_2015, wealth_var = wealth_var,
#'                          age_var = age_var,
#'                          moment_var = moment_var,
#'                          moments_before = 2,
#'                          normalize = TRUE,
#'                          scale = scale)
#'   
#'   moments <- data.table::rbindlist(
#'     list(moments,moment34)
#'   )
#'   
#'   if (N_moments <= (2 + length(ages))) return(
#'     moments[get('Nmoment') <= N_moments]
#'   )
#'   
#'   message(sprintf("Too many moments. Maximum number of moments possible: %s",
#'                   (2 + length(ages)))
#'   )
#'   
#'   return(moments)
#'   
#' }
#' 
