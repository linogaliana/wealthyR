#' Plot moment related to capital growth
#' 
#' @inheritParams moment_variation_K
#' @param simulations Microsimulated data
#' @export
plot_moment_dK <- function(
  EP_lon,
  simulations,
  N_moments = 3L,
  scale = "log",
  statfunc = "median",
  deltaK_var = "dW",
  age_2015_var = "AGE_2015",
  ages = c(30, 65),
  normalize = FALSE,
  survey_years = c(2015, 2018),
  exclude_negative = FALSE,
  by = NULL,
  plot = TRUE,
  label_observed = "survey",
  label_simulated = "microsimulation"
){
  
  # scale2 <- ifelse(N_moments <= 2, yes = "level", no = scale)
  
  
  moment_dK1 <- moment_variation_K(EP_lon = EP_lon,
                                   format = "wide", 
                                   statfunc = statfunc,
                                   deltaK_var = deltaK_var,
                                   moment_var = "moment", 
                                   age_2015_var = age_2015_var,
                                   ages = ages,
                                   stat = "difference", 
                                   normalize = normalize,
                                   scale_variable = scale,
                                   survey_years = survey_years, 
                                   exclude_negative = exclude_negative,
                                   by = by,
                                   add_label=TRUE)
  
  moment_dK1[,'endyear' := 2018]
  moment_dK1[,'source' := label_observed]
  
  
  # moment_dK2 <- lapply(2009:2023, function(firsty){
  moment_dK2 <- moment_variation_K(EP_lon = simulations, format = "long", 
                              statfunc = "median",
                              deltaK_var = deltaK_var, moment_var = "moment", 
                              age_2015_var = "age",
                              ages = ages,
                              stat = "difference", 
                              normalize = normalize,
                              scale_variable = scale,
                              survey_years = survey_years, 
                              # survey_years = c(firsty, firsty + 3), 
                              exclude_negative = exclude_negative,
                              by = by,
                              add_label=TRUE)
  #   return(moment_dK2)
  # })
  # moment_dK2 <- data.table::rbindlist(moment_dK2)
  # moment_dK2[,'endyear' := (2009:2023) + 3]
  moment_dK2[,'endyear' := 2018]
  moment_dK2[,'source' := label_simulated]
  
  moment_dK <- data.table::rbindlist(
    list(moment_dK1, moment_dK2),
    use.names = TRUE, fill = TRUE
  )
  
  moment_dK <- moment_dK[order(label)]
  
  if (isFALSE(plot)) return(moment_dK)
  
  # p <- ggplot2::ggplot(moment_dK, ggplot2::aes(x = endyear, y = moment,
  #                                              color = source,
  #                                              shape = source)) +
  #   ggplot2::geom_line() +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_hline(yintercept = 0L) +
  #   ggplot2::scale_color_viridis_d() +
  #   ggplot2::labs(x = "3-year rolling growth rate",
  #                 y = "Moment")
  
  p <- ggplot2::ggplot(moment_dK, ggplot2::aes(x = label,
                                               y = as.numeric(moment),
                                               color = source,
                                               shape = source,
                                               group = source)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L) +
    ggplot2::labs(title = "3-year rolling growth rate",
                  x = 'Age in wave 1',
                  y = "Moment")
  
  return(p)
}
