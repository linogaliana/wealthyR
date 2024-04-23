#' @inheritParams moment_pic
#' @param age_var_survey Age variable in wealth survey
#' @param wealth_var_survey Wealth variable in wealth survey
#' @param age_var_simulations Age variable in microsimulated data
#' @param wealth_var_simulations Wealth variable in microsimulated data
#' @export

plot_moment_pic <- function(EP_2015, EP_2018,
                            simulations,
                            survey_years = c(2015L, 2018L),
                            age_var_survey = "AGEPR",
                            wealth_var_survey = "PATFISOM",
                            age_var_simulations = "age",
                            wealth_var_simulations = "wealth",
                            normalize = FALSE,
                            N_moments = 3L,
                            scale = "level",
                            exclude_negative = FALSE,
                            by = NULL){
  
  # scale1 <- ifelse(N_moments <= 2, yes = "log", no = scale)
  
  list_survey <- list(EP_2015, EP_2018)
  
  moments_observed <- lapply(seq_len(length(list_survey)), function(i){
    
    moment1 <- moment_pic(list_survey[[i]],
                          wealth_var = wealth_var_survey, 
                          age_var = age_var_survey,
                          moment_var = "moment",
                          normalize = normalize, 
                          scale = scale,
                          survey_year = survey_years[i],
                          exclude_negative = exclude_negative,
                          by = by)
    moment1[, 'year' := survey_years[i]]
    
  })
  
  moments_observed <- data.table::rbindlist(
    moments_observed
  )
  moments_observed[,'source' := "survey"]                            
  
  
  moment_simulation <- lapply(2009:2025, function(yy){
    df <- moment_pic(simulations, wealth_var = wealth_var_simulations, 
               moment_var = "moment",
               age_var = age_var_simulations,
               normalize = normalize, 
               scale = scale,
               survey_year = yy,
               exclude_negative = exclude_negative, by = by)
    df[, 'year' := yy]  
  })
  moment_simulation <- data.table::rbindlist(moment_simulation)
  moment_simulation[,'source' := "simulation"]                            
  
  moments_age <- data.table::rbindlist(list(
    moments_observed,
    moment_simulation), use.names = TRUE, fill = TRUE
  )
  moments_age[, 'Nmoment' := NULL]
  
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = moments_age,
                        ggplot2::aes(x = year,
                                     y = moment,
                                     shape = source,
                                     color = source)) +
    ggplot2::geom_line(data = moments_age[source=='simulation'],
                       ggplot2::aes(x = year,
                                    y = moment,
                                    color = source)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = "Year", y = "Moment value")

  if (!is.null(by)) p <- p + ggplot2::facet_wrap(by)
  
  return(p)
}
