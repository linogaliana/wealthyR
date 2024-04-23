#' @export
plot_moment_age_multiple <- function(EP_2015,
                                     EP_2018,
                                     simulations,
                                     wealth_var_survey = "PATFISOM",
                                     age_var_survey = "AGEPR",
                                     ages = c(20,80),
                                     normalize = TRUE,
                                     scale = "log",
                                     survey_years = c(2015,2018)
){
  
  wealth_surveys <- list(EP_2015, EP_2018)
  
  moment_data <- lapply(1:length(wealth_surveys), function(i) {
    
    df <- moment_age_multiple(wealth_surveys[[i]],
                                         wealth_var = wealth_var_survey, 
                                         ages = ages,
                                         age_var = age_var_survey,
                                         moment_var = "moment", 
                                         normalize = normalize,
                                         scale = scale,
                                         moments_before = 1L, 
                                         survey_year = survey_years[i])
    
    df[,'year' := survey_years[i]]
  })
  
  
  moment_data = data.table::rbindlist(moment_data)
  moment_data[,'source' := "survey"]
  
  moment_simulation <- lapply(c(2015, 2018), function(y){
    df <- moment_age_multiple(
      simulations, wealth_var = "wealth", 
      ages = c(20, 80), age_var = "age",
      moment_var = "moment", 
      normalize = TRUE, scale = scale,
      moments_before = 1L, 
      survey_year = y)
    df[,'year' := y]
  })
  moment_simulation <- data.table::rbindlist(moment_simulation)
  moment_simulation[,'source' := "simulation"]
  
  moments_age <- data.table::rbindlist(
    list(moment_data, moment_simulation),
    use.names = TRUE
  )
  
  moments_age[, 'age' := seq(min(ages), max(ages), by = 5) + 2.5,
              by = c("source","year")]
  
  
  p <- ggplot2::ggplot(moments_age) +
    ggplot2::geom_line(ggplot2::aes(x = age, y = moment,
                                    color = source)) +
    ggplot2::geom_point(ggplot2::aes(x = age, y = moment,
                                     color = source, shape = source)) +
    ggplot2::facet_wrap(~year) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(x = "Age", y = "Median wealth")
 
  return(p) 
}
