#' @export
plot_moment_distribution_K <- function(EP_2015,
                                     EP_2018,
                                     simulations,
                                     wealth_var_survey = "PATFISOM",
                                     wealth_var_simulation = "wealth",
                                     normalize = FALSE,
                                     scale = "log",
                                     stats = c("mean","sd"),
                                     survey_years = c(2015,2018),
                                     exclude_negative = FALSE
){
  
  stats <- match.arg(stats)
  
  data_survey <- list(EP_2015, EP_2018)
  
  moment_survey <- lapply(1:length(data_survey), function(i){
    df <- moment_distribution_K(
      data_survey[[i]],
      stats = stats,
      wealth_var = wealth_var_survey,
      moment_var = "moment",
      normalize = normalize,
      scale = scale,
      exclude_negative = exclude_negative,
      survey_year = survey_years[i],
      min_moment = 3L)
    df[,'year' := survey_years[i]]
  })
  moment_survey <- data.table::rbindlist(moment_survey)
  moment_survey[,'source' := "survey"]
  
  simulations_split <- split(
    simulations[annee %between% c(2009,2023)],
    by = "annee"
  )

  moment_simulations <- lapply(1:length(simulations_split), function(i){
    df <- moment_distribution_K(
      simulations_split[[i]],
      stats = stats,
      wealth_var = wealth_var_simulation,
      moment_var = "moment",
      normalize = normalize,
      scale = scale,
      exclude_negative = exclude_negative,
      survey_year = (2009:2023)[i],
      min_moment = 3L)
    df[,'year' := (2009:2023)[i]]
  })
  moment_simulations <- data.table::rbindlist(moment_simulations)
  moment_simulations[,'source' := 'microsimulation']
  
  moments <- data.table::rbindlist(
    list(moment_survey,
        moment_simulations),
    use.names = TRUE
  )
  
  p <- ggplot2::ggplot(moments, ggplot2::aes(x = year, y = moment,
                                        color = source,
                                        shape = source)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = moments[source != "survey"]) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::facet_wrap(~m, scales = "free_y", nrow = 2) +
    ggplot2::labs(
      x = "Year", y = "Valeur du moment"
    )
    
  return(p)
}