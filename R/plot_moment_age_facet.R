#' @export

plot_moment_age_facet <- function(EP_2015, EP_2018, simulations,
                                  by_survey = "AGE", by_simulation = "age",
                                  scale = "log",
                                  facets_vars = NULL){
  
  if (is.null(facets_vars)){
    return(
      list(
        wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                                  by_survey = by_survey, by_simulation = by_simulation, scale = scale)$fit[[1]],
        wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                                  by_survey = by_survey, by_simulation = by_simulation, scale = scale)$fit[[2]]
      )
    )
  }  
  
  
  EP_lon_list <- split(EP_lon, by = facets_vars)
  simulations_list <- split(simulations, by = facets_vars)
  
  tempdf <- lapply(
    intersect(names(EP_lon_list), names(simulations_list)),
    function(group){
      d <- wealthyR::plot_moment_age(EP_2015, EP_2018, simulations = simulations,
                                     by_survey = by_survey, by_simulation = by_simulation, scale = scale,
                                     plot = FALSE)
      d[, 'facet' := group]
      return(d)
    }
  )
  
  tempdf <- data.table::rbindlist(tempdf, fill = TRUE, use.names = TRUE)
  
  ggplot2::ggplot(tempdf) +
    ggplot2::geom_bar(ggplot2::aes(x = age, y = moment,
                                   fill = source), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::facet_wrap(~year) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Age", y = "Median wealth") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::facet_wrap("facet")
    
}


