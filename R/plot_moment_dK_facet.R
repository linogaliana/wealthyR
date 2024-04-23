#' @export

plot_moment_dK_facet <- function(EP_lon, simulations, scale = "log", xaxis = "tr_age_2015",
                                 facets_vars = NULL){
  
  if (is.null(facets_vars)){
    return(
      plot_moment_dK(
        EP_lon = EP_lon, simulations = simulations,
        scale = scale, by = xaxis, plot = TRUE
      )
    )
  }
  
  
  EP_lon_list <- split(EP_lon, by = facets_vars)
  simulations_list <- split(simulations, by = facets_vars)
  
  # ARRANGE NAMES
  names(EP_lon_list) <- as.character(
    lapply(strsplit(names(EP_lon_list), split = "\\."),
           function(vec) paste(
             paste0(facets_vars, ":" , as.character(vec)),
             collapse = "; ")
    )
  )
  names(simulations_list) <- as.character(
    lapply(strsplit(names(simulations_list), split = "\\."),
           function(vec) paste(
             paste0(facets_vars, ":" , as.character(vec)),
             collapse = "; ")
    )
  )
  
  
  tempdf <- lapply(
    intersect(names(EP_lon_list), names(simulations_list)),
    function(group){
      d <- plot_moment_dK(
        EP_lon = EP_lon_list[[group]], simulations = simulations_list[[group]],
        scale = scale, by = xaxis, plot = FALSE
      )
      d[, 'facet' := group]
      return(d)
    }
  )
  
  tempdf <- data.table::rbindlist(tempdf, fill = TRUE, use.names = TRUE)
  
  
  ggplot2::ggplot(tempdf, ggplot2::aes(x = label, y = moment,
                                       color = source,
                                       shape = source)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0L) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(title = "3-year rolling growth rate",
                  x = 'Age in wave 1',
                  y = "Moment") +
    ggplot2::facet_wrap("facet")
  
}

