#' @export

plot_moment_age <- function(
  EP_2015,
  EP_2018,
  simulations,
  ages = c(20,80),
  wealth_var_survey = "PATFISOM",
  wealth_var_simulation = "wealth",
  age_var_survey = 'AGEPR',
  age_var_simulations = "age",
  moments_before = 2,
  normalize = FALSE,
  scale_variable = c('log', 'level','asinh'),
  scale_moment = c('level', 'log'),
  survey_years = c(2015, 2018),
  scale_moment_share = c("level","share"),
  by_survey = NULL,
  by_simulation = NULL,
  plot = TRUE,
  label_observed = "survey",
  label_simulated = "microsimulation"){

  scale_variable <- match.arg(scale_variable)
  scale_moment <- match.arg(scale_moment)

  data_survey <- list(EP_2015, EP_2018)

  moment_survey <- lapply(1:length(data_survey), function(i){
    df <- moment_age(data_survey[[i]],
                     wealth_var = wealth_var_survey,
                     moments_before = 0L,
                     ages = ages,
                     age_var = age_var_survey,
                     normalize = normalize,
                     moment_var = "moment",
                     scale_variable = scale_variable,
                     scale_moment = scale_moment,
                     survey_year = survey_years[i],
                     by = by_survey,
                     weight_var = "unknownvar")
    if (scale_moment_share == "share"){
      df[, `:=`(c("moment"), get("moment")/sum(get("moment"),
                                                 na.rm = TRUE))]
    }
    df[,'year' := survey_years[i]]
    df[,'age' := min(ages) - 1 + seq_len(.N)]
  })


  moment_survey <- data.table::rbindlist(moment_survey)
  moment_survey[,'source' := label_observed]

  simulations_split <- split(
    simulations[annee %in% survey_years],
    by = "annee"
  )


  moment_simulations <- lapply(1:length(simulations_split), function(i){
    df <- moment_age(simulations_split[[i]],
                     moments_before = 0L,
                     wealth_var = wealth_var_simulation,
                     ages = ages,
                     age_var = age_var_simulations,
                     moment_var = "moment",
                     normalize = normalize,
                     scale_variable = scale_variable,
                     scale_moment = scale_moment,
                     survey_year = survey_years[i],
                     by = by_simulation,
                     weight_var = "unknownvar")
    if (scale_moment_share == "share"){
      df[, `:=`(c("moment"), get("moment")/sum(get("moment"),
                                                   na.rm = TRUE))]
    }
    df[,'year' := survey_years[i]]
    df[,'age' := min(ages) - 1 + seq_len(.N)]
  })

  moment_simulations <- data.table::rbindlist(moment_simulations)
  moment_simulations[,'source' := label_simulated]

  moments <- data.table::rbindlist(
    list(moment_survey,
         moment_simulations),
    use.names = TRUE
  )


  p <- ggplot2::ggplot(moments) +
    ggplot2::geom_bar(ggplot2::aes(x = age, y = moment,
                                    fill = source), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::facet_wrap(~year) +
    # ggplot2::scale_color_viridis_d() +
    # ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Age", y = "Median wealth") +
    ggplot2::theme(legend.position = "bottom")

  p2 <- ggplot2::ggplot(moments) +
    ggplot2::geom_line(ggplot2::aes(x = age, y = moment,
                                    color = source)) +
    ggplot2::geom_point(ggplot2::aes(x = age, y = moment,
                                     color = source, shape = source)) +
    ggplot2::facet_wrap(~year) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Age", y = "Median wealth")

  p3a <- ggplot2::ggplot(moments[(get('year') ==  survey_years[1])]) +
    ggplot2::geom_bar(ggplot2::aes(x = age, y = moment,
                                   fill = source), position = "dodge",
                      stat='identity', width=.5) +
    ggplot2::facet_wrap(~year) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Age", y = "Median wealth") +
    ggplot2::theme(legend.position = "top")

  p3b <- ggplot2::ggplot(moments[(get('source') == "microsimulation") & (get('year') == survey_years[1])]) +
    ggplot2::geom_bar(ggplot2::aes(x = age, y = weight), stat = 'identity') +
    ggplot2::geom_point(ggplot2::aes(x = age, y = weight), color = 'red') +
    ggplot2::labs(y = "Density", x = "Age") +
    ggplot2::scale_y_reverse()

  if (isFALSE(plot)) return(moments[(get('year') ==  survey_years[1])])

  return(list('bar' = p, 'line' = p2, "fit" = list(p3a, p3b)))
}

