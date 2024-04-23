label_moments <- function(N_moments = 26L,
                          by = NULL,
                          data = NULL,
                          agevar = "AGEPR_2015",
                          scale = "log",
                          ages = c(35,50),
                          quantiles = c(0.1,0.5,0.9),
                          survey_years = c(2015,2018),
                          stats = c("mean","sd"),
                          select_moments = NULL){
  
  data <- data[get(agevar) %between% ages]
  
  # FIRST MOMENT: WEALTH ACME ---------------------------
  
  # scale1 <-  ifelse(N_moments<=2, yes = "log",
  #                   no = scale)
  
  if (!is.null(by) && !is.na(by[1])){
    m1 <- "Richesse médiane par âge"
  } else{
    m1 <- "Acme de la richesse médiane par âge"
  }
  
  moment1 <- data.frame(
    label = m1,
    Nmoment = 1L,
    scale = scale,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  # SECOND MOMENT: WEALTH GROWTH PANEL ---------------------------
  
  
  # scale2 <-  ifelse(N_moments<=2, yes = "level",
  #                   no = scale)
  
  if (!is.null(by) && !is.null(data) && !is.na(by[2])){
    moment2 <- data.frame(
      label = paste0("Médiane de la variation de la richesse individuelle",
                     unique(data[[by]])
      ),
      Nmoment = seq(
        max(moment1$Nmoment) + 1,
        max(moment1$Nmoment) + length(unique(data[[by]]))
      ),
      scale = scale,
      year = paste0(survey_years[1],"-",survey_years[2]),
      champs = paste0(min(ages), "-", max(ages))
    )
  } else{
    moment2 <- data.frame(
      label = "Médiane de la variation de la richesse individuelle",
      Nmoment = min(moment1$Nmoment) + 1,
      scale = scale,
      year = paste0(survey_years[1],"-",survey_years[2]),
      champs = paste0(min(ages), "-", max(ages))
    )
  }
  
  
  
  moments <- data.table::rbindlist(list(moment1,moment2))
  
  # THIRD MOMENT: WEALTH ACME 2018 ---------------------------
  
  moment1bis <- data.frame(
    label = "Pic de la richesse médiane",
    Nmoment = max(moments$Nmoment) + 1,
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  
  moments <- data.table::rbindlist(list(moments,moment1bis))
  
  # 4th-?? MOMENTS: CROSS SECTIONAL MOMENTS 2015  --------------------------
  
  moments_wave1 <- data.frame(
    label = paste0("Richesse en coupe: ", stats),
    Nmoment = max(moments$Nmoment) + 1:length(stats),
    scale = scale,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave1))
  
  
  # ??-?? MOMENTS: MEAN AND SD (OR OTHER STATS) FOR dK BETWEEN 2015-2018 -------
  
  moments_distribution_dK <- data.frame(
    label = paste0("Variation de richesse entre deux vagues de panel: ", stats),
    Nmoment = max(moments$Nmoment) + 1:length(stats),
    scale = scale,
    year = paste0(survey_years[1],"-",survey_years[2]),
    champs = paste0(min(ages), "-", max(ages))
  )
  
  
  # ??-?? MOMENTS: CROSS SECTIONAL MOMENTS 2018  --------------------------
  
  moments_wave2 <- data.frame(
    label = paste0("Richesse en coupe: ", stats),
    Nmoment = max(moments$Nmoment) + 1:length(stats),
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave2))
  
  
  # ??-?? WEALTH BY AGE (2015):  -------------
  
  ages2 <- c(20,80)
  
  moments_age <- data.frame(
    label =  paste0("Richesse médiane tranche d'age (",
                    seq(min(ages2), max(ages2), by = 5),
                    "-",
                    seq(min(ages2), max(ages2), by = 5)+5,
                    "ans)"),
    Nmoment = max(moments$Nmoment) + 1:length(seq(min(ages2), max(ages2), by = 5)),
    scale = scale,
    year = survey_years[1],
    champs = paste0(seq(min(ages2), max(ages2), by = 5),
                    "-",
                    seq(min(ages2), max(ages2), by = 5)+5,
                    "ans")
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_age))
  
  
  # ??-?? WEALTH BY AGE (2018):  -------------
  
  moments_age2 <- data.frame(
    label =  paste0("Richesse médiane tranche d'age (",
                    seq(min(ages2), max(ages2), by = 5),
                    "-",
                    seq(min(ages2), max(ages2), by = 5)+5,
                    "ans)"),
    Nmoment = max(moments$Nmoment) + 1:length(seq(min(ages2), max(ages2), by = 5)),
    scale = scale,
    year = survey_years[2],
    champs = paste0(seq(min(ages2), max(ages2), by = 5),
                    "-",
                    seq(min(ages2), max(ages2), by = 5)+5,
                    "ans")
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_age2))
  
  
  # ??-?? MOMENTS: CROSS SECTIONAL QUANTILES ---------------------------
  
  moments_wave1_quantile <- data.frame(
    label = paste0("Richesse en coupe: quantile ", quantiles),
    Nmoment = max(moments$Nmoment) + 1:length(quantiles),
    scale = scale,
    year = survey_years[1],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave1_quantile))
  
  
  
  
  moments_wave2_quantile <- data.frame(
    label = paste0("Richesse en coupe: quantile ", quantiles),
    Nmoment = max(moments$Nmoment) + 1:length(quantiles),
    scale = scale,
    year = survey_years[2],
    champs = "Toute population"
  )
  
  moments <- data.table::rbindlist(list(moments,
                                        moments_wave2_quantile))
  
  
  
  
  moments <- moments[get('Nmoment') <= N_moments]
  if (!is.null(select_moments)) {
    moments <- moments[get("Nmoment") %in% select_moments]
  }
  
  return(moments)
}


