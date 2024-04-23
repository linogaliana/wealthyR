#' @export

individualize_EP <- function(path_data, year = 2015,
                             individualize_income = FALSE){

  macro <- capitulation::macro

  cols_wealth <- c('PATRI_NET','PATRI_BRUT',
                   'PATFI',
                   'PATFISOM','PATIMM',
                   'PATPROFENT','PATPROFHENT',
                   "MTDETTES")

  income_var <- c("ZSALAIRES_I",  "ZRETRAITES_I", "ZCHOMAGE_I")

  individual_characteristics <- c("AGE","SEXE","DATEFORA","ANAIS","SITUA")
  
  pondvar <- ifelse(year == 2015, "POND", "POND_TRANS")

  individual_characteristics <- c("AGE","SEXE","DATEFORA","ANAIS","SITUA",
                                  pondvar)

  EP_2015_indiv <- read_EP(macro,
                           path_data = path_data,
                           year = year,
                           level = "individual",
                           .colsWealth = c("IDENT", sprintf("IDENTIND%s", year - 2001),
                                           "IDENTINDL",
                                           income_var, individual_characteristics,
                                           "LIENPREF")
  )

  EP_2015_indiv[,'AGFINETU' := get('DATEFORA') - get('ANAIS')]
  EP_2015_indiv[, c('DATEFORA', 'ANAIS') := NULL]

  EP_2015_indiv <- EP_2015_indiv[grepl("(00|01)",get('LIENPREF'))]
  EP_2015_indiv[,'N_adulte' := .N, by = "IDENT"]

  if (isTRUE(individualize_income)) cols_wealth <- c(cols_wealth,
                                                     "ZSALAIRES",  "ZRETRAITES", "ZCHOMAGE")

  EP_2015_menages <- read_EP(macro,
                             path_data = path_data,
                             year = year,
                             level = "household",
                             .colsWealth = c('IDENT','AGEPR',
                                             cols_wealth,
                                             'NBUC','NPERS'))


  data_indiv <- merge(EP_2015_indiv, EP_2015_menages,
                      by = "IDENT")
  cols_wealth2 <- cols_wealth
  if ('w' %in% colnames(data_indiv)) cols_wealth2 <- c(cols_wealth2, "w")
  if ('w_real' %in% colnames(data_indiv)) cols_wealth2 <- c(cols_wealth2, "w_real")

  data_indiv[,c(cols_wealth2) := lapply(.SD, function(d) as.numeric(d)/as.numeric(get("N_adulte"))),
             .SDcols = cols_wealth2]

  data_indiv[,'IDENTINDL' := NULL]

  return(data_indiv)
}

