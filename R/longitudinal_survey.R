#' Create longitudinal wealth survey at household level
#' 
#' @inheritParams read_EP
#' @param EP_2015,EP_2018 Wealth surveys (temporary
#'  fix for the function to run)
#' @return 2015 and 2018 longitudinal wealth survey
#' @export

longitudinal_survey <- function(macro, EP_2015,
                                EP_2018, path_data){
  
  # READ 2015 DATA
  # ----------------------
  
  EP_2015_indiv <- read_EP(macro,
                           path_data = path_data,
                           year = 2015,
                           .colsWealth = c('IDENTINDL','IDENT'),
                           level = 'individual')
  

  # READ 2018 DATA
  # ----------------------
  
  EP_2018_indiv <- read_EP(macro,
                           path_data = path_data,
                           year = 2018,
                           .colsWealth = c('IDENTINDL','IDENT'),
                           level = 'individual')
  

  # RENAME VARIABLES BEFORE MERGING
  # -----------------------------------
  
  data.table::setnames(EP_2018_indiv,
                       old='IDENT',
                       new = 'IDENT_2018')
  
  data.table::setnames(EP_2015_indiv,
                       old= colnames(EP_2015_indiv)[colnames(EP_2015_indiv) != 'IDENTINDL'],
                       new = paste0(colnames(EP_2015_indiv)[colnames(EP_2015_indiv) != 'IDENTINDL'],
                                    "_2015")
  )
  
  # MERGE TO GET LONGITUDINAL HOUSEHOLDS
  # -----------------------------------------
  
  EP_longitudinal <- merge(EP_2015_indiv, EP_2018_indiv,
                           by = 'IDENTINDL')
  
  
  # REFORMAT DATA
  # -----------------------
  
  
  data.table::setnames(EP_2015, old = 'IDENT', new = 'IDENT_2015')
  data.table::setnames(EP_2018, old = 'IDENT', new = 'IDENT_2018')
  
  
  EP_2015_lon <- merge(EP_2015,EP_longitudinal,
                       by = 'IDENT_2015')
  
  EP_lon <- merge(EP_2015_lon, EP_2018,
                  by = c('IDENT_2018'),
                  suffixes = c("_2015", "_2018"))
  
  

  EP_lon[, 'nth' := row.names(.SD), by = c('IDENT_2018', 'IDENT_2015')]
  
  EP_lon <- EP_lon[get('nth')==1]
  
  if ('SEXE_2015' %in% colnames(EP_lon)){
    EP_lon[,'SEXE' := get('SEXE_2015')]
  } else if ('SEXE_2018' %in% colnames(EP_lon)){
    EP_lon[,'SEXE' := get('SEXE_2018')]
  }
  
  if ('AGFINETU_2015' %in% colnames(EP_lon)){
    EP_lon[,'AGFINETU' := get('AGFINETU_2015')]
  } else if ('AGFINETU_2018' %in% colnames(EP_lon)){
    EP_lon[,'AGFINETU' := get('AGFINETU_2018')]
  }
  
  
  return(EP_lon)
}
