
#' Read inheritance data
#' 
#' @param path_survey Path for survey data
#' @param year Panel wave year
#' 
#' @importFrom lest case_when
#' @export

read_inheritance <- function(path_survey = "./inst/dataINSEE/Enquete Patrimoine",
                             year = 2009){
  
  #Avoid NSE notes
  macro<-NULL
  
  if (year==2009){
    directory <- paste0(path_survey,"/GEN_A1635100_DDIFASAS/")
  } else{
    directory <- paste0(path_survey,"/GEN_A1635140_DFPRPATRISAS/")
  }
  path_complete <- paste0(directory,"/TRANSMISSION.sas7bdat")
  
  
  df_transmission <- haven::read_sas(path_complete)
  df_transmission <- data.table::as.data.table(df_transmission)
  
  if (year == 2009){
    cols_to_keep <- c("IDENTMEN",
      "IDENTPOS",
      "IDENTTRANS",
      "TRANSNA",
      "POND",
      "NOP",
      "ANNEE",
      "MTDONRC","MTDONR",
      "MTDONVC", "MTDONV",
      "MTHERC","MTHER",
      "TRANSQUA_MOB",
      "TRANSQUA_ARG","TRANSQUA_AVI",
      "VALEUR")    
  } else{
    cols_to_keep <- c("IDENT",
                      "IDENTPOS",
                      "IDENTTRANS",
                      "TRANSNA",
                      "POND",
                      "NOP",
                      "ANNEE",
                      "MTDONRC","MTDONR",
                      "MTDONVC", "MTDONV",
                      "MTHERC","MTHER",
                      "TRANSQUA_MOB",
                      "TRANSQUA_ARG","TRANSQUA_AVI",
                      "VALEUR")       
  }
  
  df_transmission <- df_transmission[,.SD,
                                     .SDcols = cols_to_keep]
  
  macro <- capitulation::macro
  
  macro <- data.table::as.data.table(macro)[,.SD,.SDcols = c('annee','Prix')]
  
  data.table::setnames(df_transmission,
                       old = "ANNEE",
                       new = "annee")
  
  df_transmission[df_transmission == ""] <- NA
  
  changeCols<- names(Filter(is.character, df_transmission))
  
  df_transmission[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]
  
  df_transmission <- merge(df_transmission, macro)
  
  
  df_transmission[, `:=` ('MTDONRC_real' = lest::case_when(
    get('VALEUR') == 1 ~ get('MTDONRC')/get('Prix'),
    TRUE ~ get('MTDONRC')
  ),
  'MTDONVC_real' = lest::case_when(
    get('VALEUR') == 1 ~ get('MTDONVC')/get('Prix'),
    TRUE ~ get('MTDONVC')
  )
  )]
  
  
}


