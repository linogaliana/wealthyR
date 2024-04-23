
#' Alternative function to read wealth data
#' 
#' @inheritParams read_wealth
#' @inheritParams read_EP
#' @param path_survey Directory where data are stored
#' @param filename_survey Filename for survey data
#' @param level_matching Should we use individual or household
#'  data
#' @param id_var Identifier variable
#' @param age_var Age variable
#' @param wealth_var Wealth variable
#' @param findet_var End of studying year variable name
#' @importFrom stringr str_sub

read_wealth2 <- function(path_survey = "./inst/dataINSEE",
                         year = c(NULL,2009,2015),
                         filename_survey = "basecomplete0_62663.sas7bdat",
                         level_matching = c("household","individual"),
                         id_var = 'IDENTMEN',
                         age_var = 'AGEPR',
                         wealth_var = 'PATFISOM',
                         findet_var =  "AGFINETU",
                         .colsWealth = c('IDENTIND','IDENTMEN','age','AGEPR','POND','PATRI_NET','PATRI_BRUT',
                                         'PATFI','PATFIMTC_DECL','PATFISOM','PATIMM',
                                         'PATPROFENT','PATPROFHENT','PATRIC_DECL','AGFINETU')
){
  
  if (missing(level_matching)) level_matching <- "household"
  if (missing(year)) level_matching <- NULL
  
  
  if (!is.null(year)){
    if (year==2009){
      directory <- paste0(path_survey,"/GEN_A1635100_DDIFASAS/")
    } else{
      directory <- paste0(path_survey,"/GEN_A1635140_DFPRPATRISAS/")
    }
  } else{
    # Year not provided: we use 'Enquete Patrimoine' directory name
    directory <- paste0(path_survey,"/Enquete Patrimoine/")
  }
  
  
  
  # IMPORT HOUSEHOLD WEALTH SURVEY
  # ----------------------------------
  
  if (endsWith(filename_survey,"sas7bdat")){
    df <- haven::read_sas(paste0(directory,filename_survey))
    df <- data.table::data.table(df)
  }else{
    df <- data.table::fread(paste0(path_survey,"/Enquete Patrimoine/",filename_survey))
  }
  
  # KEEP ONLY HOUSEHOLD HEAD INFORMATION: IDENTIND = IDENT || NOI
  # --------------------------------------------------------------------
  
  if (level_matching == 'individual'){
    # Extract NOI
    df[,'NOI' := as.numeric(stringr::str_sub(get("IDENTIND"),start = -2))]
    
    # Head: minimal NOI
    df[, 'minNOI' := min(get('NOI'),na.rm=TRUE), , by = id_var]
    df <- df[get('NOI') == get('minNOI')]
  }
  
  # KEEP RELEVENT INFORMATION
  df <- if (!is.null(.colsWealth)) df[,.SD,
                                      .SDcols = .colsWealth] else df[,.SD,
                                                                     .SDcols = c(id_var,
                                                                                 age_var,
                                                                                 wealth_var,
                                                                                 findet_var)]
  df <- unique(df)
  
  return(df)
}
