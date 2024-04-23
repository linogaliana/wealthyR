#' Prepare sample for inheritance analysis
#' 
#' @param path_survey Path to survey data
#' @param year Survey year

prepare_inheritance_sample <- function(path_survey = "../../../capitulation/inst/dataINSEE/Enquete Patrimoine",
                                  year = 2015){
  
  # ----------------------------------------------------------------
  #     LOAD DATA (TO EXTRACT HOUSEHOLD HEAD AGE)
  # ----------------------------------------------------------------
  
  if (year == 2015){
    wealth_survey_id <- "IDENT"  
  } else{
    wealth_survey_id <- "IDENTMEN"  
  }
  
  
  # LOAD INDIVIDUAL LEVEL DATA
  # --------------------------------
  # Annee 2009: GEN_A1635100_DDIFASAS
  # Annee 2015: GEN_A1635140_DFPRPATRISAS
  
  if (year==2009){
    directory <- paste0(path_survey,"/GEN_A1635100_DDIFASAS/")
  } else{
    directory <- paste0(path_survey, sprintf("/GEN_A1635%s0_DFPRPATRISAS", year-2001))
  }
  path_complete <- paste0(directory,"/INDIVIDU.sas7bdat")
  
  # READ SAS INDIVIDUAL TABLE
  data_patri <- haven::read_sas(path_complete)
  
  
  if (year==2009){
    cols_to_keep <- c(wealth_survey_id,
                      "IDENTIND",
                      "POND",
                      "NOI",
                      "SEXE",
                      "ANAIS",
                      "AGE",
                      "AGFINETU",
                      "ZSALAIRES_I",
                      "ZRETRAITES_I",
                      "ZCHOMAGE_I")
  } else{
    cols_to_keep <- c(wealth_survey_id,
                      "IDENTIND14",
                      "IDENTINDL",
                      "POND",
                      "NOI",
                      "SEXE",
                      "ANAIS",
                      "AGE",
                      "DATEFORA",
                      "ZSALAIRES_I",
                      "ZRETRAITES_I",
                      "ZCHOMAGE_I", "MER1E", "PER1E")
  }
  
  # GET IT AS DATA.TABLE
  data_patri <- data.table::as.data.table(data_patri)
  data_patri <- data_patri[,.SD,
                           .SDcols = cols_to_keep]

  
  # DEDUCE FINDET FOR 2015 DATA
  # --------------------------------
  
  if (year==2015){
    data_patri[,'AGFINETU' := get('DATEFORA') - get('ANAIS')]
  }

  # CREATE A DECILE VARIABLE
  # ---------------------------------
  
  data_patri[,'income' := get('ZSALAIRES_I') +  get('ZRETRAITES_I') +  get('ZCHOMAGE_I')]


    
  data_patri[,'income_group' := cut(get('income'),
                          breaks = quantile(get('income'),
                                            c(0, 0.5, 6:10/10)
                                            ),
                          labels = c("bottom50", paste0("D",6:10)), right = FALSE)]
    
  # LOAD INHERITANCE TABLE
  # ---------------------------------
  
  
  df_transmission <- 
    read_inheritance(path_survey = path_survey,
                     year = year)
  
  
  EP_inheritance <- df_transmission[!is.na(get('MTHER'))]
  
  EP_inheritance[, 'source' := 'inheritance']
  
  # MERGE WITH INDIVIDUAL LEVEL INFORMATION
  # ------------------------------------------
  
  EP_completed <- merge(EP_inheritance,
                        data_patri,
                        by = intersect(names(EP_inheritance), names(data_patri)),
                        all.y = TRUE)
  
  
  EP_completed[,'inherited' := !is.na(get("MTHER"))]

  EP_completed[,'MTHER' := factor(get('MTHER'),
                                   ordered = TRUE)]
  
  # REMOVE PEOPLE WHOSE PARENTS DID NOT DIE
  return(
    EP_completed#[(isTRUE(get("inherited")) | (get('PER1E') == 3) & (get('MER1E')  == 3))]
  )
}
