#' Import wealth survey data
#' @importFrom stringi %s+%
#' @importFrom haven read_sas
#' @import data.table
#' @importFrom magrittr %>%
#' 
#' @param path Path for wealth survey
#' @param tabname SAS table name
#' @param create_education Logical value indicating whether we
#'  should create an education variable
#' @export

read_wealth <- function(path = "./inst/dataINSEE/Enquete Patrimoine",
            tabname = "basecomplete0_62663.sas7bdat",
            create_education = FALSE){

  # Import data
  # -----------------------------------------------------
  
  data_patri <- haven::read_sas(paste0(path, "/", tabname))
  
  
  # Keep label column names correspondance somewhere
  # -----------------------------------------------------
  
  data_patri <- data.table::as.data.table(data_patri)
  
  
  # Transform factor variables rather than characters
  # -----------------------------------------------------
  
  data_patri[data_patri == ""] <- NA
  
  changeCols <- names(Filter(is.character, data_patri))
  data_patri[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]
  
  return(data_patri)
}




#' Read wealth survey to prepare moment computations
#' 
#' @param macro Macroeconomic table
#' @param path_data Directory where data are stored
#' @param level Analysis level. Default to 'household'
#' @param year Survey year
#' @param .colsWealth If needed, columns to keep. If
#'  \code{NULL}, alls columns are kept
#' @return Wealth survey
#' @export

read_EP <- function(macro,
                    path_data = getwd(),
                    level = c('household','individual'),
                    year = 2015,
                    .colsWealth = NULL){
  
  level <- match.arg(level)
  subdir <- sprintf("GEN_A1635%s0_DFPRPATRISAS", year-2001)
  # subdir <- ifelse(year == 2015,
  #                  yes = "GEN_A1635140_DFPRPATRISAS",
  #                  no = "Insee_HVP2017-Livraison3_BDF")
  lev <- ifelse(level == 'household',
                yes = 'MENAGE',
                no = 'INDIVIDU')
  
  # PROBLEM WITH LINUX SCRIPT
  if ((.Platform$OS.type == "unix") && year == 2018) lev <- tolower(lev)
  
  # CREATE PATH VARIABLE
  path_data <- sprintf("%s/Enquete Patrimoine/%s/%s.sas7bdat",
                       path_data, subdir,lev)
  
  # READ DATA
  EP_data <- haven::read_sas(path_data)
  EP_data <- data.table::data.table(EP_data)
  
  # KEEP ONLY SOME COLUMNS  
  if (!is.null(EP_data) && !is.null(.colsWealth)) EP_data <-  EP_data[,.SD,.SDcols = .colsWealth]
  
  if (level != 'household') return(EP_data)
  
  # /!\ cette division nest plus d'actualité car elle intervient plus tard
  EP_data[,`:=` ('w' = get('PATFISOM'),#/get('NPERS'),
                 'annee' = year)]
  EP_data <- merge(EP_data, macro, by = 'annee')
  EP_data[,'w_real' := get('w')/get('Prix')]
  
  return(EP_data)
}
