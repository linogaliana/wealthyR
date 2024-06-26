% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_wealth2.R
\name{read_wealth2}
\alias{read_wealth2}
\title{Alternative function to read wealth data}
\usage{
read_wealth2(
  path_survey = "./inst/dataINSEE",
  year = c(NULL, 2009, 2015),
  filename_survey = "basecomplete0_62663.sas7bdat",
  level_matching = c("household", "individual"),
  id_var = "IDENTMEN",
  age_var = "AGEPR",
  wealth_var = "PATFISOM",
  findet_var = "AGFINETU",
  .colsWealth = c("IDENTIND", "IDENTMEN", "age", "AGEPR", "POND", "PATRI_NET",
    "PATRI_BRUT", "PATFI", "PATFIMTC_DECL", "PATFISOM", "PATIMM", "PATPROFENT",
    "PATPROFHENT", "PATRIC_DECL", "AGFINETU")
)
}
\arguments{
\item{path_survey}{Directory where data are stored}

\item{year}{Survey year}

\item{filename_survey}{Filename for survey data}

\item{level_matching}{Should we use individual or household
data}

\item{id_var}{Identifier variable}

\item{age_var}{Age variable}

\item{wealth_var}{Wealth variable}

\item{findet_var}{End of studying year variable name}

\item{.colsWealth}{If needed, columns to keep. If
\code{NULL}, alls columns are kept}
}
\description{
Alternative function to read wealth data
}
