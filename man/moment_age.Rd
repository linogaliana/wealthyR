% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moment_age.R
\name{moment_age}
\alias{moment_age}
\title{Determine age where wealth is at its acme}
\usage{
moment_age(
  EP_data,
  ages = c(35, 50),
  wealth_var = "PATFISOM",
  age_var = "AGEPR",
  moment_var = "moment_data",
  weight_var = "POND",
  moments_before = 2,
  normalize = TRUE,
  scale_variable = c("level", "log", "asinh"),
  scale_moment = c("level", "log", "asinh"),
  survey_year = 2015,
  by = NULL
)
}
\arguments{
\item{EP_data}{Wealth survey data or microsimulated data}

\item{ages}{Thresholds (lower, upper) that should be considered
as sample}

\item{wealth_var}{Wealth variable name}

\item{age_var}{Age variable}

\item{moment_var}{Variable name}

\item{moments_before}{Number of moments computed before}

\item{normalize}{Logical value indicating whether we want
to normalize wealth variable to get variance equal to 1}

\item{survey_year}{Year that should be used to compute moments}

\item{scale}{Should values be considered in \emph{level} (default) or \emph{log}}
}
\value{
A \link[data.table]{data.table} with 1 row and 2 columns
\describe{
  \item{\code{moment_var}}{Median varia tion of wealth, given \code{stat} and a \code{scale} arguments}
  \item{Nmoment}{Column equal to \code{moments_before}+1: moment number}
}
}
\description{
Determine age where wealth is at its acme
}
