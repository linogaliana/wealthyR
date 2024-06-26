% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moment_variation_K.R
\name{moment_variation_K}
\alias{moment_variation_K}
\title{Compute wealth evolution between two wealth survey waves}
\usage{
moment_variation_K(
  EP_lon,
  format = c("wide", "long"),
  by = NULL,
  wealth_var_prefix = "w_real",
  statfunc = "median",
  wealth_var = "wealth",
  deltaK_var = "dW",
  moment_var = "moment_data",
  age_2015_var = "AGEPR_2015",
  weight_var = "POND",
  ages = c(30, 65),
  survey_years = c(2015, 2018),
  stat = c("proportion", "difference", "growth"),
  normalize = TRUE,
  scale_variable = c("level", "log", "asinh"),
  exclude_negative = FALSE,
  min_moment = 1L,
  add_label = FALSE,
  return_N = FALSE,
  ...
)
}
\arguments{
\item{EP_lon}{Longitudinal data, either microsimulated or
wealth survey created by \link{longitudinal_survey}}

\item{format}{Data structure. Either \emph{wide} (default)
or \emph{long}}

\item{statfunc}{Statistical function (moment, quantile, etc. ) that should be used}

\item{wealth_var}{Wealth variable}

\item{deltaK_var}{Name for the growth variable that is temporarily created}

\item{moment_var}{Name for the moment variable that is created}

\item{age_2015_var}{Age variable storing age at first occurrence}

\item{ages}{Ages considered to restrict sample}

\item{survey_years}{Collection years for the two survey waves}

\item{stat}{Should we collect $W_{1}/W_{0}$ (\emph{proportion}, default)
or $W_{1}-W_{0}$ (\emph{difference})}

\item{normalize}{Logical value indicating whether values are normalized
to compare distributions with variance equal to 1}

\item{exclude_negative}{Should we exclude negative values? Default to \code{FALSE}}

\item{min_moment}{Moment numbering from which that output should start}

\item{...}{Additional argument that should be provided to \code{statfunc}}

\item{scale}{Should values be considered in \emph{level} (default) or \emph{log}}
}
\value{
A \link[data.table]{data.table} with 1 row and 2 columns
\describe{
  \item{\code{moment_var}}{Median variation of wealth, given \code{stat} and a \code{scale} arguments}
  \item{Nmoment}{Column equal to 2: second moment}
}
}
\description{
Use panel wealth data to derive median wealth growth over 3 years
}
