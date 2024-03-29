% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{ecodown_build}
\alias{ecodown_build}
\title{Prepare full Quarto site}
\usage{
ecodown_build(
  quarto_folder = here::here(),
  verbosity = c("summary", "verbose", "silent")
)
}
\arguments{
\item{quarto_folder}{Base target Quarto folder. Defaults to current workspace.}

\item{verbosity}{Level of messaging available during run time. Possible values
are 'verbose', 'summary', and 'silent'.}
}
\description{
Prepare full Quarto site
}
