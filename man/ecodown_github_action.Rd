% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github-action.R
\name{ecodown_github_action}
\alias{ecodown_github_action}
\title{Copies a GHA file that automates the site re-build}
\usage{
ecodown_github_action(project_folder = ".")
}
\arguments{
\item{project_folder}{The location of the projects root folder}
}
\description{
Copies a GHA file that automates the site re-build
}
\details{
Copies a GHA file called \code{ecodown.yaml} that automates the package
cloning, document copying, reference creation and then commits the changes
back to the repo.
}
