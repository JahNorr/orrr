% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_dir.R
\name{dir.project.exists}
\alias{dir.project.exists}
\title{Project Directory Exists}
\usage{
dir.project.exists(names, create = F)
}
\arguments{
\item{names}{character vector of folder names in a branch}

\item{create}{logical create folders if missing?}
}
\value{
logical - does the folder exist
}
\description{
Returns whether or not the folder exists, and optionally creates the path (recursively) if missing
}
\examples{
\dontrun{
dir.project.exists("data")
}
}
