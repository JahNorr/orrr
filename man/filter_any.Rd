% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_like.R
\name{filter_any}
\alias{filter_any}
\title{Keep rows that match any condition}
\usage{
filter_any(
  df,
  ...,
  patterns,
  invert = FALSE,
  fixed = FALSE,
  ignore.case = FALSE
)
}
\arguments{
\item{df}{A data frame, data frame extension (e.g. a tibble),}

\item{...}{<data-masking> A variable in .data}

\item{patterns}{A vector of patterns with which the variable will be compared with grepl}

\item{invert}{logical. If TRUE return rows for elements that do not match.}

\item{fixed}{logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments}

\item{ignore.case}{logical. if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.}
}
\value{
The filter_any() function is used to subset the rows of .data, applying the patterns to the variable (...) to determine which rows should be retained.
}
\description{
Keep rows that match any condition
}
\examples{

require(dplyr, quietly = TRUE, warn.conflicts = FALSE)

my_cars <- c("mazda", "Merc 2.*")
mtcars \%>\%
  mutate(car = row.names(.)) \%>\%
  filter_any(car, patterns = my_cars, ignore.case = T) \%>\%
  select(-car)

starwars \%>\% data.frame() \%>\%
  filter_any(species,patterns = c("human","droid"), ignore.case = T, invert = T) \%>\%
  select(name, species) \%>\%
  arrange(species, name)
}
