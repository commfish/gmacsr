% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmacs_get_effective_n.R
\name{gmacs_get_effective_n}
\alias{gmacs_get_effective_n}
\title{Get Size Composition Effective Sample Size}
\usage{
gmacs_get_effective_n(
  all_out = NULL,
  file = NULL,
  model_name = NULL,
  version = NULL
)
}
\arguments{
\item{all_out}{Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).}

\item{file}{NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{model_name}{NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{version}{NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.}
}
\value{
Tibble of effective sample size summary from Gmacsall.out in long format with column 'model'.
}
\description{
Isolate size composition effective sample size summary from Gmacsall.out by model
}
\examples{
gmacs_get_effective_n(all_out = list(mod_23.0a, mod_23.1b))

}
