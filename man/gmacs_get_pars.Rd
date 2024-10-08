% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmacs_get_pars.R
\name{gmacs_get_pars}
\alias{gmacs_get_pars}
\title{Get Parameter Estimates}
\usage{
gmacs_get_pars(all_out = NULL, file = NULL, model_name = NULL, version = NULL)
}
\arguments{
\item{all_out}{Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).}

\item{file}{NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{model_name}{NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{version}{NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.}
}
\value{
Tibble of parameter estimates from Gmacsall.out with column 'model'.
}
\description{
Isolate parameter estimates from Gmacsall.out by model
}
\examples{
gmacs_get_pars(all_out = list(mod_23.0a, mod_23.1b))

}
