% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmacs_do_exe.R
\name{gmacs_do_exe}
\alias{gmacs_do_exe}
\title{Run gmacs.exe}
\usage{
gmacs_do_exe(
  gmacs.dat,
  pin = F,
  wait = T,
  reweight = F,
  level = 0.001,
  max_iter = 100,
  reweight_only = F,
  jitter_rseed = NULL
)
}
\arguments{
\item{gmacs.dat}{File path to gmacs.dat file}

\item{pin}{T/F use pin file. Default = F.}

\item{wait}{Passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.}

\item{reweight}{T/F tune Francis weights. Default = F.}

\item{level}{Level of convergence for Francis weights. Default = 0.001.}

\item{max_iter}{Maximum iterations for tuning Francis weights. Default = 100.}

\item{reweight_only}{T/F do initial GMACS run (F) or skip to iterative reweighting (T).}

\item{jitter_rseed}{Use random number seed from jitter.txt file to match previous jittering run.}
}
\value{
Run GMACS, standard GMACS output files.
}
\description{
Run gmacs.exe and do iterative reweighing of Francis weights
}
\examples{
gmacs_do_exe(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat")

}
