% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmacs_plot_mmb.R
\name{gmacs_plot_mmb}
\alias{gmacs_plot_mmb}
\title{Plot MMB}
\usage{
gmacs_plot_mmb(
  all_out = NULL,
  save_plot = T,
  plot_dir = NULL,
  plot_ci = F,
  ci_alpha = 0.05,
  yrs = NULL,
  plot_proj = T,
  data_summary = NULL,
  file = NULL,
  model_name = NULL,
  version = NULL,
  std_file = NULL,
  std_list = NULL
)
}
\arguments{
\item{all_out}{Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).}

\item{save_plot}{T/F save plot. Default = T.}

\item{plot_dir}{Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.}

\item{plot_ci}{T/F Add confidence interval ribbon to MMB.}

\item{ci_alpha}{Alpha value for confidence interval, a = 0.05 is 95\% CI. Default = 0.05.}

\item{yrs}{NULL. Subset a specific year range, example: c(1990:2022)}

\item{plot_proj}{T/F Add point to plot for projection year MMB. Default = T}

\item{data_summary}{NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()}

\item{file}{NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{model_name}{NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.}

\item{version}{NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.}

\item{std_file}{File path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.}

\item{std_list}{Output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.}
}
\value{
Plots of mature biomass and abundance
}
\description{
Plot MMB trajectory from Gmacsall.out data summary
}
\examples{
gmacs_plot_mmb(all_out = list(model_25.0))

}
