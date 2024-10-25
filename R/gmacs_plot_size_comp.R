#' Plot Fit to Size Comp Data
#'
#' Plot fits to catch data from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param add_n T/F Add observed multinomial sample size. Default = T.
#' @param add_n_est T/F Add estimated multinomial sample size. Default = T.
#' @param agg_series T/F Plot aggregate series together. Default = T.
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of fit to catch data by series
#' @examples gmacs_plot_catch(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_size_comp <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", add_n = T, add_n_est = T, agg_series = T, data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # get size summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name, version)}


  if(agg_series == T){

    # aggregate plots ----

    data_summary %>%
      group_by(model, mod_series, aggregate_series, size) %>%
      summarise()

    # sample size notation
    if(add_n == T & add_n_est == F){data_summary %>% mutate(n_note = paste0("N = ", nsamp_obs))}
    if(add_n == F & add_n_est == T){data_summary %>% mutate(n_note = paste0("N est = ", nsamp_est))}
    if(add_n == T & add_n_est == T){data_summary %>% mutate(n_note = paste0("N = ", nsamp_obs, "\nN est = ", nsamp_est))}
    if(add_n == F & add_n_est == F){data_summary %>% mutate(n_note = NA)}



  }





}

