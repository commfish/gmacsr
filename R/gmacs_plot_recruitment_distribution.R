#' Plot Recruitment Distribution
#'
#' Plot recruitment distribution from gmacs_get_recruitment_distribution()
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param n_rec_class Number of recruitment classes as list with first element vector for males, second element vector for females.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of recruitment distribution.
#' @examples gmacs_plot_recruitment_distribution(all_out = list(model_25.0), n_rec_class = 5)
#'
#' @export
#'
gmacs_plot_recruitment_distribution <- function(all_out = NULL, save_plot = T, plot_dir = NULL, n_rec_class = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_recruitment_distribution(all_out, file, model_name, version, n_rec_class = n_rec_class)}

  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    ggplot()+
    geom_line(aes(x = size, y = rec_dist, color = model))+
    labs(x = size_lab, y = "Recruitment Proportion", color = NULL)+
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
    scale_color_manual(values = cbpalette)+
    {if(length(unique(data_summary$sex)) > 1) {facet_wrap(~sex, ncol = 1)}} -> p
  if(save_plot == T){
    ggsave(file.path(plot_dir, "recruitment_distribution.png"), plot = p, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F) {return(p)}
}
