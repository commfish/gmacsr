#' Plot Length Composition Residuals
#'
#' Plot length composition one-step-ahead (OSA) from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_index_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Residual plots by series
#' @examples gmacs_plot_osa_residuals(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_osa_residuals <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name, version)}

  # plots
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  data_summary %>%
    nest_by(across(intersect(names(.), c("mod_series", "aggregate_series"))), .keep = T) %>% ungroup %>%# pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map(data, function(data) {
      # qq plot
      data %>%
        filter(!is.na(osa_residual)) %>%
        mutate(theor_q = stats::qqnorm(osa_residual, plot.it = FALSE)$x) %>%
        ggplot()+
        geom_abline()+
        geom_point(aes(x = theor_q, y = osa_residual, color = factor(size)))+
        scale_color_manual(values = cbpalette, guide = guide_legend(ncol = 2))+
        labs(color = NULL, x = "Theoretical Quantiles", y = "Sample quantiles")+
        theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> qq_plot
      # dot plot
      data %>%
        filter(!is.na(osa_residual)) %>%
        mutate(sign = ifelse(osa_residual > 0, "+", "-")) %>%
        ggplot()+
        geom_point(aes(x = factor(year), y = size, size = abs(osa_residual), fill = sign), shape = 21)+
        scale_fill_manual(values = c("white", "grey30"))+
        labs(x = NULL, y = size_lab, fill = NULL, size = "abs(Residual)")+
        scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels) -> dot_plot
      # combine
      p <- qq_plot + dot_plot

      # file name
      if(save_plot == T){
      if("aggregate_series" %in% names(data)) {file_name = paste0("osa_residuals_series_", unique(data$mod_series),"_", unique(data$aggregate_series), ".png")}
      if(!("aggregate_series" %in% names(data))) {file_name = paste0("osa_residuals_series_", unique(data$mod_series), ".png")}
      ggsave(file.path(plot_dir, file_name),
             plot = p, height = 4, width = 10, units = "in")
      }

      return(p)

    })) -> plots

  # return ----
  if(save_plot == T) {return("done")
  } else {return(plots$plot)}

}
