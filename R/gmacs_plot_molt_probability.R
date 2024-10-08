#' Plot Molt Probability
#'
#' Plot molt probability from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of molt probability by sex, time block
#' @examples gmacs_plot_molt_probability(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_molt_probability <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL, version = NULL){

  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_molt_probability(all_out, file, model_name, version)}

  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # plot molt probability ----

  data_summary %>%
    distinct(model, sex, block, size, molt_probability, block) %>%
    nest_by(sex) %>% ungroup %>%
    mutate(plot = purrr::map2(sex, data, function(sex, data){

      data %>%
        filter(!is.na(block)) %>%
        ggplot()+
        geom_line(aes(x = size, y = molt_probability, color = model))+
        {if(length(unique(data$block)) > 1) {facet_wrap(~block, nrow = 1)}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Molt Probability", color = NULL) -> x

      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x,
               filename = file.path(plot_dir, paste0(sex, "_molt_probability.png")),
               height = length(unique(sex)) * 3, width = min(length(unique(data$block[!is.na(data$block)]))*4, 8), units = "in")
      }
      return(x)
    })) -> out

  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {out$plot}

}

