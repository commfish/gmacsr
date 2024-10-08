#' Plot Fit to Catch Data
#'
#' Plot fits to catch data from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param y_labs NULL. Optional, custom y axis labels as character vector.
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of fit to catch data by series
#' @examples gmacs_plot_catch(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_catch <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {

  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_catch_summary(all_out, file, model_name, version = version)}

  # plots

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  data_summary %>%
    nest_by(series, units, .keep = T) %>% ungroup %>%
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {

      # y label
      if(is.null(y_labs)) {
        y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$wt_units), ")")
        if(unique(data$units) == "Numbers") {
          y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$n_units), ")")
        }
      }

      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, units),
                               year = min(data$year):max(data$year)),
                   by = join_by(model, series, year, units)) %>%
        mutate(obs_l95 = obs_catch * exp(-1.96 * sqrt(log(1 + cv^2))),
               obs_u95 = obs_catch * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
        ggplot()+
        geom_point(aes(x = year, y = obs_catch), color = "grey40")+
        geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey40")+
        geom_line(aes(x = year, y = pred_catch, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p

      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks) -> p }

      if(save_plot == T) {

        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        # save plot
        ggsave(plot = p,
               filename = file.path(plot_dir, paste0("catch_fit_", tolower(unique(data$fleet)), "_", tolower(unique(data$units)), ".png")),
               width = pwidth,
               height = pwidth * (3/5), units = "in")
      }

      return(p)

    })) -> plots

  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2),
           filename = file.path(plot_dir, "catch_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in")

    return("done")

  } else {return(plots$plot)}

}
