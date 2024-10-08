#' Plot MMB
#'
#' Plot MMB trajectory from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param plot_ci T/F Add confidence interval ribbon to MMB.
#' @param ci_alpha Alpha value for confidence interval, a = 0.05 is 95% CI. Default = 0.05.
#' @param yrs NULL. Subset a specific year range, example: c(1990:2022)
#' @param plot_proj T/F Add point to plot for projection year MMB. Default = T
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.
#' @param std_file File path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
#' @param std_list Output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.

#' @return Plots of mature biomass and abundance
#' @examples gmacs_plot_mmb(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_mmb <- function(all_out = NULL, save_plot = T, plot_dir = NULL, plot_ci = F, ci_alpha = 0.05, yrs = NULL, plot_proj = T, data_summary = NULL,
                           file = NULL, model_name = NULL, version = NULL, std_file = NULL, std_list = NULL) {

  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_derived_quantity_summary(all_out, file, model_name, version)}

  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_ssb", par)) %>%
      transmute(ssb_se = se / (1 / exp(est)),
                ssb_lci = exp(est) + ssb_se * qnorm(ci_alpha / 2),
                ssb_uci = exp(est) + ssb_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(ssb_se = NA, ssb_lci = NA, ssb_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}
  # add line for projection year
  if(plot_proj == T){
    proj <- gmacs_get_ref_points(all_out) %>%
      mutate(year = max(data_summary$year) + 1)
  }

  # plot ssb
  data_summary %>%
    ggplot()+
    {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = ssb_lci, ymax = ssb_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
    geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
    {if(plot_proj == T){geom_point(data = proj,
                                   aes(x = factor(year), y = mmb, color = model))}}+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    scale_fill_manual(values = cbpalette)+
    labs(x = NULL, y = paste0("MMB (", unique(data_summary$wt_units), ")"), color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mmb
  # plot ssa
  data_summary %>%
    ggplot()+
    geom_line(aes(x = factor(year), y = ssa, group = model, color = model))+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL,
         y = ifelse(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones"),
                    "MMA", paste0("MMA (", unique(data_summary$n_units), ")")),
         color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mma
  if(save_plot == T){
    ggsave(file.path(plot_dir, "mmb_trajectory.png"), plot = mmb, height = 4.2, width = 7, units = "in")
    ggsave(file.path(plot_dir, "mma_trajectory.png"), plot = mma, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F){return(list(mmb, mma))}

}
