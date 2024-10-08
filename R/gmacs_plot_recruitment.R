#' Plot Recruitment
#'
#' Plot recruitment trajectory from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param plot_ci T/F Add confidence interval ribbon to MMB.
#' @param ci_alpha Alpha value for confidence interval, a = 0.05 is 95% CI. Default = 0.05.
#' @param yrs NULL. Subset a specific year range, example: c(1990:2022)
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.
#' @param std_file File path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
#' @param std_list Output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.

#' @return Plot of recruitment trajectory
#' @examples gmacs_plot_recruitment(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_recruitment <- function(all_out = NULL, save_plot = T, plot_dir = NULL, plot_ci = F, ci_alpha = 0.05, yrs = NULL, data_summary = NULL, file = NULL, model_name = NULL, std_file = NULL, std_list = NULL, version = NULL) {

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
      filter(grepl("sd_log_recruits", par)) %>%
      transmute(rec_se = se / (1 / exp(est)),
                rec_lci = exp(est) + rec_se * qnorm(ci_alpha / 2),
                rec_uci = exp(est) + rec_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(rec_se = NA, rec_lci = NA, rec_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}

  # male and female
  if("recruit_female" %in% names(data_summary)) {

    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- c("Total Recruitment", "Female Recruitment", "Male Recruitment")} else{
      y_labs <- paste0(c("Total Recruitment", "Female Recruitment", "Male Recruitment"), " (", unique(data_summary$n_units), ")")
    }
    data_summary %>%
      mutate(tot_recruit = recruit_male + recruit_female) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = tot_recruit, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = y_labs[1], color = NULL)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> tot
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_female, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[2], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> fem
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[3], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal

    if(save_plot == T){
      ggsave(file.path(plot_dir, "total_recruitment.png"), plot = tot, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "female_recruitment.png"), plot = fem, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "male_recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = c(tot, fem, mal)}
  }
  # only male
  if(!("recruit_female" %in% names(data_summary))) {
    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- "Recruitment"} else{
      if(is.na(data_summary$n_units)[1] == TRUE){y_labs <- "Recruitment"} else{
        y_labs <- paste0("Recruitment", " (", unique(data_summary$n_units), ")")}
    }
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = rec_lci, ymax = rec_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(NA, NA))+
      labs(x = NULL, y = y_labs, color = NULL)+
      scale_color_manual(values = cbpalette)+
      {if(plot_ci == T){scale_fill_manual(values = cbpalette)}}+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal

    if(save_plot == T){
      ggsave(file.path(plot_dir, "recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = mal}
  }
  if(save_plot == F){plots} else{"done"}

}
