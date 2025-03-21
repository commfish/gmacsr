#' Plot Data Range
#'
#' Plot data rangey for a single model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of data range by type
#' @examples gmacs_plot_data_range(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_data_range <- function(all_out = NULL, save_plot = T, plot_dir = NULL, file = NULL, model_name = NULL, version = NULL) {
  # setup ----
  # read all out
  if(is.null(all_out)) {
    tibble(file = file,
           model_name = model_name,
           version = version) %>%
      mutate(ao = purrr::pmap(list(file, model_name, version), function(file, model_name, version) {gmacs_read_allout(file, model_name, version)})) -> ao
  }
  if(!is.null(all_out)) {
    tibble(ao = all_out,
           model_name = purrr::map_chr(ao, function(ao) {ao$model_name})) -> ao
  }

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # get data pieces and plot ----

  # catch
  ao %>%
    mutate(catch = purrr::map(ao, function(data){
      data$catch_fit_summary %>%
        mutate(type = gsub("All", "Total", type)) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex)

    }),
    index = purrr::map(ao, function(data){
      data$index_fit_summary %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, series, sex), paste(fleet, series)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(fleet, sex)
    }),
    size_composition = purrr::map(ao, function(data){
      data$size_fit_summary %>%
        mutate(type = str_to_title(gsub("All", "Total", type))) %>%
        rename(series = mod_series) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex)
    })) %>%
    transmute(model_name, catch, index, size_composition) %>%
    pivot_longer(2:4, names_to = "process", values_to = "data") %>%
    unnest(data) %>%
    mutate(process = factor(str_to_title(gsub("_", " ", process)), level = c("Catch", "Index", "Size Composition"))) %>%
    arrange(model_name, process, type, fleet, sex) %>%
    mutate(group = factor(group, levels = unique(group))) %>%
    nest_by(model_name) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(model_name, data, function(model_name, data) {
      data %>%
        filter(year > 1980) %>%
        ggplot()+
        geom_point(aes(y = group, x = year, color = fleet), shape = 15, size = 3.75, show.legend = F)+
        facet_wrap(~process, ncol = 1, scales = "free_y")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_discrete(limits = rev)+
        scale_color_manual(values = cbpalette)+
        labs(y = NULL, x = NULL)+
        theme(panel.border = element_blank(),
              axis.line = element_line()) -> p_dat

      if(save_plot == T) {
        ggsave(plot = p_dat,
               filename = file.path(plot_dir, paste0(model_name, "_data_range.png")),
               width = 7,
               height = 5, units = "in")
      }
      return(p_dat)
    })) -> out

  # output ----

  if(save_plot == T){ return("done")} else{return(transmute(out, model_name, plot))}
}
