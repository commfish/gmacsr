#' Plot Fishing Mortality
#'
#' Plot F by sex and fleet from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param yrs NULL. Subset a specific year range, example: c(1990:2022)
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of fishing mortality by sex and fleet
#' @examples gmacs_plot_f(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_f <- function(all_out = NULL, save_plot = T, plot_dir = NULL, yrs = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL){

  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_f(all_out, file, model_name, version)}

  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # plot f by fleet and sex ----

  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified
  data_summary %>%
    group_by(model, year, sex, fleet, n_sex) %>%
    summarize(f = sum(`F`)) %>% ungroup %>%
    mutate(fleet_name = gsub("_", " ", fleet),
           sex_name = str_to_title(sex)) %>%
    nest_by(sex_name, fleet_name, .keep = T) %>%
    mutate(ylab = ifelse(length(unique(data$sex_name)) > 1,
                         paste(unique(data$fleet_name), unique(data$sex_name), "F"),
                         paste(unique(data$fleet_name), "F"))) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(data, ylab, function(data, ylab) {

      data %>%
        ggplot()+
        geom_line(aes(x = factor(year), y = f, color = model, group = model))+
        labs(x = NULL, y = ylab, color = NULL)+
        scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_color_manual(values = cbpalette) -> x
      if(save_plot == T){
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        ggsave(file.path(plot_dir, paste0(tolower(gsub(" ", "_", ylab)), ".png")), plot = x,
               height = 0.6*pwidth, width = pwidth, units = "in")
      }
      return(x)
    })) -> by_fleet

  return(if(save_plot == T){"done"}else{by_fleet$plot})

}
