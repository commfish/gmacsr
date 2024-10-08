#' Plot Natural Mortality
#'
#' Plot M by sex and size from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param by NULL. Vector of grouping variables. Example = c("year", "sex"). Optional, if NULL, the function will determine how M varies.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
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
gmacs_plot_m <- function(all_out = NULL, save_plot = T, plot_dir = NULL, by = NULL, size_lab = "Size", yrs = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL){

  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_m(all_out, file, model_name, version)}

  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # plot m ----

  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified

  if(is.null(by)){
    data_summary %>%
      distinct(M, .keep_all = T) %>%
      dplyr::select_if(function(x){length(unique(x)) > 1}) %>%
      dplyr::select(-M) %>% names -> by
  }
  by <- by[by!="model"]

  #plot dimensions
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(sex) %>% unique %>% length -> nsex
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(maturity) %>% unique %>% length -> nmat

  if(!("size" %in% by) & "year" %in% by) {

    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = M, group = model, color = model))+
      geom_point(data = function(x) filter(x, year == max(year)),
                 aes(x = factor(year), y = M, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = paste0("Natural mortality (M)"), color = NULL)+
      facet_wrap(by[by!="year"], ncol = 1, scales = "free_y") -> x

  }
  if("size" %in% by & "year" %in% by){

    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = factor(year), linetype = model))+
      labs(x = size_lab, y = "Natural Mortaity (M)", color = NULL, linetype = NULL)+
      facet_wrap(by[by != "size"]) -> x

  }
  if("size" %in% by & !("year" %in% by)){

    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = model))+
      labs(x = size_lab, y = "Natural Mortaity (M)", color = NULL)+
      facet_wrap(by[!(by %in% c("size", "year"))], ncol = 1) -> x

  }

  if(save_plot == T){
    ggsave(file.path(plot_dir, "natural_mortality.png"), plot = x, height = 4.2 * nsex * nmat, width = 7, units = "in")
  }

  return(if(save_plot == T){"done"}else{x})

}
