#' Plot Aggregated Fit to Size Comp Data
#'
#' Plot fits to size data from Gmacsall.out data summary aggregated over all years by series
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param add_n T/F Add observed multinomial sample size. Default = T.
#' @param add_n_est T/F Add estimated multinomial sample size. Default = T.
#' @param agg_series T/F Plot aggregate series together. Default = T.
#' @param agg_series_label character vector of labels for aggregate series, ex: c("Male", "Female") or c("New Shell", "Old Shell") or list with elements being character vectors for each aggregate series (if you want to use different labels)
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of fit to catch data by series
#' @examples gmacs_plot_size_comp_aggregate(list(bbrkc), save_plot = T,size_lab = "CL", add_n = F, add_n_est = T, agg_series = F)
#'
#' @export
#'
gmacs_plot_size_comp_aggregate <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", add_n = T, add_n_est = T, agg_series = T, agg_series_label = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # get size summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name, version)}

  # combine sample sizes of aggregate series'
  data_summary %>%
    group_by(model, mod_series, year, size) %>%
    mutate(nsamp_obs = sum(nsamp_obs),
           nsamp_est = sum(nsamp_est)) %>% ungroup -> data_summary

  data_summary %>%
    group_by(model, org_series, mod_series, aggregate_series, size) %>%
    summarise(obs = sum(obs),
              pred = sum(pred),
              nsamp_obs = sum(nsamp_obs),
              nsamp_est = sum(nsamp_est)) %>% ungroup -> data_summary

  # sample size notation
  if(add_n == T & add_n_est == F){data_summary <- data_summary %>% mutate(n_note = paste0("N = ", prettyNum(nsamp_obs, big.mark = ",")))}
  if(add_n == F & add_n_est == T){data_summary <- data_summary %>% mutate(n_note = paste0("N est = ", prettyNum(round(nsamp_est), big.mark = ",")))}
  if(add_n == T & add_n_est == T){data_summary <- data_summary %>% mutate(n_note = paste0("N = ", prettyNum(nsamp_obs, big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est), big.mark = ",")))}
  if(add_n == F & add_n_est == F){data_summary <- data_summary %>% mutate(n_note = NA)}

  if(agg_series == F){
    data_summary %>%
      group_by(org_series, mod_series, aggregate_series) %>%
      nest %>% ungroup %>%
      dplyr::select(-mod_series) %>%
      rowid_to_column(var = "mod_series") %>%
      unnest() %>%
      mutate(aggregate_series = NA) -> data_summary
    }
  data_summary %>%
    nest_by(mod_series, .keep = T) %>% ungroup %>% #pull(data) %>% .[[4]] -> data
    mutate(plot = purrr::map(data, function(data) {

      # determine if comp is aggregated
      agg <- ifelse(sum(data %>% pull(aggregate_series) %>% is.na()) > 0, F, T)

      # get some detail about size bins
      size_bins <- data %>% pull(size) %>% unique
      n_bins <- length(size_bins)
      n_yr <- length(unique(data$year))
      bin_width <- size_bins[2] - size_bins[1]

      if(agg == F){
        ### plot
        data %>%
          mutate(obs = ifelse(obs == 0, NA, obs),
                 pred = ifelse(pred == 0, NA, pred)) %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model))+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = n_note),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p

      }
      if(agg == T){

        # adjust size bin for the secondary series
        data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size)
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        data %>%
          distinct(aggregate_series, plot_size) %>%
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(agg_series_label)) {agg_series_label <- unique(data$aggregate_series)}

        ### plot
        data %>%
          mutate(obs = ifelse(obs == 0, NA, obs),
                 pred = ifelse(pred == 0, NA, pred)) %>%
          mutate(agg_series_label = factor(agg_series_label[aggregate_series], levels = agg_series_label)) %>%

          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = agg_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = n_note),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p
      }

      ### save
      if(save_plot == T){
        ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_series_", unique(data$mod_series),".png")),
               plot = p, height = 3.6, width = 6, units = "in")
      }

      return(p)

    } )) -> out

  if(save_plot == T){return("done")}else{out$plot}

}

