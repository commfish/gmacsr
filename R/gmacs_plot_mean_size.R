#' Plot Fit to Mean Size Comp Data
#'
#' Plot mean size based on fits to size data from Gmacsall.out data summary
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param size_lab Optional, custom size axis label, as character vector, example: "Carapace Length (mm)". Default = "Size".
#' @param agg_series T/F Plot aggregate series together. Default = T.
#' @param agg_series_label character vector of labels for aggregate series, ex: c("Male", "Female") or c("New Shell", "Old Shell") or list with elements being character vectors for each aggregate series (if you want to use different labels)
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Plot of fit to mean size data by series
#' @examples gmacs_plot_mean_size(list(bbrkc), save_plot = T, size_lab = "CL",  agg_series = F)
#'
#' @export
#'
gmacs_plot_mean_size <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", agg_series = T, agg_series_label = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {

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
    nest_by(model, mod_series, .keep = T) %>% ungroup %>% #pull(data) %>% .[[4]] -> data
    mutate(plot = purrr::map2(data, model, function(data, model) {

      # determine if comp is aggregated
      agg <- ifelse(sum(data %>% pull(aggregate_series) %>% is.na()) > 0, F, T)

      if(agg == F){

        data %>%
          group_by(year) %>%
          summarise(obs_mean_size = weighted.mean(size, obs),
                    pred_mean_size = weighted.mean(size, pred),
                    sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
                    l95 = pred_mean_size + sd * qnorm(0.025),
                    u95 = pred_mean_size + sd * qnorm(0.975),
                    sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
                    l95_est = pred_mean_size + sd_est * qnorm(0.025),
                    u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
          ggplot()+
          geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
          geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
          geom_line(aes(x = year, y = pred_mean_size, group = 1))+
          geom_point(aes(x = year, y = obs_mean_size))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
          labs(x = NULL, y = paste0("Mean ", size_lab)) -> p

      }
      if(agg == T){

        if(is.null(agg_series_label)) {agg_series_label <- unique(data$aggregate_series)}

        data %>%
          mutate(aggregate_series = factor(agg_series_label[aggregate_series], levels = agg_series_label)) %>%
          group_by(year, aggregate_series) %>%
          summarise(obs_mean_size = weighted.mean(size, obs),
                    pred_mean_size = weighted.mean(size, pred),
                    sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
                    l95 = pred_mean_size + sd * qnorm(0.025),
                    u95 = pred_mean_size + sd * qnorm(0.975),
                    sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
                    l95_est = pred_mean_size + sd_est * qnorm(0.025),
                    u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
          ggplot()+
          geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)+
          geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
          geom_line(aes(x = year, y = pred_mean_size, group = 1))+
          geom_point(aes(x = year, y = obs_mean_size))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
          labs(x = NULL, y = paste0("Mean ", size_lab))+
          facet_wrap(~aggregate_series, ncol = 1) -> p
      }

      ### save
      if(save_plot == T){
        height = ifelse(agg == T, length(agg_series_label) * 3.6, 3.6)
        width = 6
        ggsave(file.path(plot_dir, paste0("model_", model, "_mean_size_series_", unique(data$mod_series),".png")),
               plot = p, height = height, width = width, units = "in")
      }

      return(p)

    } )) -> out

  if(save_plot == T){return("done")}else{out$plot}

}

