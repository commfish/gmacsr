#' Plot Summary of Fits to Size Composition
#'
#' Plot summary of fits to size data from Gmacsall.out data summary
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

#' @return Plot of aggregated fit to size composition data and OSA residuals by series
#' @examples gmacs_plot_size_fit_summary(list(bbrkc), save_plot = T,size_lab = "CL", add_n = F, add_n_est = T, agg_series = F)
#'
#' @export
#'
#'
gmacs_plot_size_fit_summary <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", add_n = T, add_n_est = T, agg_series = T, agg_series_label = NULL, data_summary = NULL, file = NULL, model_name = NULL, version = NULL) {

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # get size summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name, version)}

  # add aggregate series if missing
  if(!("aggregate_series" %in% names(data_summary))) {data_summary$aggregate_series <- NA}

  # make plot
  data_summary %>%
    nest_by(across(intersect(names(.), c("mod_series", "aggregate_series"))), .keep = T) %>% ungroup %>%# pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map(data, function(data) {

      # dot plot
      data %>%
        filter(!is.na(osa_residual)) %>%
        mutate(sign = ifelse(osa_residual > 0, "+", "-")) %>%
        ggplot()+
        geom_point(aes(x = factor(year), y = size, size = abs(osa_residual), fill = sign), shape = 21)+
        scale_fill_manual(values = c("white", "grey30"))+
        labs(x = NULL, y = size_lab, fill = NULL, size = "abs(Residual)")+
        scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels) -> dot_plot
      # qq plot
      data %>%
        filter(!is.na(osa_residual)) %>%
        mutate(theor_q = stats::qqnorm(osa_residual, plot.it = FALSE)$x) %>%
        ggplot()+
        geom_abline()+
        geom_point(aes(x = theor_q, y = osa_residual, color = factor(size)))+
        scale_color_manual(values = cbpalette, guide = guide_legend(ncol = 2))+
        labs(color = NULL, x = "Theoretical Quantiles", y = "Sample quantiles") -> qq_plot

      # get some detail about size bins
      size_bins <- data %>% pull(size) %>% unique
      n_bins <- length(size_bins)
      n_yr <- length(unique(data$year))
      bin_width <- size_bins[2] - size_bins[1]


      # combine sample sizes of aggregate series'
      data %>%
        group_by(model, mod_series, aggregate_series, year, size) %>%
        mutate(nsamp_obs = sum(nsamp_obs),
               nsamp_est = sum(nsamp_est)) %>% ungroup -> agg_comp_data

      # sum across years
      agg_comp_data %>%
        group_by(model, org_series, mod_series, aggregate_series, size) %>%
        summarise(obs = sum(obs),
                  pred = sum(pred),
                  nsamp_obs = sum(nsamp_obs),
                  nsamp_est = sum(nsamp_est)) %>% ungroup -> agg_comp_data

      # sample size notation
      if(add_n == T & add_n_est == F){agg_comp_data <- agg_comp_data %>% mutate(n_note = paste0("N = ", prettyNum(nsamp_obs, big.mark = ",")))}
      if(add_n == F & add_n_est == T){agg_comp_data <- agg_comp_data %>% mutate(n_note = paste0("N est = ", prettyNum(round(nsamp_est), big.mark = ",")))}
      if(add_n == T & add_n_est == T){agg_comp_data <- agg_comp_data %>% mutate(n_note = paste0("N = ", prettyNum(nsamp_obs, big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est), big.mark = ",")))}
      if(add_n == F & add_n_est == F){agg_comp_data <- agg_comp_data %>% mutate(n_note = NA)}

      # determine if comp is aggregated
      agg <- ifelse(sum(agg_comp_data %>% pull(aggregate_series) %>% is.na()) > 0, F, T)

      # do plot
      if(agg == F){
        ### plot
        agg_comp_data %>%
          mutate(obs = ifelse(obs == 0, NA, obs),
                 pred = ifelse(pred == 0, NA, pred)) %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model), show.legend = F)+
          geom_point(aes(x = size, y = pred, color = model), show.legend = F)+
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
                panel.background = element_blank()) -> agg_plot

      }
      if(agg == T){

        # adjust size bin for the secondary series
        agg_comp_data <- mutate(agg_comp_data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size)
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        agg_comp_data %>%
          distinct(aggregate_series, plot_size) %>%
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        agg_comp_data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        agg_comp_data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(agg_series_label)) {agg_series_label <- unique(data$aggregate_series)}

        ### plot
        agg_comp_data %>%
          mutate(obs = ifelse(obs == 0, NA, obs),
                 pred = ifelse(pred == 0, NA, pred)) %>%
          mutate(agg_series_label = factor(agg_series_label[aggregate_series], levels = agg_series_label)) %>%

          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = agg_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model), show.legend = F)+
          geom_point(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model), show.legend = F)+
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
                panel.background = element_blank()) -> agg_plot
      }

      # combine plots
      plot <- dot_plot / qq_plot / agg_plot + plot_layout(heights = c(3, 1, 1))

      ### save
      if(save_plot == T){
        ggsave(file.path(plot_dir, paste0("size_comp_summary_series_", unique(data$mod_series),".png")),
               plot = plot, height = 9, width = 6, units = "in")
      }

      return(plot)
    })) -> out

    if(save_plot == T){return("done")}else{out$plot}

}
