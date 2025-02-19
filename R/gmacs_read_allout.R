#' Read Gmacsall.out file
#'
#' Load Gmacsall.out file in R
#' @param file NULL. Path to Gmacsall.out file.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.17".

#' @return List of file contents and model results
#' @examples gmacs_read_allout(file = "./AIGKC/models/2024/may/EAG/23.1b/Gmacsall.out", model_name = "23.1b")
#'
#' @export
#'
gmacs_read_allout <- function(file, model_name = NULL, version = NULL) {

  # ggplot theme anticipating plotting later in the workflow
  theme_set(theme_sleek())

if(is.null(version)){version = "2.20.20"}

if(version == "2.01.M.10"){
  # setup ----
  # Suppress the NA message in the coercion to double
  options(warn = -1)

  # read text file
  allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  out <- list()

  # version ----
  out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
  # model name ----
  out$model_name <- model_name
  # stock ----
  out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[2,], collapse = " ", na.rm = T))
  # general info ----
  ## years
  out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
  out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
  last <- grep("Year_range", allout[,1]) # start saving last position in file
  ## number of seasons
  out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## number of fleets
  out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## fleet names
  out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
  ## n sexes
  out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n shell conidition
  out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n maturity states
  out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## units
  out$wt_units <-  gsub("Weightunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  out$n_units <- gsub("Numbersunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
            out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
            out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
            out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s",
            out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units

  # likelihoods by type ----

  # read lines individually
  catch = as.numeric(na.omit(as.numeric(allout[last + 2,]))); last <- last + 2
  index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1

  # coerce to tibble
  rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
    transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
    add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
    add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
    add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
    add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type

  # likelihoods by type and fleet ----
  ## catches
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 4,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 5,])))) %>%
    transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 5
  ## index
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 4
  ## size composition
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 4
  ## recruitment penalties
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 2
  ## tagging
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 4
  ## growth
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 2
  bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet



  # penalties ----

  tmp <- matrix(nrow = 12, ncol = 3)
  for(i in 1:12) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
  as_tibble(tmp) %>%
    mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Mdevs", "Rec_ini", "Rec_dev", "Sex_ratio",
                       "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                       "Seldevs")) %>%
    transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp

  out$penalties <- tmp
  last <- last + 14

  # parameters ----

  ## par tibble
  tmp <- matrix(ncol = 11, nrow = length((last + 2):(grep("#---", allout[,1])[1]-1)))
  for(i in 1:nrow(tmp)) {
    if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
      as.character(allout[last + 1 + i, 1:13]) %>%
        .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
    } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
  }
  as_tibble(tmp) %>%
    rename_all(~c("parameter_count", "parameter", "colon", "estimate", "phase", "lower_bound", "upper_bound",
                  "penalty", "gradient", "standard_error", "est_count")) %>%
    #janitor::clean_names() %>%
    mutate_at(c(1, 4:11), as.numeric) %>%
    dplyr::select(-colon) -> out$parameters; last <- grep("#---", allout[,1])[1]
  out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
  ## parameters at bounds
  out$parameters %>%
    mutate(range = upper_bound - lower_bound,
           status = ifelse(estimate < (lower_bound+range*0.01), 1,
                           ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
    filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds

  # max gradient ----
  out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)
  # reference points ----

  ## ref tibble
  tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- na.omit(as.numeric(allout[last + 1 + i,]))
  }
  as_tibble(tmp) %>%
    mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                   paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets),
                   paste0("OFL_", 1:out$n_fleets))) %>%
    transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
  out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
  out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
  out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
  out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
  out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
  out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
  out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
  last <- grep("#---", allout[,1])[2] - 1
  ## ref sigma
  allout[last,] %>% dplyr::select_if(~ !any(is.na(.))) %>%
    mutate_all(., function(x){gsub(";", "", x)}) %>%
    .[1,] %>% as.numeric() %>%na.omit() %>% as.numeric() -> out$ref_sigmaR
  names(out$ref_sigmaR) <- c("sigmaR", "weight")
  last <- grep("#---", allout[,1])[2]

  # overall summary ----

  tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+2+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~make.unique(as.character(allout[last+2,1:ncol(tmp)]))) %>%
    janitor::clean_names(.) %>% rename(year = number_year, log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
  # do some renaming

  if(out$n_sex == 2) {
    out$derived_quant_summary %>%
      rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
  }
  last <- grep("#---", allout[,1])[3]

  # mean wt ----

  ## add size bins
  out$size_bins <- as.numeric(na.omit(as.numeric(allout[last+2,])))
  ## number of bins
  out$n_size_bins <- length(out$size_bins)
  ## weight at size matrix
  tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_bins)+3)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    rename_all(~c("sex", "maturity", "year", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
    mutate(size = as.numeric(size)) -> out$wt_at_size
  last <- last + 3 + nrow(tmp)

  # maturity vector ----

  out$maturity_at_size_vector <- as.numeric(allout[last+1,1:length(out$size_bins)]); last <- grep("#---", allout[,1])[4]

  # catch fit summary ----

  ## catch summary
  tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-3)), ncol = 14)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:14])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m",
                  "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
  ## catch q
  tibble(series = unique(out$catch_fit_summary$series),
         log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]

  # index fix_summary ----

  ## index summary
  tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:13])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv",
                  "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
  ## sdnr_MAR_cpue
  tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3, 1:2])
  }
  out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]

  # size composition fit summary ----

  ## size composition fit summary
  ## get info first
  tmp <- matrix(ncol = 10, nrow = length((last+3):(grep("sdnr_MAR_lf", allout[,1])-2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity", "nsamp_obs")) %>%
    mutate_at(c(1:3, 5, 10), as.numeric) -> tmp

  ## get comps
  last <- last + 2 # set last to start where the data is
  tmp %>%
    nest_by(mod_series, .keep = T) %>% ungroup() %>%
    mutate(row = purrr::map_dbl(data, ~nrow(.)),
           row = lag(row),
           row = cumsum(ifelse(is.na(row), 0, row)) + last) %>%
    mutate(comps = purrr::map2(data, row, function(data, row) {

      comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
      for(i in 1:nrow(comp_tmp)) {
        comp_tmp[i,] <- as.numeric(allout[row + i, 11:ncol(allout)])
      }
      as_tibble(comp_tmp) %>%
        dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
      if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
      if(comp_agg == F){

        comp_tmp %>%
          rename_all(~c(paste0("obs_", out$size_bins[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_bins[1:(ncol(comp_tmp)/2)]))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit2) %>%
          transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      if(comp_agg == T){
        nobs <- ncol(comp_tmp)/2
        group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
        comp_tmp %>%
          rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
          transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }

      return(comp_out)

    })) %>% transmute(comps) %>% unnest -> out$size_fit_summary

  last <- grep("sdnr_MAR_lf", allout[,1])
  ## sdnr_MAR_lf
  tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+i, 1:2])
  }
  out$sdnr_MAR_lf <- tmp
  last <- grep("Francis_weights", allout[,1])
  ## francis weights
  out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[7]

  ## add stage two weights to fit summary
  out$size_fit_summary %>%
    mutate(lambda = out$francis_weights[.$mod_series],
           nsamp_est = exp(out$parameters$estimate[grepl("Log_vn_comp", out$parameters$parameter)][.$mod_series]) * nsamp_obs * lambda) -> out$size_fit_summary

  # selectivity ----

  ## selex
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
  ## retention
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
  ## discard
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_disc; last <- grep("Select_control", allout[,2])
  ## slx control
  as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
    mutate_all(as.numeric) %>%
    rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
                  "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
    # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
    group_by(gear, start_yr, end_yr) %>%
    mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
    transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control

  out$slx_control %>%
    mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
           type = ifelse(gear > 0, "capture", "retention")) %>%
    distinct(fleet, type, sex, start_yr, end_yr) %>%
    mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
           end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
           year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
    unnest(year) %>%
    mutate(block = paste(start_yr, "-", end_yr)) %>%
    dplyr::select(-start_yr, -end_yr) -> tmp
  slx_cap %>%
    left_join(tmp %>%
                filter(type == "capture") %>%
                transmute(year, fleet, sex, capture_block = block),
              by = join_by(year, sex, fleet)) %>%
    left_join(slx_ret %>%
                left_join(tmp %>%
                            filter(type == "retention") %>%
                            transmute(year, fleet, sex, ret_disc_block = block),
                          by = join_by(year, sex, fleet)),
              by = join_by(year, sex, fleet, size)) %>%
    left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
    transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
              slx_discard, ret_disc_block) -> out$selectivity
  last <- grep("#----", allout[,1])[8]

  # mortality ----

  ## M by season
  tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last + 3 + i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4

  ## M by sex-maturity-size class
  tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "maturity", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
    mutate_at(c(1, 3:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 2

  ## fully selected F by season, sex, fleet
  tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
    mutate_at(c(3:ncol(.)), as.numeric) %>%
    pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 2

  ## fully selected F by season, sex, fleet
  ## skip same as above
  last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 2

  ## F by sex, year, season, size
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 2
  out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size));

  ## Z by sex, year, maturity, season, size
  tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- grep("#---", allout[,1])[9]
  out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))

  # n matrix ----

  nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[9]+2):grep("#---", allout[,1])[10],], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))

  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+1+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_bins)) %>%
      mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
  }
  do.call("bind_rows", list_tmp) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) -> out$n_matrix; last <- grep("#---", allout[,1])[10]


  # growth ----

  ## molt probability
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_bins))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", out$size_bins)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
    mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)

  ## growth transition matrix
  gmats <- grep("#growth_matrix", allout[,1]); last <- gmats[1]
  gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(gmats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- gmats_names
  out$growth_transition <- list_tmp

  ## size transition matrix
  smats <- grep("#size_matrix", allout[,1]); last <- gmats[1]
  smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(smats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- smats_names
  out$size_transition <- list_tmp
  last <- grep("#---", allout[,1])[11]




  # reference points ----

  # combinations of seasons and fleets with Fs
  tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F

  ##reference points and ofl
  out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
  out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
  out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
  out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
  out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
  out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
  out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
  out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
  out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])

  # simple likelihood stuff ----
  max(c(length(unique(out$catch_fit_summary$series)),
        length(unique(out$index_fit_summary$series)),
        length(unique(out$size_fit_summary$series)))) -> cols
  tmp <- matrix(nrow = 5, ncol = cols)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[grep("nloglike", allout[,1])+i, 1:ncol(tmp)])
  }
  out$nloglike <- tmp
  out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
  out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))

  # objective function ----
  out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)

}
if(version == "2.20.14"){
  # setup ----
  # Suppress the NA message in the coercion to double
  options(warn = -1)

  # read text file
  allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  out <- list()

  # version ----
  out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
  # model name ----
  out$model_name <- model_name
  # stock ----
  out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[7,], collapse = " ", na.rm = T))
  # general info ----
  last <- 8
  ## units
  out$wt_units <-  gsub("Weightunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  out$n_units <- gsub("Numbersunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
            out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
            out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
            out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s",
            out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units
  ## years
  out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
  out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
  last <- grep("Year_range", allout[,1]) # start saving last position in file
  ## number of seasons
  out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## number of fleets
  out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## fleet names
  out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
  ## n sexes
  out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n shell conidition
  out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n maturity states
  out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## size structure
  out$n_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  out$max_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  out$size_breaks <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1
  out$size_mid_points <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1

  # likelihoods by type ----

  # read lines individually
  catch = as.numeric(na.omit(as.numeric(allout[last + 3,]))); last <- last + 3
  index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1

  # coerce to tibble
  rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
    transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
    add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
    add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
    add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
    add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type

  # likelihoods by type and fleet ----
  ## catches
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 5,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 6,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 7,])))) %>%
    transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 8
  ## index
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 5
  ## size composition
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 5
  ## recruitment penalties
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 3
  ## tagging
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 5
  ## growth
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 4
  bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet



  # penalties ----

  tmp <- matrix(nrow = 10, ncol = 3)
  for(i in 1:10) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
  as_tibble(tmp) %>%
    mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Rec_dev", "Sex_ratio",
                       "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                       "Seldevs")) %>%
    transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp

  out$penalties <- tmp
  last <- last + 14

  # parameters ----

  ## par tibble
  tmp <- matrix(ncol = 11, nrow = length((last + 1):(grep("#---", allout[,1])[1]-3)))
  for(i in 1:nrow(tmp)) {
    if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
      as.character(allout[last + 1 + i, 1:13]) %>%
        .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
    } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
  }
  # oddly spaced column on one par
  log_vn_lines <- grep("Log_vn_size", tmp[,2])
  bind_rows( as_tibble(tmp[1:(log_vn_lines[1]-1), 1:11]),
             as_tibble(tmp[log_vn_lines, c(1, 2, 4:11)]),
             as_tibble(tmp[(log_vn_lines[length(log_vn_lines)]+1):nrow(tmp), 1:11]) ) %>%
    rename_all(~c("parameter_count", "parameter", "estimate", "phase", "lower_bound", "upper_bound", "status",
                  "penalty", "gradient", "standard_error", "est_count")) %>%
    #janitor::clean_names() %>%
    mutate_at(c(1, 3:11), as.numeric) %>%
    dplyr::select(-status) -> out$parameters; last <- grep("#---", allout[,1])[1]
  out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
  ## parameters at bounds
  out$parameters %>%
    mutate(range = upper_bound - lower_bound,
           status = ifelse(estimate < (lower_bound+range*0.01), 1,
                           ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
    filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds

  # max gradient ----
  out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)

  # reference points ----

  ## ref tibble
  tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 5)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- na.omit(as.numeric(allout[last + 2 + i,]))
  }
  as_tibble(tmp) %>%
    mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                   paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets),
                   paste0("OFL_", 1:out$n_fleets))) %>%
    transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
  out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
  out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
  out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
  out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
  out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
  out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
  out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
  last <- grep("#---", allout[,1])[2] - 2
  ## ref sigma
  allout[last:(last+1),] %>% dplyr::select_if(~ !any(is.na(.))) %>%
    mutate_all(., function(x){gsub(";", "", x)}) %>%
    dplyr::select(-2) %>%
    pivot_wider(names_from = X1, values_from = X3) -> out$ref_sigmaR
  last <- grep("#---", allout[,1])[2]

  # overall summary ----

  tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~make.unique(as.character(allout[last+3,1:ncol(tmp)]))) %>%
    janitor::clean_names(.) %>% rename(log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
  # do some renaming

  if(out$n_sex == 2) {
    out$derived_quant_summary %>%
      rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
  }
  last <- grep("#---", allout[,1])[3]

  # mean wt ----

  ## weight at size matrix
  tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_mid_points)+3)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+4,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    rename_all(~c("sex", "maturity", "year", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
    mutate(size = as.numeric(size)) -> out$wt_at_size
  last <- last + 4 + nrow(tmp)

  # maturity vector ----

  out$maturity_at_size_vector <- as.numeric(allout[last+2,1:length(out$size_mid_points)]); last <- grep("#---", allout[,1])[4]

  # catch fit summary ----

  ## catch summary
  tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-4)), ncol = 14)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:14])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m",
                  "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
  ## catch q
  tibble(series = unique(out$catch_fit_summary$series),
         log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]

  # index fix_summary ----

  ## index summary
  tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:13])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv",
                  "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
  ## sdnr_MAR_cpue
  tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3, 1:2])
  }
  out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]

  # size composition fit summary ----

  ## size composition fit summary
  ## get info first
  tmp <- matrix(ncol = 12, nrow = length((last+3):(grep("sdnr_MAR_lf", allout[,1])-2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity","shell_name", "maturity_name", "nsamp_obs")) %>%
    mutate_at(c(1:3, 5, 12), as.numeric) -> tmp

  ## get comps
  last <- last + 2 # set last to start where the data is
  tmp %>%
    nest_by(mod_series, .keep = T) %>% ungroup() %>%
    mutate(row = purrr::map_dbl(data, ~nrow(.)),
           row = lag(row),
           row = cumsum(ifelse(is.na(row), 0, row)) + last) %>%
    mutate(comps = purrr::map2(data, row, function(data, row) {

      comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
      for(i in 1:nrow(comp_tmp)) {
        comp_tmp[i,] <- as.numeric(allout[row + i, 13:ncol(allout)])
      }
      as_tibble(comp_tmp) %>%
        dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
      if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
      if(comp_agg == F){

        comp_tmp %>%
          rename_all(~c(paste0("obs_", out$size_mid_points[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_mid_points[1:(ncol(comp_tmp)/2)]))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit2) %>%
          transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      if(comp_agg == T){
        nobs <- ncol(comp_tmp)/2
        group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
        comp_tmp %>%
          rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
          transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }

      return(comp_out)

    })) %>% transmute(comps) %>% unnest -> out$size_fit_summary

  last <- grep("sdnr_MAR_lf", allout[,1])
  ## sdnr_MAR_lf
  tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+i, 1:2])
  }
  out$sdnr_MAR_lf <- tmp
  last <- grep("Francis_weights", allout[,1])
  ## francis weights
  out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[7]

  ## add stage two weights to fit summary
  out$size_fit_summary %>%
    mutate(vn = exp(out$parameters$estimate[grepl("Log_vn_size_comp", out$parameters$parameter)][.$mod_series]),
           out_lambda = out$francis_weights[.$mod_series],
           nsamp_est = exp(out$parameters$estimate[grepl("Log_vn_size_comp", out$parameters$parameter)][.$mod_series]) * nsamp_obs * out_lambda) -> out$size_fit_summary

  # selectivity ----

  ## selex
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
  ## retention
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
  ## discard
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_disc#; last <- grep("Select_control", allout[,2])
  # ## slx control
  # as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
  #   mutate_all(as.numeric) %>%
  #   rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
  #                 "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
  #   # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
  #   group_by(gear, start_yr, end_yr) %>%
  #   mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
  #   transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
  #
  # out$slx_control %>%
  #   mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
  #          type = ifelse(gear > 0, "capture", "retention")) %>%
  #   distinct(fleet, type, sex, start_yr, end_yr) %>%
  #   mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
  #          end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
  #          year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
  #   unnest(year) %>%
  #   mutate(block = paste(start_yr, "-", end_yr)) %>%
  #   dplyr::select(-start_yr, -end_yr) -> tmp
  # slx_cap %>%
  #   left_join(tmp %>%
  #               filter(type == "capture") %>%
  #               transmute(year, fleet, sex, capture_block = block),
  #             by = join_by(year, sex, fleet)) %>%
  #   left_join(slx_ret %>%
  #               left_join(tmp %>%
  #                           filter(type == "retention") %>%
  #                           transmute(year, fleet, sex, ret_disc_block = block),
  #                         by = join_by(year, sex, fleet)),
  #             by = join_by(year, sex, fleet, size)) %>%
  #   left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
  #   transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
  #             slx_discard, ret_disc_block) -> out$selectivity

  slx_cap %>% left_join(slx_ret, by = join_by(year, sex, fleet, size)) %>% left_join(slx_disc, by = join_by(year, sex, fleet, size)) -> out$selectivity
  last <- grep("#----", allout[,1])[8]

  # mortality ----

  ## M by season
  tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last + 4 + i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4

  ## M by sex-maturity-size class
  tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "maturity", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
    mutate_at(c(1, 4:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 3

  ## fully selected F by season, sex, fleet
  tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
    mutate_at(c(3:ncol(.)), as.numeric) %>%
    pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 4

  ## fully selected F by season, sex, fleet
  ## skip same as above
  last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 4

  ## F by sex, year, season, size
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuous") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 4
  out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size))

  ## Z by sex, year, maturity, season, size
  tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
    mutate_at(3:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
    mutate_at(3:ncol(.), as.numeric) %>%
    mutate(maturity = case_when(maturity == 1 ~ "mature",
                                maturity == 2 ~ "immature",
                                maturity == 0 ~ "undetermined")) -> disc; last <- grep("#---", allout[,1])[9]
  out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))

  # n matrix ----

  ## n matrix by sex and maturity
  nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[9]+2):grep("sex_maturity_shell_con", allout[,1]),], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))

  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
      mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
  }
  bind_rows(list_tmp) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) -> out$n_matrix


  ## n matrix by sex, maturity and shell condition
  nmats <- grep("#N(.)", apply(allout[(grep("sex_maturity_shell_con", allout[,1])):grep("#---", allout[,1])[10],], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))

  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+i, c(1, 5:(ncol(tmp)+3))])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
      mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
  }
  bind_rows(list_tmp) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) %>%
    right_join(out$n_matrix, ., by = join_by(year, size)) -> out$n_matrix

  last <- grep("#---", allout[,1])[10]


  # growth ----

  ## molt probability
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", out$size_mid_points)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
    mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)

  ## growth transition matrix
  gmats <- grep("#growth_matrix", allout[,1]) + 1; last <- gmats[1]
  gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(gmats)) {
    tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- gmats_names
  out$growth_transition <- list_tmp

  ## size transition matrix
  smats <- grep("#size_matrix", allout[,1]) + 1; last <- gmats[1]
  smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(smats)) {
    tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- smats_names
  out$size_transition <- list_tmp
  last <- grep("#---", allout[,1])[11]




  # reference points ----

  # combinations of seasons and fleets with Fs
  tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F

  ##reference points and ofl
  out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
  out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
  out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
  out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
  out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
  out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
  out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
  out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
  out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
  last <- grep("#---", allout[,1])[14]

  # simple likelihood stuff ----
  max(c(length(unique(out$catch_fit_summary$series)),
        length(unique(out$index_fit_summary$series)),
        length(unique(out$size_fit_summary$series)))) -> cols
  tmp <- matrix(nrow = 5, ncol = cols + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+1+i, 1:ncol(tmp)])
  }
  out$nloglike <- tmp[,-1]
  out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
  out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))

  # objective function ----
  out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)
}
if(version %in% c("2.20.16", "2.20.17", "2.20.19", "2.20.20")){
  # setup ----
  # Suppress the NA message in the coercion to double
  options(warn = -1)

  # read text file
  allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  out <- list()

  # version ----
  out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
  # model name ----
  out$model_name <- model_name
  # stock ----
  out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[7,], collapse = " ", na.rm = T))
  # general info ----
  last <- 8
  ## units
  out$wt_units <-  gsub("Weightunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  out$n_units <- gsub("Numbersunit:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
            out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
            out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
            out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s",
            out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units
  ## years
  out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
  out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
  last <- grep("Year_range", allout[,1]) # start saving last position in file
  ## number of seasons
  out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## number of fleets
  out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## fleet names
  out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
  ## n sexes
  out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n shell conidition
  out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n maturity states
  out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## size structure
  out$n_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  out$max_size_bins <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  out$size_breaks <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1
  out$size_mid_points <- as.numeric(allout[last + 1, 2:(out$n_size_bins+1)], na.rm = T); last <- last + 1

  # likelihoods by type ----

  # read lines individually
  catch = as.numeric(na.omit(as.numeric(allout[last + 3,]))); last <- last + 3
  index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1

  # coerce to tibble
  rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
    transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
    add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
    add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
    add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
    add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type

  # likelihoods by type and fleet ----
  ## catches
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 5,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 6,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 7,])))) %>%
    transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 8
  ## index
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 5
  ## size composition
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 5
  ## recruitment penalties
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 3
  ## tagging
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 5
  ## growth
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 4
  bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet



  # penalties ----

  tmp <- matrix(nrow = 10, ncol = 3)
  for(i in 1:10) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
  as_tibble(tmp) %>%
    mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Rec_dev", "Sex_ratio",
                       "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                       "Seldevs")) %>%
    transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp

  out$penalties <- tmp
  last <- last + 14

  # parameters ----

  ## par tibble
  tmp <- matrix(ncol = 11, nrow = length((last + 1):(grep("#---", allout[,1])[1]-3)))
  for(i in 1:nrow(tmp)) {
    if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
      as.character(allout[last + 1 + i, 1:13]) %>%
        .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
    } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
  }
  # oddly spaced column on one par
  log_vn_lines <- grep("Log_vn_size", tmp[,2])
  bind_rows( as_tibble(tmp[1:(log_vn_lines[1]-1), 1:11]),
             as_tibble(tmp[log_vn_lines, c(1, 2, 4:11)]),
             as_tibble(tmp[(log_vn_lines[length(log_vn_lines)]+1):nrow(tmp), 1:11]) ) %>%
    rename_all(~c("parameter_count", "parameter", "estimate", "phase", "lower_bound", "upper_bound", "status",
                  "penalty", "gradient", "standard_error", "est_count")) %>%
    #janitor::clean_names() %>%
    mutate_at(c(1, 3:11), as.numeric) %>%
    dplyr::select(-status) -> out$parameters; last <- grep("#---", allout[,1])[1]
  out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
  ## parameters at bounds
  out$parameters %>%
    mutate(range = upper_bound - lower_bound,
           status = ifelse(estimate < (lower_bound+range*0.01), 1,
                           ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
    filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds

  # max gradient ----
  out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)

  # reference points ----

  ## ref tibble
  tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 5)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- na.omit(as.numeric(allout[last + 2 + i,]))
  }
  as_tibble(tmp) %>%
    mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                   paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets),
                   paste0("OFL_", 1:out$n_fleets))) %>%
    transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
  out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
  out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
  out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
  out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
  out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
  out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
  out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
  last <- grep("#---", allout[,1])[2] - 2
  ## ref sigma
  allout[last:(last+1),] %>% dplyr::select_if(~ !any(is.na(.))) %>%
    mutate_all(., function(x){gsub(";", "", x)}) %>%
    dplyr::select(-2) %>%
    pivot_wider(names_from = X1, values_from = X3) -> out$ref_sigmaR
  last <- grep("#---", allout[,1])[2]

  # overall summary ----

  tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~make.unique(as.character(allout[last+3,1:ncol(tmp)]))) %>%
    janitor::clean_names(.) %>% rename(log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
  # do some renaming

  if(out$n_sex == 2) {
    out$derived_quant_summary %>%
      rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
  }
  last <- grep("#---", allout[,1])[3]

  # mean wt ----

  ## weight at size matrix
  tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_mid_points)+3)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+4,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    rename_all(~c("sex", "maturity", "year", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
    mutate(size = as.numeric(size)) -> out$wt_at_size
  last <- last + 4 + nrow(tmp)

  # maturity vector ----

  out$maturity_at_size_vector <- as.numeric(allout[last+2,1:length(out$size_mid_points)]); last <- grep("#---", allout[,1])[4]

  # catch fit summary ----

  ## catch summary
  tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-4)), ncol = 14)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:14])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m",
                  "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
  ## catch q
  tibble(series = unique(out$catch_fit_summary$series),
         log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]

  # index fix_summary ----

  ## index summary
  tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:13])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv",
                  "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
  ## sdnr_MAR_cpue
  tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3, 1:2])
  }
  out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]

  # size composition fit summary ----

  ## size composition fit summary
  ## get info first
  tmp <- matrix(ncol = 12, nrow = length((last+3):(grep("#----", allout[,1])[7]-2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity","shell_name", "maturity_name", "nsamp_obs")) %>%
    mutate_at(c(1:3, 5, 12), as.numeric) -> tmp

  # save info for neff below
  tmp_neff <- tmp

  ## get comps
  last <- last + 2 # set last to start where the data is
  tmp %>%
    nest_by(mod_series, .keep = T) %>% ungroup() %>%
    mutate(row = purrr::map_dbl(data, ~nrow(.)),
           row = lag(row),
           row = cumsum(ifelse(is.na(row), 0, row)) + last) %>%
    mutate(comps = purrr::map2(data, row, function(data, row) {

      comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
      for(i in 1:nrow(comp_tmp)) {
        comp_tmp[i,] <- as.numeric(allout[row + i, 13:ncol(allout)])
      }
      as_tibble(comp_tmp) %>%
        dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
      if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
      if(comp_agg == F){

        comp_tmp %>%
          rename_all(~c(paste0("obs_", out$size_mid_points[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_mid_points[1:(ncol(comp_tmp)/2)]))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit2) %>%
          transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      if(comp_agg == T){
        nobs <- ncol(comp_tmp)/2
        group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
        comp_tmp %>%
          rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_mid_points, ncol = nobs))))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>%
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
          transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }

      return(comp_out)

    })) %>% transmute(comps) %>% unnest -> out$size_fit_summary

  ## sample size
  last <- grep("Sample_size_multipliers", allout[,1])
  tmp <- matrix(ncol = 5, nrow = (grep("sdnr_MAR_lf", allout[,1])-4) - last)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+1+i, 1:5])
  }
  as_tibble(tmp) %>%
    rename_all(~c("mod_series", "row", "multiplier", "nsamp_est", "nsamp_obs_wt")) -> out$effective_sample_size

  ## add estimated effective sample size to output
  out$size_fit_summary %>%
    left_join(out$effective_sample_size %>%
                transmute(year = tmp_neff$year, mod_series, multiplier, nsamp_est, nsamp_obs_wt),
              by = join_by(year, mod_series)) -> out$size_fit_summary

  ## sdnr_MAR_lf
  last <- grep("sdnr_MAR_lf", allout[,1])
  tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+i, 1:2])
  }
  out$sdnr_MAR_lf <- tmp
  last <- grep("Francis_weights", allout[,1])
  ## francis weights
  out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[8]

  # selectivity ----

  ## selex
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
  ## retention
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
  ## discard
  tmp <- matrix(ncol = 3 + length(out$size_mid_points), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_disc#; last <- grep("Select_control", allout[,2])
  # ## slx control
  # as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
  #   mutate_all(as.numeric) %>%
  #   rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
  #                 "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
  #   # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
  #   group_by(gear, start_yr, end_yr) %>%
  #   mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
  #   transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
  #
  # out$slx_control %>%
  #   mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
  #          type = ifelse(gear > 0, "capture", "retention")) %>%
  #   distinct(fleet, type, sex, start_yr, end_yr) %>%
  #   mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
  #          end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
  #          year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
  #   unnest(year) %>%
  #   mutate(block = paste(start_yr, "-", end_yr)) %>%
  #   dplyr::select(-start_yr, -end_yr) -> tmp
  # slx_cap %>%
  #   left_join(tmp %>%
  #               filter(type == "capture") %>%
  #               transmute(year, fleet, sex, capture_block = block),
  #             by = join_by(year, sex, fleet)) %>%
  #   left_join(slx_ret %>%
  #               left_join(tmp %>%
  #                           filter(type == "retention") %>%
  #                           transmute(year, fleet, sex, ret_disc_block = block),
  #                         by = join_by(year, sex, fleet)),
  #             by = join_by(year, sex, fleet, size)) %>%
  #   left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
  #   transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
  #             slx_discard, ret_disc_block) -> out$selectivity

  slx_cap %>% left_join(slx_ret, by = join_by(year, sex, fleet, size)) %>% left_join(slx_disc, by = join_by(year, sex, fleet, size)) -> out$selectivity
  last <- grep("#----", allout[,1])[9]

  # mortality ----

  ## M by season
  tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last + 4 + i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4

  ## M by sex-maturity-size class
  tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "maturity", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
    mutate_at(c(1, 4:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 3

  ## fully selected F by season, sex, fleet
  tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
    mutate_at(c(3:ncol(.)), as.numeric) %>%
    pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 4

  ## fully selected F by season, sex, fleet
  ## skip same as above
  last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 4

  ## F by sex, year, season, size
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuous") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_mid_points)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 4
  out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size))

  ## Z by sex, year, maturity, season, size
  tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
    mutate_at(3:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 4
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+4+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_mid_points)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
    mutate_at(3:ncol(.), as.numeric) %>%
    mutate(maturity = case_when(maturity == 1 ~ "mature",
                                maturity == 2 ~ "immature",
                                maturity == 0 ~ "undetermined")) -> disc; last <- grep("#---", allout[,1])[10]
  out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))

  # n matrix ----

  ## n matrix by sex and maturity
  nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[10]+2):grep("sex_maturity_shell_con", allout[,1]),], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))

  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+2+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
      mutate(type = tolower(gsub("-", "_", gsub("#N[(]|[)]|#", "", nmats[m])))) -> list_tmp[[m]]
  }
  bind_rows(list_tmp) %>%
    filter(!is.na(year)) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) -> out$n_matrix


  ## n matrix by sex, maturity and shell condition
  nmats <- grep("#N(.)", apply(allout[(grep("sex_maturity_shell_con", allout[,1])):grep("#---", allout[,1])[11],], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))

  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs)+1, ncol = 1 + length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+i, c(1, 5:(ncol(tmp)+3))])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_mid_points)) %>%
      mutate(type = tolower(gsub("-", "_", gsub("#N[(]|[)]|#", "", nmats[m])))) -> list_tmp[[m]]
  }
  bind_rows(list_tmp) %>%
    filter(!is.na(year)) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) %>%
    right_join(out$n_matrix, ., by = join_by(year, size)) -> out$n_matrix

  last <- grep("#---", allout[,1])[11]


  # growth ----

  ## molt probability
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_mid_points))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", out$size_mid_points)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
    mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)

  ## growth transition matrix
  gmats <- grep("#growth_matrix", allout[,1]) + 1; last <- gmats[1]
  gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(gmats)) {
    tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- gmats_names
  out$growth_transition <- list_tmp

  ## size transition matrix
  smats <- grep("#size_matrix", allout[,1]) + 1; last <- gmats[1]
  smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(smats)) {
    tmp <- matrix(nrow = length(out$size_mid_points), ncol = length(out$size_mid_points))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_mid_points; colnames(tmp) <- out$size_mid_points
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- smats_names
  out$size_transition <- list_tmp
  last <- grep("#---", allout[,1])[12]




  # reference points ----

  # combinations of seasons and fleets with Fs
  tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F

  ##reference points and ofl
  out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
  out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
  out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
  out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
  out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
  out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
  out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
  out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
  out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
  last <- grep("#---", allout[,1])[15]

  # simple likelihood stuff ----
  max(c(length(unique(out$catch_fit_summary$series)),
        length(unique(out$index_fit_summary$series)),
        length(unique(out$size_fit_summary$series)))) -> cols
  tmp <- matrix(nrow = 5, ncol = cols + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+1+i, 1:ncol(tmp)])
  }
  out$nloglike <- tmp[,-1]
  out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
  out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))

  # objective function ----
  out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)
}

# output ----
return(out)
}
