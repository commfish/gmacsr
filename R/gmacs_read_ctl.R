#' Read .ctl file
#'
#' Load .ctl file in R
#' @param ctl_file NULL. Path to .ctl file.
#' @param dat_file NULL. Path to .dat file.
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.17".

#' @return List of .ctl file contents. LIKELY STILL CONTAINS BUGS
#' @examples gmacs_read_ctl(ctl_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.ctl", dat_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat", model_name = "23.1b")
#'
#' @export
#'
gmacs_read_ctl <- function(ctl_file, dat_file, version = NULL){

  if(is.null(version)){version = "2.20.20"}
  if(version %in% c("2.20.16", "2.20.17", "2.20.19", "2.20.20")){
    # setup ----

    # Suppress the NA message in the coercion to double
    options(warn = -1)

    # read text file
    ctl <- read.delim(ctl_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "",
                      colClasses = "character", comment.char = "#")
    dat <- gmacs_read_dat(dat_file) # read data file for sex, shell, fleet information
    # create out object
    out <- list()
    last <- 0

    # block structure ----

    n_block_groups <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    blocks_per_group <- NULL
    for(i in 1:n_block_groups){
      blocks_per_group[i] <- as.numeric(ctl[last + i, 1])
    }; last <- last + n_block_groups
    blocks <- matrix(nrow = sum(blocks_per_group), ncol = 2)
    for(i in 1:sum(blocks_per_group)){
      blocks[i,1:2] <- as.numeric(ctl[last + i, 1:2])
    }; last <- last + nrow(blocks)
    # out
    tibble(block_group = rep(1:n_block_groups, blocks_per_group),
           l_blk = blocks[,1],
           u_blk = blocks[,2]) -> out$block_structure

    # other controls ----

    out$first_yr_rec <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_yr_rec <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$terminal_molt <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$rec_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$rec_sex_ratio_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$init_sex_ratio <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$init_conditions_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$ref_size_class <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_lambda <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$use_yr_avg_sex_ratio <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$spr_yrs_equil <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$dev_phase <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$first_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$first_full_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_full_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    out$last_bias_correction <- as.numeric(ctl[last + 1, 1]); last <- last + 1

    # thetas ----

    if(out$init_conditions_type == 2){
      tmp <- matrix(nrow = 3 + (2 * dat$n_sex) + 3 + (dat$n_sex * dat$n_shell * dat$n_maturity * dat$n_size_bins), ncol = 7)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
      }

      as_tibble(tmp) %>%
        rename_all(~c("initial", "lower_bound", "upper_bound", "phase", "prior_type", "prior_1", "prior_2")) %>%
        mutate(parameter = c("log_R0", "log_Rini", "log_Rbar",
                             expand_grid(sex = 1:dat$n_sex, prefix = c("rec_a_sex_", "rec_b_sex_")) %>%
                               mutate(par = paste0(prefix, sex)) %>%
                               pull(par),
                             "log_sigmaR", "steepness", "rho",
                             expand_grid(prefix = "scaled_log_N_dev",
                                         sex = paste0("_sex_", 1:dat$n_sex),
                                         maturity = paste0("_maturity_", 1:dat$n_maturity),
                                         shell = paste0("_shell_", 1:dat$n_shell),
                                         class = paste0("_class_", 1:dat$n_size_bins)) %>%
                               mutate(par = paste0(prefix, sex, shell, maturity, class)) %>%
                               pull(par) )) -> out$thetas
    }
    if(out$init_conditions_type == 3){
      tmp <- matrix(nrow = 3 + (2 * dat$n_sex) + 3 + (dat$n_sex * dat$n_shell * dat$n_maturity * dat$n_size_bins - 1), ncol = 7)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
      }

      as_tibble(tmp) %>%
        rename_all(~c("initial", "lower_bound", "upper_bound", "phase", "prior_type", "prior_1", "prior_2")) %>%
        mutate(parameter = c("log_R0", "log_Rini", "log_Rbar",
                             expand_grid(sex = 1:dat$n_sex, prefix = c("rec_a_sex_", "rec_b_sex_")) %>%
                               mutate(par = paste0(prefix, sex)) %>%
                               pull(par),
                             "log_sigmaR", "steepness", "rho",
                             expand_grid(prefix = "scaled_log_N_dev",
                                         sex = paste0("_sex_", 1:dat$n_sex),
                                         maturity = paste0("_maturity_", 1:dat$n_maturity),
                                         shell = paste0("_shell_", 1:dat$n_shell),
                                         class = paste0("_class_", 1:dat$n_size_bins)) %>%
                               mutate(par = paste0(prefix, sex, shell, maturity, class)) %>%
                               pull(par) %>% .[-out$ref_size_class])) -> out$thetas
    }
    last <- last + nrow(tmp)

    # allometry, maturity, legal ----

    ## allometry
    out$allometry_type <- as.numeric(ctl[last + 1, 1]); last <- last + 1
    if(out$allometry_type == 2) {
      tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
      for(i in 1:nrow(tmp)) {
        tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
      }

      as_tibble(tmp) %>%
        rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
        mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
        pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "wt") %>%
        mutate(size = as.numeric(size)) -> out$allometry
      last <- last + nrow(tmp)
    }
    if(out$allometry_type == 3) {
      out$allometry <- list()
      for(i in 1:dat$n_sex){
        out$allometry[[i]] <- matrix(ncol = dat$n_size_bins, nrow = length(dat$start_year:dat$terminal_year)+1)
        for(j in 1:nrow(out$allometry[[i]])) {
          out$allometry[[i]][j, 1:ncol(out$allometry[[i]])] <- as.numeric(ctl[last + j, 1:ncol(out$allometry[[i]])])
        }
        out$allometry[[i]] %>%
          as_tibble() %>%
          rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
          bind_cols(tibble(year = c(dat$start_year:(dat$terminal_year+1))), .) -> out$allometry[[i]]
        last <- last + nrow(out$allometry[[i]])

      }
      names(out$allometry) <- paste0("sex_", 1:dat$n_sex)

    }


    ## maturity
    tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
      mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
      pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "maturity") %>%
      mutate(size = as.numeric(size)) -> out$maturity
    last <- last + nrow(tmp)

    ## legal status
    tmp <- matrix(ncol = dat$n_size_bins, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~as.character(dat$size_bins[-length(dat$size_bins)])) %>%
      mutate(sex = paste0("sex_", 1:dat$n_sex)) %>%
      pivot_longer(1:(ncol(.)-1), names_to = "size", values_to = "legal") %>%
      mutate(size = as.numeric(size)) -> out$legal_status
    last <- last + nrow(tmp)

    # growth controls ----

    out$max_rec_class <- as.numeric(ctl[last + 1, 1:dat$n_sex]); last <- last + 1
    out$functional_maturity <- as.numeric(ctl[last + 1, 1]); last <- last + 1

    ## growth setup
    tmp <- matrix(ncol = 3, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("growth_mat_type", "growth_inc_type", "block_group")) %>%
      mutate(sex = 1:dat$n_sex) %>%
      dplyr::select(4, 1:3) -> out$growth_options; last <- last + nrow(tmp)

    ## molt probability setup
    tmp <- matrix(ncol = 2, nrow = dat$n_sex)
    for(i in 1:nrow(tmp)) {
      tmp[i, 1:ncol(tmp)] <- as.numeric(ctl[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("molt_prob_type", "block_group")) %>%
      mutate(sex = 1:dat$n_sex) %>%
      dplyr::select(3, 1:2) -> out$molt_probability_options; last <- last + nrow(tmp)

    ## growth pars
    ### number of pars per growth type
    tibble(growth_inc_type = 1:4,
           n_pars = c(3, dat$n_size_bins+1, dat$n_size_bins+1, 2)) -> growth_types
    ### get number of expected growth pars
    out$growth_options %>%
      left_join(growth_types, by = join_by(growth_inc_type)) -> growth_tmp
    ### read growth pars
    growth_pars <- list()
    for(i in 1:nrow(growth_tmp)){
      if(growth_tmp$growth_inc_type[i] < 0) {break}
      if(growth_tmp$growth_inc_type[i] == 1) {
        # growth pars main
        growth_pars[[i]] <- list()
        growth_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(growth_tmp[i,5]))
        for(j in 1:nrow(growth_pars[[i]]$main)) {
          growth_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(growth_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0(c("alpha_sex_", "beta_sex_"), i), paste0("gscale_sex_", i))) -> growth_pars[[i]]$main
        last <- last + nrow(growth_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) == 0){growth_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          growth_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          growth_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(growth_pars[[i]]$extra)) {
            growth_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(growth_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> growth_pars[[i]]$extra
          last <- last + nrow(growth_pars[[i]]$extra)
        }
      }
      if(growth_tmp$growth_inc_type[i] == 3) {
        # growth pars main
        growth_pars[[i]] <- list()
        growth_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(growth_tmp[i,5]))
        for(j in 1:nrow(growth_pars[[i]]$main)) {
          growth_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(growth_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0("molt_increment_base_sex_", i, "_class_", 1:dat$n_size_bins), paste0("gscale_base_sex_", i))) -> growth_pars[[i]]$main
        last <- last + nrow(growth_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) == 0){growth_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(growth_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          growth_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          growth_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(growth_pars[[i]]$extra)) {
            growth_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(growth_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> growth_pars[[i]]$extra
          last <- last + nrow(growth_pars[[i]]$extra)
        }
      }

    }
    if(length(growth_pars) > 0) { names(growth_pars) <- paste0("sex_", 1:dat$n_sex) }

    ## molt pars
    tibble(molt_prob_type = 0:3,
           n_pars = c(NA, 0, 2, NA)) -> molt_types
    ### get number of expected growth pars
    out$molt_probability_options %>%
      left_join(molt_types, by = join_by(molt_prob_type)) -> molt_tmp
    ### read molt pars
    molt_pars <- list()
    for(i in 1:nrow(molt_tmp)){
      if(molt_tmp$molt_prob_type[i] == 1) {
        molt_pars[[i]] <- list()
        molt_pars[[i]]$main <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, block = NA,
                                      blk_fn = NA, env_l = NA, env_vr = NA, rw = NA, rw_blk = NA, rw_sigma = NA)[-1,]
        molt_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]
      }
      if(molt_tmp$molt_prob_type[i] == 2) {
        # growth pars main
        molt_pars[[i]] <- list()
        molt_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(molt_tmp[i,4]))
        for(j in 1:nrow(molt_pars[[i]]$main)) {
          molt_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(molt_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = c(paste0("molt_probability_mu_base_sex_", i), paste0("molt_probability_cv_base_sex_", i) )) -> molt_pars[[i]]$main
        last <- last + nrow(molt_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(molt_pars[[i]]$main, block != 0)) == 0){molt_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(molt_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          molt_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          molt_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(molt_pars[[i]]$extra)) {
            molt_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(molt_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> molt_pars[[i]]$extra
          last <- last + nrow(molt_pars[[i]]$extra)
        }
      }
    }
    names(molt_pars) <- paste0("sex_", 1:dat$n_sex)

    # custom growth matrix
    out$growth_matrix <- list()
    for(i in 1:nrow(growth_tmp)){
      if(growth_tmp$growth_mat_type[i] == 1){
        out$growth_matrix[[i]] <- matrix(ncol = dat$n_size_bins, nrow = dat$n_size_bins)
        for(j in 1:nrow(out$growth_matrix[[i]] )) {
          out$growth_matrix[[i]][j, 1:ncol(out$growth_matrix[[i]])] <- as.numeric(ctl[last + j, 1:ncol(out$growth_matrix[[i]])])
        }
        names(out$growth_matrix)[i] <- paste0("sex_", i)
        last <- last + nrow(out$growth_matrix[[i]])
      }
    }
    # null if no custom growth matrix
    if(length(out$growth_matrix) == 0) {out$growth_matrix <- NULL}

    ### write to out
    out$growth_pars <- growth_pars
    out$molt_probability_pars <- molt_pars

    # natural mortality controls ----

    ## options
    out$natural_mortality_options <- matrix(ncol = 13, nrow = dat$n_sex * dat$n_maturity)
    for(i in 1:nrow(out$natural_mortality_options)){
      out$natural_mortality_options[i, 1:13] <- as.numeric(ctl[last + i, 1:13])
    }
    as_tibble(out$natural_mortality_options) %>%
      rename_all(~c("relative", "type", "extra", "brkpts", "mirror", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw", "mirror_rw")) -> out$natural_mortality_options
    last <- last + nrow(out$natural_mortality_options)
    ## brkpts
    out$natural_mortality_brkpts <- list()
    for(i in 1:nrow(out$natural_mortality_options)){
      if(out$natural_mortality_options$brkpts[i] > 0){
        out$natural_mortality_brkpts[[i]] <- vector()
        out$natural_mortality_brkpts[[i]][1:out$natural_mortality_options$brkpts] <- as.numeric(ctl[last + i, 1:out$natural_mortality_options$brkpts])
        names(out$natural_mortality_brkpts)[i] <- paste0("sex_", i, "_maturity_", i)
      }
    }
    if(length(out$natural_mortality_brkpts) == 0) {out$natural_mortality_brkpts <- NULL}
    last <- last + length(out$natural_mortality_brkpts)

    ## types of natural morality
    tibble(type = 0:1,
           n_pars = c(1, NA)) -> nat_m_types
    ## get number of expected pars
    out$natural_mortality_options %>%
      left_join(nat_m_types, by = join_by(type)) %>%
      mutate(n_pars = n_pars + brkpts) %>%
      mutate(sex_mat = expand_grid(sex = 1:dat$n_sex, mat = 1:dat$n_maturity) %>%
               mutate(sex_mat = paste0("sex_", sex, "_maturity_", mat)) %>% pull(sex_mat)) -> nat_m_tmp

    nat_m_pars <- list()
    for(i in 1:nrow(out$natural_mortality_options)){
      nat_m_pars[[i]] <- list()
      if(nat_m_tmp$type[i] == 0){
        nat_m_pars[[i]]$main <- matrix(ncol = 7, nrow = nat_m_tmp$n_pars[i])
        for(j in 1:nrow(nat_m_pars[[i]]$main)) {
          nat_m_pars[[i]]$main[j,] <- as.numeric(ctl[last+j, 1:7])
        }

        as_tibble(nat_m_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
          mutate(par = ifelse(nat_m_tmp$brkpts[i] == 0,
                              paste0("M_base_", nat_m_tmp$sex_mat[i]),
                              c(paste0("M_base_", nat_m_tmp$sex_mat[i]),
                                paste0("M_base_", nat_m_tmp$sex_mat[i], "_brk_", 1:nat_m_tmp$brkpts[i])))) -> nat_m_pars[[i]]$main
        last <- last + nrow(nat_m_pars[[i]]$main)
        if(nat_m_tmp$block[i] == 0) {nat_m_pars[[i]]$extra <- nat_m_pars[[i]]$main %>% dplyr::slice(-1:-nrow(.)) }
        if(nat_m_tmp$block[i] > 0) {
          # get names (and number) of extra pars
          nat_m_pars[[i]]$main %>%
            mutate(n_blocks = blocks_per_group[nat_m_tmp$block[i]]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars
          nat_m_pars[[i]]$extra <- matrix(ncol = 7, nrow = length(extra_pars))
          for(j in 1:nrow(nat_m_pars[[i]]$extra)) {
            nat_m_pars[[i]]$extra[j, 1:7] <- as.numeric(ctl[last + j, 1:7])
          }
          as_tibble(nat_m_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
            mutate(par = extra_pars) -> nat_m_pars[[i]]$extra
          last <- last + nrow(nat_m_pars[[i]]$extra)
        }
      }
    }
    # rename list
    names(nat_m_pars) <- nat_m_tmp$sex_mat
    # write to out object
    out$natural_mortality_pars <- nat_m_pars

    # selectivity controls ----

    ## selex options
    out$selectivity_options <- matrix(ncol = dat$n_fleets, nrow = 2 + 4*dat$n_sex)
    for(i in 1:nrow(out$selectivity_options)){
      out$selectivity_options[i, 1:ncol(out$selectivity_options)] <- as.numeric(ctl[last+i, 1:ncol(out$selectivity_options)])
    }
    t(out$selectivity_options) %>%
      as_tibble %>%
      rename_all(~ c("sex_specific", paste0("type_sex_", 1:dat$n_sex), "sel_with_another_gear", paste0("extra_sex_", 1:dat$n_sex),
                     paste0("force_to_one_sex_", 1:dat$n_sex), paste0("force_class_sex_", 1:dat$n_sex))) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$selectivity_options
    last <- last + ncol(out$selectivity_options) - 1

    ## retention options
    out$retention_options <- matrix(ncol = dat$n_fleets, nrow = 1 + 4*dat$n_sex)
    for(i in 1:nrow(out$retention_options)){
      out$retention_options[i, 1:ncol(out$retention_options)] <- as.numeric(ctl[last+i, 1:ncol(out$retention_options)])
    }
    t(out$retention_options) %>%
      as_tibble %>%
      rename_all(~ c("sex_specific", paste0("type_sex_", 1:dat$n_sex), paste0("ret_flag_sex_", 1:dat$n_sex), paste0("extra_sex_", 1:dat$n_sex), paste0("est_max_sex_", 1:dat$n_sex))) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$retention_options
    last <- last + ncol(out$retention_options) - 1

    # selectivity pars
    ### number of pars per selectivity type
    tibble(type = 0:11,
           n_pars = c(dat$n_size_bins, dat$n_size_bins, 2, 2, 3, 0, 0, 4, 2, NA, 1, NA)) -> sel_types
    ### get number of expected selectivity pars
    out$selectivity_options %>%
      dplyr::select(fleet, sex_specific, grep("type", names(.))) %>%
      pivot_longer(3:ncol(.), names_to = "sex", values_to = "type") %>%
      mutate(sex = gsub("type_sex_", "", sex)) %>% arrange(sex) %>%
      left_join(out$selectivity_options %>%
                  dplyr::select(fleet, grep("extra", names(.))) %>%
                  pivot_longer(2:ncol(.), names_to = "sex", values_to = "extra") %>%
                  mutate(sex = gsub("extra_sex_", "", sex)) %>% arrange(sex), by = join_by(fleet, sex)) %>%
      left_join(sel_types, by = join_by(type)) %>%
      # add extra pars for spline knots??
      # remove parameters when selectivity isn't sex specific
      mutate(n_pars = ifelse(is.na(n_pars) & type == 9, extra, n_pars),
             n_pars = ifelse(sex > 1, sex_specific * (n_pars + extra), n_pars + extra)) %>%
      filter(n_pars != 0) -> sel_tmp

    ### read selectivity pars
    sel_pars <- list()
    for(i in 1:nrow(sel_tmp)){
      if(sel_tmp$type[i] == 0) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_class_", 1:dat$n_size_bins)) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 2) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_base_logistic_", c("mean", "cv"))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 3) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_base_logistic_", c("sel50", "sel95"))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 8) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = ifelse(sel_tmp$extra[i] == 0,
                              paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_", c("sel50", "sel95")),
                              c(paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_", c("sel50", "sel95")),
                                paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_extra_par_", 1:sel_tmp$extra[i] )))) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # sel pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
      if(sel_tmp$type[i] == 10) {
        # sel pars main
        sel_pars[[i]] <- list()
        sel_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(sel_tmp[i,"n_pars"]))
        for(j in 1:nrow(sel_pars[[i]]$main)) {
          sel_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(sel_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", sel_tmp$fleet[i], "_sex_", sel_tmp$sex[i], "_des_logistic_mean")) -> sel_pars[[i]]$main
        last <- last + nrow(sel_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) == 0){sel_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(sel_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          sel_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          sel_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            sel_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(sel_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> sel_pars[[i]]$extra
          last <- last + nrow(sel_pars[[i]]$extra)
        }
      }
    }
    names(sel_pars) <- sel_tmp %>%
      mutate(name = paste0(fleet, "_sex_", sex)) %>% pull(name)

    # retention pars
    out$retention_options %>%
      dplyr::select(fleet, sex_specific, grep("type", names(.))) %>%
      pivot_longer(3:ncol(.), names_to = "sex", values_to = "type") %>%
      mutate(sex = gsub("type_sex_", "", sex)) %>% arrange(sex) %>%
      left_join(out$retention_options %>%
                  dplyr::select(fleet, grep("extra", names(.))) %>%
                  pivot_longer(2:ncol(.), names_to = "sex", values_to = "extra") %>%
                  mutate(sex = gsub("extra_sex_", "", sex)) %>% arrange(sex), by = join_by(fleet, sex)) %>%
      left_join(sel_types, by = join_by(type)) %>%
      # add extra pars for spline knots??
      # remove parameters when selectivity isn't sex specific
      mutate(n_pars = ifelse(is.na(n_pars) & type == 9, extra, n_pars),
             n_pars = ifelse(sex > 1, sex_specific * n_pars, n_pars)) %>%
      filter(n_pars != 0) -> ret_tmp

    ### read retention pars
    ret_pars <- list()
    for(i in 1:nrow(ret_tmp)){
      if(ret_tmp$type[i] == 0) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_class_", 1:dat$n_size_bins)) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(sel_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
      if(ret_tmp$type[i] == 2) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_base_logistic_", c("mean", "cv"))) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(ret_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
      if(ret_tmp$type[i] == 3) {
        # ret pars main
        ret_pars[[i]] <- list()
        ret_pars[[i]]$main <- matrix(ncol = 14, nrow = as.numeric(ret_tmp[i,"n_pars"]))
        for(j in 1:nrow(ret_pars[[i]]$main)) {
          ret_pars[[i]]$main[j, 1:14] <- as.numeric(ctl[last + j, 1:14])
        }
        as_tibble(ret_pars[[i]]$main) %>%
          rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "block", "blk_fn", "env_l", "env_vr", "rw", "rw_blk", "rw_sigma")) %>%
          mutate(par = paste0("sel_", ret_tmp$fleet[i], "_sex_", ret_tmp$sex[i], "_base_logistic_", c("ret50", "ret95"))) -> ret_pars[[i]]$main
        last <- last + nrow(ret_pars[[i]]$main)
        # growth pars extra
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) == 0){ret_pars[[i]]$extra <- tibble(initial = NA,lower_bound = NA,upper_bound = NA, prior_type = NA, prior_1 = NA, prior_2 = NA, phase = NA, relative = NA)[-1,]}
        if(nrow(filter(ret_pars[[i]]$main, block != 0)) > 0){

          # get names (and number) of extra pars
          ret_pars[[i]]$main %>%
            filter(block > 0) %>%
            mutate(n_blocks = blocks_per_group[block]) %>%
            mutate(extra_pars = purrr::map2(par, n_blocks, function(par, n_blocks){
              paste0(par, "_block_", 1:n_blocks)
            })) %>% pull(extra_pars) %>%
            unlist -> extra_pars

          ret_pars[[i]]$extra <- matrix(ncol = 8, nrow = length(extra_pars))
          for(j in 1:nrow(ret_pars[[i]]$extra)) {
            ret_pars[[i]]$extra[j, 1:8] <- as.numeric(ctl[last + j, 1:8])
          }
          as_tibble(ret_pars[[i]]$extra) %>%
            rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase", "relative")) %>%
            mutate(par = extra_pars) -> ret_pars[[i]]$extra
          last <- last + nrow(ret_pars[[i]]$extra)
        }
      }
    }
    names(ret_pars) <- ret_tmp %>%
      mutate(name = paste0(fleet, "_sex_", sex)) %>% pull(name)

    # write to out
    out$selectivity_pars <- sel_pars
    out$retention_pars <- ret_pars

    # catchability controls ----

    out$catchability_options <- matrix(nrow = length(unique(dat$index$series)), ncol = 10)
    for(i in 1:nrow(out$catchability_options)){
      out$catchability_options[i, 1:10] <- as.numeric(ctl[last+i, 1:10])
    }
    as_tibble(out$catchability_options) %>%
      rename_all(~c("analytic", "lambda", "emphasis", "mirror", "block", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw")) %>%
      bind_cols(tibble(index_series = unique(dat$index$series)), .) -> out$catchability_options
    last <- last + nrow(out$catchability_options)

    q_pars <- list()
    # main pars
    q_pars$main <- matrix(ncol = 7, nrow = filter(out$catchability_options, mirror == 0) %>% nrow)
    for(i in 1:nrow(out$catchability_options)){
      # q pars main
      q_pars$main[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(q_pars$main) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("survey_q_", filter(out$catchability_options, mirror == 0)$index_series)) -> q_pars$main
    last <- last + nrow(q_pars$main)
    # extra pars
    if(filter(out$catchability_options, block > 0) %>% nrow(.) == 0) {q_pars$extra <- NULL} else {stop("Can't yet read extra catchability pars")}
    # write to out
    out$catchability_pars <- q_pars

    # additional cv ----

    out$additional_cv_options <- matrix(nrow = length(unique(dat$index$series)), ncol = 7)
    for(i in 1:nrow(out$additional_cv_options)){
      out$additional_cv_options[i, 1:7] <- as.numeric(ctl[last+i, 1:7])
    }
    as_tibble(out$additional_cv_options) %>%
      rename_all(~c("mirror", "block", "env_l", "env_vr", "rw", "rw_blk", "sigma_rw")) %>%
      bind_cols(tibble(index_series = unique(dat$index$series)), .) -> out$additional_cv_options
    last <- last + nrow(out$additional_cv_options)

    addcv_pars <- list()
    # main pars
    addcv_pars$main <- matrix(ncol = 7, nrow = filter(out$additional_cv_options, mirror == 0) %>% nrow)
    for(i in 1:nrow(addcv_pars$main)){
      # addcv pars main
      addcv_pars$main[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(addcv_pars$main) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("add_cv_", filter(out$additional_cv_options, mirror == 0)$index_series)) -> addcv_pars$main
    last <- last + nrow(addcv_pars$main)
    # extra pars
    if(filter(out$catchability_options, block > 0) %>% nrow(.) == 0) {addcv_pars$extra <- NULL} else {stop("Can't yet read extra catchability pars")}
    # write to out
    out$additional_cv_pars <- addcv_pars

    # fishing mortality controls ----

    out$fishing_mortality_options <- matrix(nrow = dat$n_fleets, ncol = 12)
    for(i in 1:nrow(out$fishing_mortality_options)){
      out$fishing_mortality_options[i, 1:12] <- as.numeric(ctl[last + i, 1:12])
    }
    as_tibble(out$fishing_mortality_options) %>%
      rename_all(~c("init_sex_1_F", "init_sex_2_F", "pen_sd_sex_1", "pen_sd_sex_2", "phz_mean_F_sex_1", "phz_mean_F_sex_2", "lower_mean_F", "upper_mean_F",
                    "lower_ann_F_sex_1", "upper_ann_F_sex_1", "lower_ann_F_sex_2", "upper_ann_F_sex_2")) %>%
      bind_cols(tibble(index_series = length(unique(dat$index$series))), .) -> out$fishing_mortality_options
    last <- last + nrow(out$fishing_mortality_options)

    # size composition controls ----

    out$size_composition_options <- matrix(ncol = dat$n_size_series, nrow = 8)
    for(i in 1:nrow(out$size_composition_options)){
      out$size_composition_options[i, 1:ncol(out$size_composition_options)] <- as.numeric(ctl[last + i, 1:ncol(out$size_composition_options)])
    }
    t(out$size_composition_options) %>%
      as_tibble() %>%
      rename_all(~c("lik_type", "tail_compression", "pmin", "aggregator_code", "prediction_type", "lambda_sample_size", "lambda", "survey_q")) %>%
      bind_cols(dat$size_comp %>%
                  distinct(org_series, fleet, sex, shell, maturity, type) %>%
                  transmute(fleet = dat$fleet_names[fleet], sex, shell, maturity, type), .) -> out$size_composition_options
    last <- last + 8

    # dispersion pars
    comp_pars <- matrix(nrow = max(out$size_composition_options$aggregator_code), ncol = 7)
    for(i in 1:nrow(comp_pars)){
      comp_pars[i, 1:7] <- as.numeric(ctl[last + i, 1:7])
    }
    as_tibble(comp_pars) %>%
      rename_all(~c("initial", "lower_bound", "upper_bound", "prior_type", "prior_1", "prior_2", "phase")) %>%
      mutate(par = paste0("overdispersion_for_size_comp_", 1:max(out$size_composition_options$aggregator_code))) -> out$size_composition_pars
    last <- last + nrow(out$size_composition_pars)

    # emphasis factors ----

    ## tagging data
    out$emphasis_tagging <- as.numeric(ctl[last + 1, 1]);last <- last + 1
    ## catch data
    out$emphasis_catch <- as.numeric(ctl[last + 1, 1:dat$n_catch_series]);last <- last + 1
    ## weights for penalties
    out$fishing_mortality_pen <- matrix(nrow = dat$n_fleets, ncol = 4)
    for(i in 1:dat$n_fleets) {
      out$fishing_mortality_pen[i, 1:4] <- as.numeric(ctl[last+i, 1:4])
    }
    as_tibble(out$fishing_mortality_pen) %>%
      rename_all(~c("mean_sex_1_fdevs", "mean_sex_2_fdevs", "ann_sex_1_fdevs", "ann_sex_2_fdevs")) %>%
      bind_cols(tibble(fleet = dat$fleet_names), .) -> out$fishing_mortality_pen
    last <- last + nrow(out$fishing_mortality_pen)
    ## emphasis factors
    out$emphasis_factors <- NULL
    for(i in 1:13) {
      out$emphasis_factors[i] <- as.numeric(ctl[last+i, 1])
    }
    tibble(penalty = c("log_fdev", "mean_F", "not_used", "not_used", "not_used", "rec_dev_smoothness", "mean_sex_ratio",
                       "molt_prob_smoothness", "selex_smoothness", "init_n_smoothness", "ann_fdevs_sex_1", "ann_fdevs_sex_2", "dev_pars"),
           emphasis = out$emphasis_factors) -> out$emphasis_factors
    last <- last + 13

    # eof ----

    if(as.numeric(ctl[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}

  }

}
