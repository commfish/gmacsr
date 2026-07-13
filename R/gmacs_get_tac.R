#' Get Catch Summary
#'
#' Isolate catch summary data from Gmacsall.out by model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.
#' @param stock name of stock to apply harvest strategy, currently only AIGKC
#' @param hs_pars list of harvest strategy parameters needed. For AIGKC example: list(max_ex = 0.15, avg_wt = 4.41, calc_wt = F)

#' @return Tibble of tac summary from.
#' @examples gmacs_get_catch_summary(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_tac <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL, stock = c("AIGKC", "BBRKC", "BSTC"), hs_pars = NULL, bbrkc_lba = F) {

  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }

  tibble(mod = names(all_out),
         all_out = all_out) %>% #pull(all_out) %>% .[[1]] -> all_out
    mutate(out = purrr::map(all_out, function(all_out) {
      # unpack hs_pars
     #  hs_pars <- list2DF(hs_pars)

      if(stock == "AIGKC") {

        # long term mma average
        gmacs_get_derived_quantity_summary(list(all_out)) %>%
          filter(year %in% 1987:2017) %>% pull(ssa) %>% mean -> mma_avg
        # current mma
        gmacs_get_derived_quantity_summary(list(all_out)) %>%
          filter(year == max(year)) %>% pull(ssa) -> mma
        # status
        mma_status <- mma / mma_avg
        # legal male abundance
        gmacs_get_n_matrix(list(all_out)) %>%
          filter(year == max(year)) %>%
          filter(size >= 136) %>% pull(males) %>% sum -> lma

        avg_wt <- hs_pars$avg_wt
        # do calulated wt
        if("calc_wt" %in% names(hs_pars) && hs_pars$calc_wt == T){
          gmacs_get_n_matrix(list(all_out)) %>%
            mutate(size = as.numeric(size)) %>%
            filter(year == max(year)) %>%
            left_join(all_out$wt_at_size %>%
                        filter(year == max(year)) %>%
                        distinct(size, wt)) %>%
            # kg and thousand n cancel out conversions
            mutate(calc_lb = wt * 2.20462) %>%
            filter(size >= 136) %>%
            summarise(avg_wt = sum(males * calc_lb) / sum(males)) %>%
            pull(avg_wt) -> avg_wt
        }


        # compute exploitation rate on mma
        if(mma_status < 0.25) {ex_mma <- 0}
        if(mma_status >= 0.25 & mma_status < 1) {ex_mma <- hs_pars$max_ex * mma_status}
        if(mma_status >= 1) {ex_mma <-  hs_pars$max_ex}


        # create output
        tibble(model = all_out$model_name,
               mma = mma / 1000,
               mma_avg = mma_avg / 1000,
               mma_status = mma_status,
               lma = lma / 1000,
               lma_cap = 0.25 * lma,
               max_ex_mma = hs_pars$max_ex,
               avg_wt = avg_wt,
               ex_mma = ex_mma,
               tac_n_no_cap = ex_mma * mma,
               tac_n = min(ex_mma * mma, 0.25 * lma),
               hs_ex_mma = tac_n / mma,
               tac_mil_lb = min(ex_mma * mma, 0.25 * lma) * avg_wt) -> out

      }
      if(stock == "BBRKC") {

        if(bbrkc_lba == F) {
          # nmfs survey catchability
          nmfs_q <- gmacs_get_pars(list(all_out)) %>%
            filter(grepl("Survey_q_survey_1", parameter)) %>%
            pull(estimate)
          # survey slx
          nmfs_slx_m <- gmacs_get_slx(list(all_out)) %>%
            filter(fleet == "NMFS_Trawl", year == max(year), sex == "male") %>%
            transmute(size, slx_capture)
          nmfs_slx_f <- gmacs_get_slx(list(all_out)) %>%
            filter(fleet == "NMFS_Trawl", year == max(year), sex == "female") %>%
            transmute(size, slx_capture)

          # mature female abundance
          mfa <- gmacs_get_n_matrix(list(all_out)) %>%
            mutate(size = as.numeric(size)) %>%
            filter(year == max(year), size >= 90) %>%
            left_join(nmfs_slx_f, by = c("size")) %>%
            mutate(females = females * nmfs_q * slx_capture) %>%
            pull(females) %>% sum()
          # mature male abundance
          mma <- gmacs_get_n_matrix(list(all_out)) %>%
            mutate(size = as.numeric(size)) %>%
            filter(year == max(year), size >= 120) %>%
            left_join(nmfs_slx_m, by = c("size")) %>%
            mutate(males = males * nmfs_q * slx_capture) %>%
            pull(males) %>% sum()
          # legal male abundance
          lma <- gmacs_get_n_matrix(list(all_out)) %>%
            mutate(size = as.numeric(size)) %>%
            filter(year == max(year), size >= 135) %>%
            left_join(nmfs_slx_m, by = c("size")) %>%
            mutate(males = males * nmfs_q * slx_capture) %>%
            pull(males) %>% sum()
          # get effective spawning biomass
          # relative mating effectiveness of males (positive for sizes 120 - 165+)
          ra <- tibble(size = all_out$size_mid_points,
                       ra = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1.2, 1.4, 1.6, 1.8, 2.1, 2.4, 2.7, 3.0))

          all_out$n_matrix %>%
            transmute(year, size = as.numeric(size), males, females) %>%
            left_join(nmfs_slx_m %>% rename(slx_capture_m = slx_capture), by = c("size")) %>%
            left_join(nmfs_slx_f %>% rename(slx_capture_f = slx_capture), by = c("size")) %>%
            mutate(males = males * nmfs_q * slx_capture_m,
                   females = females * nmfs_q * slx_capture_f) %>%
            left_join(all_out$wt_at_size %>%
                        filter(sex == "female") %>%
                        transmute(year, size, wt) %>%
                        # fill in projection year with year before
                        bind_rows(
                          all_out$wt_at_size %>%
                            filter(sex == "female", year == max(year)) %>%
                            transmute(year = year + 1, size, wt)
                        ),
                      by = join_by(year, size)) %>%
            left_join(ra, by = join_by(size)) %>%
            group_by(year) %>%
            summarise(efma = sum(males * ra),
                      mfb = sum(females[size >= 90] * wt[size >= 90]),
                      mfa = sum(females[size >= 90]), .groups = "keep") %>%
            mutate(mate_ratio = min(efma / mfa, 1),
                   esb = mfb * mate_ratio) %>%
            transmute(year, esb)  %>%
            filter(year == max(year) ) %>% pull(esb) %>% .[1] * 2204.62 -> esb

        }

          avg_wt <- hs_pars$avg_wt
          # do calulated wt
          if("calc_wt" %in% names(hs_pars) && hs_pars$calc_wt == T){
            gmacs_get_n_matrix(list(all_out)) %>%
              mutate(size = as.numeric(size)) %>%
              filter(year == max(year)) %>%
              left_join(all_out$wt_at_size %>%
                          filter(year == max(year), sex == "male") %>%
                          distinct(size, wt)) %>%
              mutate(calc_lb = wt * 2204.62) %>%
              filter(size >= 135) %>%
              summarise(avg_wt = sum(males * calc_lb) / sum(males)) %>%
              pull(avg_wt) -> avg_wt
          }

          # do harvest strategy
          if(mfa < 8.4e6){ex_mma <- 0}
          if(mfa >= 8.4e6){
            if(esb < 14.5e6) {ex_mma <- 0}
            if(esb >= 14.5e6 & esb < 34.75e6) {ex_mma <- 0.1}
            if(esb >= 34.75e6 & esb < 55e6) {ex_mma <- 0.125}
            if(esb > 55e6) {ex_mma <- 0.15}
          }

          # tac table (mil and mil lb)
          tibble(model = all_out$model_name,
                 mma = mma / 1e6,
                 mfa = mfa / 1e6,
                 lma = lma / 1e6,
                 esb = esb / 1e6,
                 ex_mma = ex_mma,
                 lma_cap = 0.5 * lma,
                 avg_wt = avg_wt,
                 tac_n_no_cap = ex_mma * mma,
                 tac_n = min(ex_mma * mma, lma_cap),
                 hs_ex_mma = tac_n / mma,
                 tac_mil_lb = tac_n * avg_wt)

      }
      if(stock == "BSTC") {

        # get inputs for raw survey as input
        if(hs_pars$input == "survey") {list2env(hs_pars, envir = environment())}
        if(hs_pars$input == "model_survey") {

          # mfb
          gmacs_get_n_matrix(list(all_out)) %>% transmute(year, size,  mature_females = females_mature_new +  females_mature_old) %>%
            # survey slx
            left_join(gmacs_get_slx(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "female") %>%
                        transmute(year, size, slx_capture) %>%
                        gmacs_carry_forward(), by = join_by(year, size)  ) %>%

            # catchability
            left_join(gmacs_get_index_summary(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "female", maturity == "mature") %>%
                        transmute(year, q) %>%
                        gmacs_carry_forward(), by = join_by(year)) %>%
            # weight at size
            left_join(all_out$wt_at_size %>%
                        filter(maturity == 1) %>%
                        distinct(year, sex, size, wt) %>%
                        filter(sex == "female") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            group_by(year) %>%
            summarise(mfb = sum(mature_females * wt * q * slx_capture) * 1000 / 0.000453592) %>% ungroup -> mfb_lb

          # mmb
          gmacs_get_n_matrix(list(all_out)) %>% transmute(year, size,  mature_males = males_mature_new +  males_mature_old) %>%
            # survey slx
            left_join(gmacs_get_slx(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "male") %>%
                        transmute(year, size, slx_capture) %>%
                        gmacs_carry_forward(), by = join_by(year, size)  ) %>%

            # catchability
            left_join(gmacs_get_index_summary(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "male") %>%
                        transmute(year, q) %>%
                        gmacs_carry_forward(), by = join_by(year)) %>%
            # weight at size
            left_join(all_out$wt_at_size %>% distinct(year, sex, size, wt) %>%
                        filter(sex == "male") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            left_join(hs_pars$gamma_mmb,by = join_by(year)) %>%
            group_by(year) %>%
            summarise(mmb = sum(mature_males * wt * q * slx_capture * gamma_mmb) * 1000 / 0.000453592) %>% ungroup -> mmb_lb

          # preferred males by shell condition
          gmacs_get_n_matrix(list(all_out)) %>%
            transmute(year, size,
                      new = males_mature_new +  males_imature_new,
                      old = males_mature_old +  males_imature_old) %>%
            # survey slx
            left_join(gmacs_get_slx(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "male") %>%
                        transmute(year, size, slx_capture) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            # catchability
            left_join(gmacs_get_index_summary(list(all_out)) %>%
                        filter(fleet == "NMFS", sex == "male") %>%
                        transmute(year, q) %>%
                        gmacs_carry_forward(), by = join_by(year)) %>%
            # weight at size
            left_join(all_out$wt_at_size %>% distinct(year, sex, size, wt) %>%
                        filter(sex == "male") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            pivot_longer(c(new, old), names_to = "shell", values_to = "n") %>%
            left_join(hs_pars$gamma_pref_male,by = join_by(year, shell)) %>%
            group_by(year, shell) %>%
            summarise(abundance = sum(n * q * slx_capture * gamma_pref_male) * 1e6,
                      biomass = sum(n * wt * q * slx_capture * gamma_pref_male) * 1000 / 0.000453592) %>% ungroup -> pref_male

          # hs inputs
          ## mfb
          mfb <- mfb_lb %>% filter(year == max(year)) %>% pull(mfb)
          mfb_avg <- mfb_lb %>% filter(year %in% 1982:2018) %>% pull(mfb) %>% mean
          ## mmb
          mmb <- mmb_lb %>% filter(year == max(year)) %>% pull(mmb)
          mmb_avg <- mmb_lb %>% filter(year %in% 1982:2018) %>% pull(mmb) %>% mean
          ## pref_male
          pref_male <- pref_male %>% filter(year == max(year))

        }
        if(hs_pars$input == "model_population") {

          # mfb
          gmacs_get_n_matrix(list(all_out)) %>% transmute(year, size,  mature_females = females_mature_new +  females_mature_old) %>%
            # weight at size
            left_join(all_out$wt_at_size %>%
                        filter(maturity == 1) %>%
                        distinct(year, sex, size, wt) %>%
                        filter(sex == "female") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            group_by(year) %>%
            summarise(mfb = sum(mature_females * wt) * 1000 / 0.000453592) %>% ungroup -> mfb_lb

          # mmb
          gmacs_get_n_matrix(list(all_out)) %>% transmute(year, size,  mature_males = males_mature_new +  males_mature_old) %>%
            # weight at size
            left_join(all_out$wt_at_size %>% distinct(year, sex, size, wt) %>%
                        filter(sex == "male") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%

            left_join(hs_pars$gamma_mmb,by = join_by(year)) %>%
            group_by(year) %>%
            summarise(mmb = sum(mature_males * wt * gamma_mmb) * 1000 / 0.000453592) %>% ungroup -> mmb_lb

          # preferred males by shell condition
          gmacs_get_n_matrix(list(all_out)) %>%
            transmute(year, size,
                      new = males_mature_new +  males_imature_new,
                      old = males_mature_old +  males_imature_old) %>%
            # weight at size
            left_join(all_out$wt_at_size %>% distinct(year, sex, size, wt) %>%
                        filter(sex == "male") %>%
                        transmute(year, size, wt) %>%
                        gmacs_carry_forward(), by = join_by(year, size)) %>%
            pivot_longer(c(new, old), names_to = "shell", values_to = "n") %>%
            left_join(hs_pars$gamma_pref_male,by = join_by(year, shell)) %>%
            group_by(year, shell) %>%
            summarise(abundance = sum(n * gamma_pref_male) * 1e6,
                      biomass = sum(n * wt * gamma_pref_male) * 1000 / 0.000453592) %>% ungroup -> pref_male

          # hs inputs
          ## mfb
          mfb <- mfb_lb %>% filter(year == max(year)) %>% pull(mfb)
          mfb_avg <- mfb_lb %>% filter(year %in% 1982:2018) %>% pull(mfb) %>% mean
          ## mmb
          mmb <- mmb_lb %>% filter(year == max(year)) %>% pull(mmb)
          mmb_avg <- mmb_lb %>% filter(year %in% 1982:2018) %>% pull(mmb) %>% mean
          ## pref_male
          pref_male <- pref_male %>% filter(year == max(year))

        }

        # get b / bavg
        mmb_mmb_avg <- mmb / mmb_avg
        mfb_mfb_avg <- mfb / mfb_avg

        # step 1: exploitation rate on mmb
        # fishery closed
        if(mmb_mmb_avg < 0.25) {ex_mmb <- 0}
        # fishery open
        if(mmb_mmb_avg >= 0.25) {

          # no dimmer
          if(mfb_mfb_avg < 0.25) {
            ex_mmb <- 0.05
          }
          if(mfb_mfb_avg > 1) {
            ex_mmb <- min(0.2, ((((mmb_mmb_avg - 0.25) / 0.75) * 0.15) + 0.05))
          }

          # compute female dimmer
          if(mfb_mfb_avg >= 0.25 & mfb_mfb_avg <= 1) {
            m <- (((((mfb_mfb_avg - 0.25) / 0.75) * 0.15) + 0.05) - 0.05) / 0.75
            b <- 0.05 - (m * 0.25)

            ex_mmb <- m * min(mmb_mmb_avg, 1) + b
          }
        }

        # step 2: elma max cap
        ## perf male data.frame must be formatted correctly
        tibble(perf_male_abund = pref_male %>% pull(abundance) %>% sum(),
               avg_wt = pref_male %>%
                 group_by(year) %>%
                 summarise(avg_wt = sum(biomass) / sum(abundance)) %>% pull(avg_wt),
               percent_old = pref_male$abundance[pref_male$shell == "old"] / perf_male_abund,
               old_slx = ifelse("old_slx" %in% names(hs_pars), hs_pars$old_slx, 0.4),,
               elma = perf_male_abund * (1 - percent_old) + old_slx * perf_male_abund * percent_old,
               elmb = elma * avg_wt) -> elm_cap

        # compute final tac in lb, create output
        tibble(mfb, mfb_avg, mfb_mfb_avg,
               mmb, mmb_avg, mmb_mmb_avg,
               ex_mmb) %>%
          bind_cols(elm_cap) %>%
          mutate(tac_no_cap_lb = ex_mmb * mmb,
                 max_tac_lb = 0.5 * elmb,
                 tac_lb = min(tac_no_cap_lb, max_tac_lb)) %>%
          # convert to millions
          mutate_at(c(1, 2, 4, 5, 8, 12:16), function(x){x / 1e6}) -> out
      }
      if(stock == "BSSC") {

        # get inputs for raw survey as input
        if(hs_pars$input == "survey") {list2env(hs_pars, envir = environment())}
        if(hs_pars$input == "model_survey") {

          # mature male biomass from n matrix
          gmacs_get_n_matrix(list(all_out)) %>%
            transmute(year, size, males_mature) %>%
            # join to selectivty
            left_join(gmacs_get_slx(list(all_out)) %>%
                        filter((fleet == "NMFS_Trawl_1982" & year < 1989) | (fleet == "NMFS_Trawl_1989" & year >= 1989),
                               sex == "male") %>%
                        transmute(year, size, slx_capture) %>%
                        gmacs_carry_forward(),
                      by = c("year", "size")) %>%
            # join to catchability
            left_join(gmacs_get_index_summary(list(all_out)) %>%
                        filter(fleet %in% c("NMFS_Trawl_1982", "NMFS_Trawl_1989"),
                               sex == "male") %>%
                        transmute(year, q),
                      by = "year") %>%
            # join to weight at size
            left_join(all_out$wt_at_size %>%
                        filter(sex == "male", maturity == 1) %>%
                        transmute(year, size, wt),
                      by = c("year", "size")) %>%
            # summarise
            group_by(year) %>%
            summarise(mmb = sum(males_mature * wt * q * slx_capture) * 1000 / 0.000453592) %>% ungroup -> mmb_lb

          # mature female biomass from n matrix
          gmacs_get_n_matrix(list(all_out)) %>%
            transmute(year, size, females_mature) %>%
            # join to selectivty
            left_join(gmacs_get_slx(list(all_out)) %>%
                        filter((fleet == "NMFS_Trawl_1982" & year < 1989) | (fleet == "NMFS_Trawl_1989" & year >= 1989),
                               sex == "female") %>%
                        transmute(year, size, slx_capture) %>%
                        gmacs_carry_forward(),
                      by = c("year", "size")) %>%
            # join to catchability
            left_join(gmacs_get_index_summary(list(all_out)) %>%
                        filter(fleet %in% c("NMFS_Trawl_1982", "NMFS_Trawl_1989"),
                               sex == "female") %>%
                        transmute(year, q),
                      by = "year") %>%
            # join to weight at size
            left_join(all_out$wt_at_size %>%
                        filter(sex == "female", maturity == 1) %>%
                        transmute(year, size, wt),
                      by = c("year", "size")) %>%
            # summarise
            group_by(year) %>%
            summarise(mmb = sum(females_mature * wt * q * slx_capture) * 1000 / 0.000453592) %>% ungroup -> mfb_lb

          # total mature biomass

        }


        # get b / bmsy
        b_bmsy <- b / bmsy

        # declare fmsy if not provided as input
        if(!("fmsy" %in% names(hs_pars))) {fmsy <- 0.3}

        # step 1: compute exploitation rate on mmb
        if(b_bmsy < 0.25) {ex_mmb <- 0}
        if(b_bmsy >= 0.25 & b_bmsy < 1) {
          ex_mmb <- (fmsy / 3) + (b - 0.25 * bmsy) * (0.417 * fmsy / (0.75 * bmsy))
        }
        if(b_bmsy >= 1) {ex_mmb <- 0.75 * fmsy}

        # step 2: elma max cap
        ## perf male data.frame must be formatted correctly
        tibble(perf_male_abund = pref_male %>% pull(abundance) %>% sum(),
               avg_wt = pref_male %>%
                 group_by(year) %>%
                 summarise(avg_wt = sum(biomass) / sum(abundance)) %>% pull(avg_wt),
               percent_old = pref_male$abundance[pref_male$shell == "old"] / perf_male_abund,
               old_slx = ifelse("old_slx" %in% names(hs_pars), hs_pars$old_slx, 0.25),
               elma = perf_male_abund * (1 - percent_old) + old_slx * perf_male_abund * percent_old,
               elmb = elma * avg_wt) -> elm_cap

        # compute final tac in lb, create output
        tibble(b, bmsy, b_bmsy,
               mmb, ex_mmb) %>%
          bind_cols(elm_cap) %>%
          mutate(tac_no_cap_lb = ex_mmb * mmb,
                 max_tac_lb = 0.58 * elmb,
                 tac_lb = min(tac_no_cap_lb, max_tac_lb)) %>%
          # convert to millions
          mutate_at(c(1, 2, 4, 6, 9:14), function(x){x / 1e6}) -> out

      }

      return(out)

    })) %>%
    transmute(out) %>% unnest(out) -> out

  return(out)




}





