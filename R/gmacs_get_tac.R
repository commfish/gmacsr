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
gmacs_get_tac <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL, stock = c("AIGKC", "BBRKC"), hs_pars = NULL, input_type = c(1, 2, 3)) {

  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }

  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(out = purrr::map(all_out, function(all_out) {
      # unpack hs_pars
      hs_pars <- list2DF(hs_pars)

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
            # kg and thousnad n cancel out conversions
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

        if(input_type == 3){
          # mature female abundance
          mfa <- gmacs_get_n_matrix(list(ao)) %>%
            filter(year == max(year), as.numeric(size) > 90) %>%
            pull(females) %>% sum()
          # mature male abundance
          mma <- gmacs_get_n_matrix(list(ao)) %>%
            filter(year == max(year), as.numeric(size) > 120) %>%
            pull(males) %>% sum()
          # legal male abundance
          lma <- gmacs_get_n_matrix(list(ao)) %>%
            filter(year == max(year), as.numeric(size) > 135) %>%
            pull(males) %>% sum()

          # relative mating effectiveness (sizes 120 - 165+)
          ra <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1.2, 1.4, 1.6, 1.8, 2.1, 2.4, 2.7, 3.0)






          # do harvest strategy


        }


      }

      return(out)

    })) %>%
    transmute(out) %>% unnest(out) -> out

  return(out)




}





