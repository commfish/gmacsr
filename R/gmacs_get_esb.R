#' Get Effective Spawning Biomass 
#'
#' Compute effective spawning biomass (Zheng 1997) used in BBRKC harvest strategy
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble with year and effective spawning biomass.
#' @examples gmacs_get_esb(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_esb <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL) {
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }
  
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(all_out) {
      
      # relative mating effectiveness of males (positive for sizes 120 - 165+)
      ra <- tibble(size = all_out$size_mid_points,
                   ra = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1.2, 1.4, 1.6, 1.8, 2.1, 2.4, 2.7, 3.0))
      
      all_out$n_matrix %>%
        transmute(year, size = as.numeric(size), males, females) %>%
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
        transmute(year, esb) 
    })) %>% transmute(data) %>% unnest(data) -> out
      
    return(out)
      
}
  