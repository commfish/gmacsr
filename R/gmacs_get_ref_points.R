#' Get Biological Reference Points
#'
#' Isolate biological reference points from Gmacsall.out by model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble of reference points from Gmacsall.out with column 'model'.
#' @examples gmacs_get_ref_points(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_ref_points <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){

  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }

  # extract catch data ----

  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {

      x$reference_points %>%
        transmute(parameter_name, estimate) %>%
        pivot_wider(names_from = parameter_name, values_from = estimate) %>%
        transmute(model = as.character(x$model_name),
                  mmb = BMSY * `Bcurr/BMSY`,
                  b35 = BMSY,
                  b_b35 = `Bcurr/BMSY`,
                  male_rbar = `Male_spr_rbar`,
                  rbar_yrs = paste(x$spr_syr, "-", x$spr_nyr),
                  f35 = `Fmsy_1`,
                  fofl = x$spr_fofl * f35,
                  ofl_tot = OFL_tot)

    })) %>% transmute(data) %>% unnest(data) -> out

  return(out)

}
