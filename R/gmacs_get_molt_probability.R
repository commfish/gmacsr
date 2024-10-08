#' Get Molt Probability
#'
#' Isolate molt probability from Gmacsall.out by model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble of molt probability by sex, size class, and year from Gmacsall.out in long format with column 'model'.
#' @examples gmacs_get_molt_probability(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_molt_probability <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){

  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }

  # extract data ----

  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {
      x$molt_probability %>%
        mutate(model = as.character(x$model_name)) %>%
        left_join(x$molt_probability %>%
                    distinct(sex, size, molt_probability, .keep_all = T) %>%
                    distinct(sex, year) %>% group_by(sex) %>%
                    mutate(year_lead = lead(year)) %>% ungroup() %>%
                    replace_na(list(year_lead = max(x$molt_probability$year))) %>%
                    transmute(sex, year, block = paste0(year, " - ", year_lead)), by = c("sex", "year"))
    })) %>% transmute(data) %>% unnest(data) %>%
    transmute(model, sex, year, size, molt_probability, block) -> out

  return(out)

}
