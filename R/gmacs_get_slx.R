#' Get Selectivity
#'
#' Isolate selectivity from Gmacsall.out by model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble of selectivities from Gmacsall.out in long format with column 'model'.
#' @examples gmacs_get_slx(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_slx <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){

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
      x$selectivity %>%
        mutate(model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out

  return(out)

}
