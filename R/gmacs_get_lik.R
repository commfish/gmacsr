#' Get Likelihood Components
#'
#' Isolate likelihood components by data source from Gmacsall.out by model
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble of likelihood components from Gmacsall.out with column 'model'.
#' @examples gmacs_get_lik(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_lik <- function(all_out = NULL, file = NULL, model_name = NULL, version = NULL){

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

      x$likelihoods_by_type_and_fleet %>%
        transmute(model = x$model_name, process, net_lik) %>%
        # add number of parameters
        add_row(model = x$model_name, process = "n_pars", net_lik = x$parameters %>% filter(phase > 0) %>% nrow()) %>%
        # add total lik
        add_row(model = x$model_name, process = "total", net_lik = x$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik))


    })) %>% transmute(data) %>% unnest(data) %>%
    pivot_wider(names_from = model, values_from = net_lik) -> out

  return(out)

}
