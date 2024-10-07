#' Get Recruitment Distribution
#'
#' Compute size distribution of recruit classes
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param n_rec_class Number of recruitment classes as list with first element vector for males, second element vector for females.
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Tibble of proportion by size class and sex with column 'model'.
#' @examples gmacs_get_lik_type_pen(all_out = list(mod_23.0a, mod_23.1b))
#'
#' @export
#'
gmacs_get_recruitment_distribution <- function(all_out = NULL,n_rec_class = NULL, file = NULL, model_name = NULL, version = NULL){

  if(is.null(n_rec_class)){stop("Provide number of recruitment classes; its not in Gmacsall.out")}
  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout, version = version); names(all_out) <- paste0("model", model_name)
  }

  # extract rec dist data ----
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(sex = purrr::map(all_out, function(x){if(x$n_sex == 2){return(c("male", "female"))}  else{return("male")}}),
           mod = purrr::map_chr(all_out, function(x){x$model_name})) %>%
    unnest(sex) %>% group_by(sex) %>% nest() %>%
    bind_cols(tibble(n_rec_class)) %>% unnest(data, n_rec_class) %>% arrange(mod) %>% #pull(all_out) %>% .[[1]] -> x
    mutate(rec_dist = purrr::pmap(list(all_out, sex, n_rec_class), function(x, sex, n_rec_class){

      if(sex == "male") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males:", "Recruitment_rb-males:")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]; rbeta <- pars[2]
        size_breaks <- x$size_mid_points - ((x$size_mid_points[2]-x$size_mid_points[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_mid_points, rec_dist = dist))
      }
      if(sex == "female") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males:", "Recruitment_rb-males:", "Recruitment_ra-females:", "Recruitment_rb-females:")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]*exp(pars[3]); rbeta <- pars[2]*exp(pars[4])
        size_breaks <- x$size_mid_points - ((x$size_mid_points[2]-x$size_mid_points[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_mid_points, rec_dist = dist))
      }

    })) %>%
    transmute(model = mod, sex, rec_dist) %>%
    unnest(rec_dist) %>% ungroup -> out

  return(out)

}
