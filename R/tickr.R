#' Get minor ticks
#'
#' Get minor ticks breaks and labels for axis scale_ functions
#' @param data Data frame with column for year
#' @param var Name of year column
#' @param to Interval

#' @return Tibble with breaks and labels.
#' @examples tickr(tibble(yr = 1900:2100), yr, 5)
#'
#' @export
#'
tickr <- function(data, var, to) {

  VAR <- enquo(var) # makes VAR a dynamic variable

  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

