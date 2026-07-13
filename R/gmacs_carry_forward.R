#' Carry forward
#'
#' Extend values from terminal year to next (e.g., survey selectivity in last year)
#' @param x Tibble.
#' @param group_col Name of column to extend forward, usually year.

#' @return Tibble is same format as x, with one more year or whatever group
#' @examples macs_get_slx(list(all_out)) %>% gmacs_carry_forward()
#'
#' @export
#'
gmacs_carry_forward <- function(x, group_col = "year") {
  x %>% bind_rows(filter(x, year == max(year)) %>% mutate(year = year + 1))
}
