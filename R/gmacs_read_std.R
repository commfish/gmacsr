#' Read gmacs.std file
#'
#' Load gmacs.std file in R
#' @param file NULL. Path to gmacs.std file.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param sub_text NULL. Character string filtering, example: "ssb".

#' @return Tibble containing .std file contents
#' @examples gmacs_read_std(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.std", model_name = "23.1b", sub_text = "ssb")
#'
#' @export
#'
gmacs_read_std <- function(file, model_name = NULL, sub_text = NULL) {

  std <- read.delim(file, sep = "", skip = 2, header = F)

  std %>%
    as_tibble() %>%
    rename_all(~c("est_no","par", "est", "se")) %>%
    mutate(model_name = model_name) -> out

  if(!is.null(sub_text)) {
    out %>% filter(grepl(sub_text, par)) -> out
  }

  return(out)

}
