#' Read mcoutREF file
#'
#' Load projection reference point results file in R
#' @param file NULL. Path to mcoutREF.rep file.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.16".

#' @return Tibble of mcoutREF.rep file contents.
#' @examples gmacs_read_mcoutREF(file = "./AIGKC/models/2024/may/EAG/23.1b/mcoutREF.rep", model_name = "23.1b")
#'
#' @export
#'
gmacs_read_mcoutREF <- function(file, model_name = NULL, version = NULL){

  mcout <- read.delim(file, sep = "", skip = 1, header = F)
  ao <- gmacs_read_allout(file.path(dirname(file), "Gmacsall.out"), version = version)

  tibble(model = ifelse(is.null(model_name), NA, model_name),
         mcout) %>%
    rename_all(~c("model", "draw", "mean_rec", "f", "mmb", "bmsy", "bmsy_b0", "ofl",
                  paste0("fmsy_", ao$fleet_names), paste0("fofl_", ao$fleet_names))) -> out

  return(out)

}
