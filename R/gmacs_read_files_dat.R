#' Read gmacs.dat file
#'
#' Load GMACS gmacs.dat file in R
#' @param gmacs.dat NULL. Path to gmacs.dat file.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.17".

#' @return List of gmacs.dat file contents.
#' @examples gmacs_read_files_dat(dat_file = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat", model_name = "23.1b")
#'
#' @export
#'
gmacs_read_files_dat <- function(gmacs.dat, model_name = NULL, version = NULL) {

if(is.null(version)){version = "2.20.34"}
  if(version %in% c("2.20.34")){
    # setup ----

    # Suppress the NA message in the coercion to double
    options(warn = -1)

    # read text file
    dat <- read.delim(gmacs.dat, sep = "", header = F, col.names = c(1:1000), fill = T,
                      na.strings = "", colClasses = "character", comment.char = "#")
    # create out object
    out <- list()

    # model dimensions ----

    ## version
    out$version <- version
    ## model_name
    out$model_name <- model_name

    # files ----
    out$dat_file <- dat[1,1]
    out$ctl_file <- dat[2,1]
    out$prj_file <- dat[3,1]

    # units ----
    out$wt_unit <- dat[4,1]
    out$n_unit <- dat[5,1]

    # stock ----
    out$stock <- dat[6,1]

    # jittering ----
    out$jitter <- as.numeric(dat[7,1])
    out$jitter_use_pin <- <- as.numeric(dat[8,1])
    out$jitter_sd <- as.numeric(dat[9,1])

    # out options ----
    out$out_ref_pars <- as.numeric(dat[10,1])
    out$out_recruit <- as.numeric(dat[11,1])
    out$out_ssb <- as.numeric(dat[12,1])
    out$out_fbar <- as.numeric(dat[13,1])
    out$out_dynb0 <- as.numeric(dat[14,1])

    # retro peels ----
    out$nyr_retro <- as.numeric(dat[15,1])

    # run options ----
    out$max_phase <- as.numeric(dat[16,1])
    out$max_function_calls <- as.numeric(dat[17,1])
    out$calc_ref_points <- as.numeric(dat[18,1])
    out$use_pin <- as.numeric(dat[19,1])
    out$verbose <- as.numeric(dat[20,1])

    ## eof
    if(as.numeric(dat[21,1]) == 9999){return(out)} else{stop("end of file not correct, debug")}


  }
if(version %in% c("2.20.33", "2.20.31", "2.20.21", "2.20.20", "2.20.19", "2.20.17", "2.20.16", "2.20.14")){
  # setup ----

  # Suppress the NA message in the coercion to double
  options(warn = -1)

  # read text file
  dat <- read.delim(gmacs.dat, sep = "", header = F, col.names = c(1:1000), fill = T,
                    na.strings = "", colClasses = "character", comment.char = "#")
  # create out object
  out <- list()

  # model dimensions ----

  ## version
  out$version <- version
  ## model_name
  out$model_name <- model_name

  # files ----
  out$dat_file <- dat[1,1]
  out$ctl_file <- dat[2,1]
  out$prj_file <- dat[3,1]

  # units ----
  out$wt_unit <- dat[4,1]
  out$n_unit <- dat[5,1]

  # stock ----
  out$stock <- dat[6,1]

  # jittering ----
  out$jitter <- as.numeric(dat[7,1])
  out$jitter_sd <- as.numeric(dat[8,1])

  # out options ----
  out$out_ref_pars <- as.numeric(dat[9,1])
  out$out_recruit <- as.numeric(dat[10,1])
  out$out_ssb <- as.numeric(dat[11,1])
  out$out_fbar <- as.numeric(dat[12,1])
  out$out_dynb0 <- as.numeric(dat[13,1])

  # retro peels ----
  out$nyr_retro <- as.numeric(dat[14,1])

  # run options ----
  out$max_phase <- as.numeric(dat[15,1])
  out$max_function_calls <- as.numeric(dat[16,1])
  out$calc_ref_points <- as.numeric(dat[17,1])
  out$use_pin <- as.numeric(dat[18,1])
  out$verbose <- as.numeric(dat[19,1])

  ## eof
  if(as.numeric(dat[20,1]) == 9999){return(out)} else{stop("end of file not correct, debug")}


}

}
