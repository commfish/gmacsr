#' Write gmacs.dat file
#'
#' Write GMACS gmacs.dat file in R
#' @param input Named list of data needed to write file (need more info here), look at output of gmacs_read_files_dat()
#' @param file NULL. File path to write gmacs.dat file

#' @return File saved in specified location.
#' @examples gmacs_write_dat(input, file = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat")
#'
#' @export
#'
gmacs_write_files_dat <- function(input, file) {

  if(is.null(input$version)){input$version = "2.20.34"}
  if(input$version %in% c("2.20.34")){
    # setup ----

    # create output matrix
    out <- matrix(nrow = 1e6)
    last <- 0 # location tracker

    # version ----
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- paste("# GMACS", input$version, "Setup File"); last <- last + 1
    if(!is.null(input$model_name)){out[last + 1,] <- paste("# Model", input$model_name); last <- last + 1}
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1

    # files ----
    out[last + 1,] <- "# datafile"; last <- last + 1
    out[last + 1,] <- input$dat_file; last <- last + 1
    out[last + 1,] <- "# controlfile"; last <- last + 1
    out[last + 1,] <- input$ctl_file; last <- last + 1
    out[last + 1,] <- "# projectfile"; last <- last + 1
    out[last + 1,] <- input$prj_file; last <- last + 1

    # units ----
    out[last + 1,] <- "# weightunit"; last <- last + 1
    out[last + 1,] <- input$wt_unit; last <- last + 1
    out[last + 1,] <- "# numbersunit"; last <- last + 1
    out[last + 1,] <- input$n_unit; last <- last + 1

    # stock ----
    out[last + 1,] <- "# StockName"; last <- last + 1
    out[last + 1,] <- input$stock; last <- last + 1

    # jittering ----
    out[last + 1,] <- "# IsJittered"; last <- last + 1
    out[last + 1,] <- input$jitter; last <- last + 1
    out[last + 1,] <- "# Jitter_use_pin"; last <- last + 1
    out[last + 1,] <- input$jitter_use_pin; last <- last + 1
    out[last + 1,] <- "# sdJitter"; last <- last + 1
    out[last + 1,] <- input$jitter_sd; last <- last + 1

    # out options ----
    out[last + 1,] <- "# OutRefPars"; last <- last + 1
    out[last + 1,] <- input$out_ref_pars; last <- last + 1
    out[last + 1,] <- "# OutRecruit"; last <- last + 1
    out[last + 1,] <- input$out_recruit; last <- last + 1
    out[last + 1,] <- "# OutSSB"; last <- last + 1
    out[last + 1,] <- input$out_ssb; last <- last + 1
    out[last + 1,] <- "# Outfbar"; last <- last + 1
    out[last + 1,] <- input$out_fbar; last <- last + 1
    out[last + 1,] <- "# OutDynB0"; last <- last + 1
    out[last + 1,] <- input$out_dynb0; last <- last + 1


    # retro peels ----
    out[last + 1,] <- "# nyrRetro"; last <- last + 1
    out[last + 1,] <- input$nyr_retro; last <- last + 1

    # run options ----
    out[last + 1,] <- paste(input$max_phase, "# Maximum phase (stop the estimation after this phase)", sep = " "); last <- last + 1
    if(is.null(input$max_function_calls)){input$max_function_calls <- -1}
    out[last + 1,] <- paste(input$max_function_calls, "# Maximum number of function calls", sep = " "); last <- last + 1
    if(is.null(input$calc_ref_points)){input$calc_ref_points <- 1}
    out[last + 1,] <- paste(input$calc_ref_points, "# Calculate reference points (0=no)", sep = " "); last <- last + 1
    if(is.null(input$use_pin)){input$use_pin <- 0}
    out[last + 1,] <- paste(input$use_pin, "# use pin file (0=no, 1=yes)", sep = " "); last <- last + 1
    if(is.null(input$verbose)){input$verbose <- 1}
    out[last + 1,] <- paste(input$verbose, "# VERBOSE FLAG (0 = off, 1 = on, 2 = objective func; 3 diagnostics)", sep = " "); last <- last + 1

    ## eof
    out[last + 1,] <- "eof"
    out[last + 1,] <- "9999"


  }
  if(input$version %in% c("2.20.33", "2.20.31", "2.20.21", "2.20.20", "2.20.19", "2.20.17", "2.20.16", "2.20.14")){
    # setup ----

    # create output matrix
    out <- matrix(nrow = 1e6)
    last <- 0 # location tracker

    # version ----
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- paste("# GMACS", input$version, "Setup File"); last <- last + 1
    if(!is.null(input$model_name)){out[last + 1,] <- paste("# Model", input$model_name); last <- last + 1}
    out[last + 1,] <- "##==============================================================="; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1
    out[last + 1,] <- " "; last <- last + 1

    # files ----
    out[last + 1,] <- "# datafile"; last <- last + 1
    out[last + 1,] <- input$dat_file; last <- last + 1
    out[last + 1,] <- "# controlfile"; last <- last + 1
    out[last + 1,] <- input$ctl_file; last <- last + 1
    out[last + 1,] <- "# projectfile"; last <- last + 1
    out[last + 1,] <- input$prj_file; last <- last + 1

    # units ----
    out[last + 1,] <- "# weightunit"; last <- last + 1
    out[last + 1,] <- input$wt_unit; last <- last + 1
    out[last + 1,] <- "# numbersunit"; last <- last + 1
    out[last + 1,] <- input$n_unit; last <- last + 1

    # stock ----
    out[last + 1,] <- "# StockName"; last <- last + 1
    out[last + 1,] <- input$stock; last <- last + 1

    # jittering ----
    out[last + 1,] <- "# IsJittered"; last <- last + 1
    out[last + 1,] <- input$jitter; last <- last + 1
    out[last + 1,] <- "# sdJitter"; last <- last + 1
    out[last + 1,] <- input$jitter_sd; last <- last + 1

    # out options ----
    out[last + 1,] <- "# OutRefPars"; last <- last + 1
    out[last + 1,] <- input$out_ref_pars; last <- last + 1
    out[last + 1,] <- "# OutRecruit"; last <- last + 1
    out[last + 1,] <- input$out_recruit; last <- last + 1
    out[last + 1,] <- "# OutSSB"; last <- last + 1
    out[last + 1,] <- input$out_ssb; last <- last + 1
    out[last + 1,] <- "# Outfbar"; last <- last + 1
    out[last + 1,] <- input$out_fbar; last <- last + 1
    out[last + 1,] <- "# OutDynB0"; last <- last + 1
    out[last + 1,] <- input$out_dynb0; last <- last + 1


    # retro peels ----
    out[last + 1,] <- "# nyrRetro"; last <- last + 1
    out[last + 1,] <- input$nyr_retro; last <- last + 1

    # run options ----
    out[last + 1,] <- paste(input$max_phase, "# Maximum phase (stop the estimation after this phase)", sep = " "); last <- last + 1
    if(is.null(input$max_function_calls)){input$max_function_calls <- -1}
    out[last + 1,] <- paste(input$max_function_calls, "# Maximum number of function calls", sep = " "); last <- last + 1
    if(is.null(input$calc_ref_points)){input$calc_ref_points <- 1}
    out[last + 1,] <- paste(input$calc_ref_points, "# Calculate reference points (0=no)", sep = " "); last <- last + 1
    if(is.null(input$use_pin)){input$use_pin <- 0}
    out[last + 1,] <- paste(input$use_pin, "# use pin file (0=no, 1=yes)", sep = " "); last <- last + 1
    if(is.null(input$verbose)){input$verbose <- 1}
    out[last + 1,] <- paste(input$verbose, "# VERBOSE FLAG (0 = off, 1 = on, 2 = objective func; 3 diagnostics)", sep = " "); last <- last + 1

    ## eof
    out[last + 1,] <- "eof"
    out[last + 1,] <- "9999"


  }

  # write
  writeLines(out[!is.na(out[,1]),], file)

}
