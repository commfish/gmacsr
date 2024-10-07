#' Run gmacs.exe
#'
#' Run gmacs.exe and do iterative reweighing of Francis weights
#' @param gmacs.dat File path to gmacs.dat file
#' @param pin T/F use pin file. Default = F.
#' @param wait Passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
#' @param reweight T/F tune Francis weights. Default = F.
#' @param level Level of convergence for Francis weights. Default = 0.001.
#' @param max_iter Maximum iterations for tuning Francis weights. Default = 100.
#' @param reweight_only T/F do initial GMACS run (F) or skip to iterative reweighting (T).
#'
#' @return Run GMACS, standard GMACS output files.
#' @examples gmacs_do_exe(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat")
#'
#' @export
#'
gmacs_do_exe <- function(gmacs.dat, pin = F, wait = T, reweight = F, level = 0.001, max_iter = 100, reweight_only = F) {

  options(warn = -1)
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  # check for other needed inputs
  if(!file.exists("gmacs.exe")){stop("Cannot find gmacs.exe!!")}
  dat <- readLines("./gmacs.dat")
  if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
  if(pin == T){
    dat[grep("pin", dat)] <- "1 # use pin file (0=no, 1=yes)"
    writeLines(dat, "./gmacs.dat")
    if(!file.exists("gmacs.pin")) {stop(paste("Cannot find gmacs.pin!!"))}
  }
  if(pin == F){
    dat[grep("pin", dat)] <- "0 # use pin file (0=no, 1=yes)"
    writeLines(dat, "./gmacs.dat")
  }
  if(reweight_only == F){
    # run gmacs.exe
    if(wait == F){shell("gmacs.exe", wait = F, intern = F)}else{shell("gmacs.exe")}
  }
  # do reweighting
  if(reweight == T) {

    # check wts convergence first time
    # get lambdas from ctl file
    readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
    # get lambdas from allout file
    readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
    tibble(ctl_wts, rep_wts) %>%
      mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
      pull(diff) %>% sum -> test
    if(test == 0){setwd(wd); return(paste0("wts convergence reached level = ", level))}
    if(test > 0){
      converged <- F
      # turn off reference point calculation
      dat <- readLines("./gmacs_files_in.dat")
      dat[33] <- "0 # Calculate reference points (0=no)"
      writeLines(dat, "./gmacs.dat")
      ctl_file <- dat[6] # ctl file path
      # start a counter
      iteration <- 0
      while(iteration < max_iter && converged == F){
        # change ctl wts
        ctl <- readLines("gmacs_in.ctl")
        ctl[grep("# Lambda for effective sample size", ctl)] <- paste(str_flatten(rep_wts, collapse = " "), "# Lambda for effective sample size")
        writeLines(ctl, ctl_file)
        # run gmacs
        shell("gmacs.exe")
        # test convergence
        readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
        # get lambdas from allout file
        readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
        tibble(ctl_wts, rep_wts) %>%
          mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
          pull(diff) %>% sum -> test
        if(sum(test) == 0) {converged = T}
        iteration <- iteration + 1
      }
      # turn on reference point calculation, run gmacs once more
      dat[33] <- "1 # Calculate reference points (0=no)"
      writeLines(dat, "./gmacs.dat")
      shell("gmacs.exe")
      setwd(wd)
      # done
      if(converged == F) {return(paste0("wts did not reach convergence level = ", level))}
      if(converged == T) {return(paste0("wts convergence reached level = ", level))}

    }

  } else{setwd(wd); return("done!")}

}
