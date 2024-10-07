#' Run Jitter Analysis
#'
#' Run GMACS jitter analysis, then save and plot results.
#' @param gmacs.dat File path to gmacs.dat file.
#' @param sd Jitter standard deviation.
#' @param iter Number of jittering runs.
#' @param wait Passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously. Default = T.
#' @param save_csv  T/F save csv file output. Default = T.
#' @param csv_dir  Null. Directory in which to save output. If NULL, a directory called 'output' will be created in the same directory as gmacs.dat.
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param plot_only  T/F only plot results (analysis already run). Default = F.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.16".

#'
#' @return A subdirectory called 'jitter' will be created in the same directory as the gmacs.dat file. Within './jitter' a subdirectory corresponding to each run will be created. The output and plot will be saved in the provided directories or output as a list if save_csv and save_plot = F.
#' @examples gmacs_do_jitter(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat", sd = 0.3, iter = 100)
#'
#' @export
#'
gmacs_do_jitter <- function(gmacs.dat, sd, iter, wait = T, save_csv = T, csv_dir = NULL, save_plot = T, plot_dir = NULL, plot_only = F, model_name = NULL, version = NULL) {

  # create output directories
  if(save_csv == T & is.null(csv_dir)) {csv_dir <- file.path(dirname(gmacs.dat), "output"); dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # directory ----

  options(warn = -1)
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd

  if(plot_only == F){

    # set up ----

    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    dat <- gmacs_read_files_dat("./gmacs.dat", version = version)

    if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
    # make sure pin file is being not being usedused as expected
    dat$use_pin <- 0
    # turn on reference points
    dat$calc_ref_points <- 1
    # set up jitter
    dat$jitter <- 1
    dat$jitter_sd <- sd

    # do jitter ----

    # create subdirectory for jitter run files
    dir.create("./jitter")
    # rewrite gmacs.dat
    gmacs_write_files_dat(dat, file = "./jitter/gmacs.dat")
    # put files in - this likely will not work with relative pathes
    file.copy(c(dat$dat_file, dat$ctl_file, dat$prj_file, "gmacs.exe"),
              to = "./jitter")
    # set working
    setwd("./jitter")
    # names of necessary gmacs files
    gfiles <- list.files()

    # do jitter runs
    out <- tibble(iteration = 1:iter,
                  obj_function = NA,
                  max_gradient = NA,
                  catch_lik = NA,
                  index_lik = NA,
                  size_lik = NA,
                  mmb_curr = NA,
                  bmsy = NA,
                  ofl = NA)
    for (i in 1:iter) {
      rundir <- paste0("./run_", i)
      dir.create(rundir)
      file.copy(from = gfiles, to = rundir)
      # do gmacs run
      setwd(rundir)
      while(!("gmacs.rep" %in% list.files())){shell("gmacs.exe", wait = wait)}
      ao <- gmacs_read_allout("./Gmacsall.out", version = version)
      out$obj_function[i] <- ao$objective_function
      out$max_gradient[i] <- ao$max_gradient
      out$catch_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "catch"]
      out$index_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "index"]
      out$size_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "size"]
      out$mmb_curr[i] <- ao$mmb_curr
      out$bmsy[i] <- ao$bmsy
      out$ofl[i] <- ao$ofl_tot

      setwd("..")
    }
    out <- out %>% dplyr::select(where(function(x) !all(is.na(x))))
    # return to model directory
    setwd("..")

  }

  # get mle estimates of objects
  mle_ao <- gmacs_read_allout("./Gmacsall.out", model_name = model_name, version = version)
  # set wd back to original
  setwd(wd)

  # plots ----

  if(plot_only == T){out <- read_csv(paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}

  # obj fxn
  ggplot()+
    geom_histogram(data = out, aes(x = obj_function), color = 1, fill = "grey80",
                   width = 1)+
    geom_vline(xintercept = mle_ao$objective_function, linetype = 2, color = 2)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = "Negative Log-likelihood", y = "Jitter Runs") -> p_obj

  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$mmb_curr))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$mmb_curr), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = paste0("MMB (", gsub("_", " ", mle_ao$wt_units), ")") ) -> p_mmb

  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$bmsy))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$bmsy), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = bquote(B["MSY"]~"("~.(gsub("_", " ", mle_ao$wt_units))~")") ) -> p_bmsy

  ggplot()+
    geom_point(aes(x = out$obj_function, y = out$ofl))+
    geom_point(aes(x = mle_ao$objective_function, y = mle_ao$ofl_tot), size = 2, shape = 21, fill = "white")+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    labs(x = "Negative Log-likelihood", y = paste0("OFL (", gsub("_", " ", mle_ao$wt_units), ")") ) -> p_ofl


  if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".png"),
                            plot = (p_obj + p_mmb) / (p_bmsy + p_ofl),
                            height = 6, width = 8, units = "in")}

  # output ----
  plots <- list(p_obj, p_mmb, p_bmsy, p_ofl)
  if(save_csv == T) {write_csv(out, paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}

  if(save_plot == F){return(c(list(out), plots))}else{return(out)}

}
