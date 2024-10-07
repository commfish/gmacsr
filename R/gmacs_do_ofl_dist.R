#' Estimate Distribution of OFL
#'
#' Run GMACS with MCMC of reference point calculation.
#' @param gmacs.dat File path to gmacs.dat file.
#' @param n_replicates Length of MCMC chain.
#' @param n_draws Number of draws to save.
#' @param wait Passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously. Default = T.
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.16".

#'
#' @return A subdirectory called 'ofl_distribution' will be created in the same directory as the gmacs.dat file. Within './ofl_distribution' the model version with MCMC will be run. The plot will be saved in the provided directory or output as a list include mcoutREF if save_plot = F.
#' @examples gmacs_do_ofl_dist(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat",n_replicates = 1000000, n_draws = 1000)
#'
#' @export
#'
gmacs_do_ofl_dist <- function(gmacs.dat, n_replicates, n_draws, wait = T, save_plot = T, plot_dir = NULL, model_name = NULL, version = NULL){

  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  options(warn = -1)
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd

  # check for other needed inputs
  #if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
  dat <- gmacs_read_files_dat("./gmacs.dat", version = version)

  if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}

  # set up a separate subdirectory for a run with the mcmc
  dir.create("ofl_distribution")
  files_to_copy <- c("gmacs.exe", dat[2:4], "gmacs.dat")
  file.copy(as.character(files_to_copy), to = file.path("ofl_distribution", files_to_copy), recursive = T)
  setwd("./ofl_distribution")

  # run gmacs with mcmc
  call <- paste0("gmacs -mcmc ", n_replicates, " -mcsave ", n_draws)
  shell(call, wait = wait)
  shell("gmacs -mceval", wait = wait)

  # read output
  mcout <- gmacs_read_mcoutREF(file = "mcoutREF.rep")
  ao <- gmacs_read_allout("../Gmacsall.out", model_name = model_name, version = version)

  # make a plot
  ## ofl pdf
  ggplot()+
    geom_density(data = mcout, aes(x = ofl), fill = "grey80", color = "grey30", alpha = 0.5)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = paste0("OFL (", gsub("_", " ", ao$wt_units),")"), y = "Probability Denisty", fill = NULL)+
    scale_fill_manual(values = cbpalette)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> ofl_pdf
  ## stock status cdf
  mcout %>%
    mutate(mmbprj = ao$mmb_curr,
           status = mmbprj /bmsy) %>%
    ggplot()+
    stat_ecdf(aes(x = status, geom = "step", group = model, color = model))+
    geom_vline(xintercept = 0.5, linetype = 2, color = "firebrick")+
    geom_text(aes(x = 0.48, y = 0.5, label = "Overfished"), size = 4, angle = 90)+
    labs(x = bquote(MMB[prj] ~"/"~B["35%"]), y = "Cumulative Density", color = NULL)+
    scale_color_manual(values = cbpalette)+
    theme(legend.position = c(0.2, 1),
          legend.justification = c(0.2, 1)) -> status_cdf

  plot_out <- ofl_pdf / status_cdf

  # return to base dir
  setwd(wd)

  if(save_plot == F) {
    return(list(mcout = mcout, plot = plot_out))
  }
  if(save_plot == T){ggsave(filename = paste0(plot_dir, "/ref_point_dist.png"),
                            plot = plot_out,
                            height = 6, width = 5, units = "in")
    return("done")}

}
