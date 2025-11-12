#' Run Retrospective Analysis
#'
#' Run GMACS retrospective analysis, then save and plot results.
#' @param gmacs.dat File path to gmacs.dat file.
#' @param n_peel Number of retrospective peels.
#' @param pin T/F use pin file. Default = F.
#' @param wait Passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously. Default = T.
#' @param plot_mmb T/F make plot of MMB by peel.
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param plot_only  T/F only plot results (analysis already run). Default = F.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.16".

#'
#' @return A subdirectory called 'retrospectives' will be created in the same directory as the gmacs.dat file. Within './retrospectives' a subdirectory corresponding to each peel will be created. The plot will be saved in the provided directory or output as a list if save_plot = F.
#' @examples gmacs_do_retrospective(gmacs.dat = "./AIGKC/models/2024/may/EAG/23.1b/gmacs.dat", n_peel = 10)
#'
#' @export
#'
gmacs_do_retrospective <- function(gmacs.dat, n_peel, pin = F, wait = T, plot_mmb = T, save_plot = T, plot_dir = NULL, plot_only = F, model_name = NULL, version = NULL) {
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # directory ----

  options(warn = -1)
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd

  # analysis ----
  if(plot_only == F) {
    # set up ----

    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    # look for gmacs_file_in.dat - if not present, run gmacs
    # if(!file.exists("./gmacs_files_in.dat")) {setwd(wd); gmacs_do_exe(gmacs.dat, pin = pin, reweight = F)}
    ao_full <- gmacs_read_allout("Gmacsall.out", version = version)
    dat <- gmacs_read_files_dat("gmacs.dat", version = version)
    if(!file.exists(file.path(dat$dat_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$dat_file), "!!"))}
    if(!file.exists(file.path(dat$ctl_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$dat_file), "!!"))}
    if(!file.exists(file.path(dat$prj_file))) {setwd(wd); stop(paste("Cannot find", file.path(dat$prj_file), "!!"))}
    # create retrospectives dir
    dir.create("./retrospectives",recursive = T, showWarnings = F)
    files_to_copy <- c(dat$dat_file, dat$ctl_file, dat$prj_file, "gmacs.exe")
    # make sure pin file is being used as expected
    if(pin == T){
      dat$use_pin <- 1
      if(!file.exists("gmacs.pin")) {setwd(wd); stop("Cannot find gmacs.pin!!"); files_to_copy <- c(files_to_copy, "gmacs.pin")}
    }
    # copy files to retro dir
    file.copy(files_to_copy, "./retrospectives",overwrite = T, recursive = T)
    # save dat file there
    gmacs_write_files_dat(dat, file = "./retrospectives/gmacs.dat")

    # do retrospective runs ----

    setwd("./retrospectives")
    gfiles <- list.files()
    for (i in 1:n_peel){
      # create peel sub-directory
      dir.create(paste0("retro_", i))
      file.copy(gfiles, paste0("retro_", i))
      setwd(paste0("retro_", i))
      # set up gmacs.dat for retro analysis
      dat <- gmacs_read_files_dat("gmacs.dat", version = version)
      dat$nyr_retro <- i
      gmacs_write_files_dat(dat, "gmacs.dat")
      # run gmacs
      shell("gmacs.exe", wait = wait)
      setwd("..")
    }

  }

  if(plot_only == T) {
    ao_full <- gmacs_read_allout("Gmacsall.out", version = version)
    setwd("./retrospectives")
  }


  # plot ----

  if(plot_mmb == F){setwd(wd); return(mohn_rho)}

  if(plot_mmb == T){
    ao <- list()
    for(i in 1:n_peel){
      ao[[i]] <- gmacs_read_allout(file.path(paste0("retro_", i), "Gmacsall.out"), i, version = version)
    }
    setwd(wd) # return to base working directory
    data_summary <- gmacs_get_derived_quantity_summary(ao)

    data_summary %>%
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      left_join(ao_full$derived_quant_summary %>% transmute(year, ssb_full = ssb)) %>%
      filter(year == terminal_yr) %>%
      mutate(rho = (ssb - ssb_full) / ssb_full) %>%
      pull(rho) %>% mean -> mohn_rho

    data_summary %>%
      bind_rows(ao_full$derived_quant_summary) %>%
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = ssb, group = terminal_yr, color = terminal_yr))+
      geom_text_npc(aes(npcx = "right", npcy = "top"),
                    label = latex2exp::TeX(paste("Mohn's $\\rho$ = ", round(mohn_rho, 3))),
                    check_overlap = T, size = 3)+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = paste0("MMB (", unique(data_summary$wt_units), ")"), color = "Terminal Year") -> p_mmb

    if(save_plot == T){
      ggsave(file.path(plot_dir, paste0(model_name,"_retrospective_mmb.png")), plot = p_mmb, height = 3, width = 6)
      return("done")
    }
    if(save_plot == F){return(list(mohn_rho, p_mmb))}

  }
}
