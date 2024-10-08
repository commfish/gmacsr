#' Kobe Plot
#'
#' Plot F and MMB relative to FMSY and BMSY with FOFL control rule
#' @param all_out Output from gmacs_read_allout() as nested list. Example: all_out = list(mod_23.0a, mod_23.1b).
#' @param save_plot  T/F save plot. Default = T.
#' @param plot_dir  Null. Directory in which to save plot. If NULL, a directory called 'plots' will be created in the same directory as gmacs.dat.
#' @param beta FOFL control rule beta. Default = 0.25.
#' @param alpha FOFL control rule alpha. Default = 0.10.
#' @param spr_target SPR target. Default = 0.35.
#' @param data_summary NULL. Alternate way to bring in data, output of gmacs_get_catch_summary()
#' @param file NULL. File paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param model_name NULL. Character string passed to gmacs_read_allout(). Expressed as character vector, not needed if all.out is provided.
#' @param version NULL. Character string passed to gmacs_read_allout() denoting GMACS version, not needed if all.out is provided.

#' @return Kobe plot
#' @examples gmacs_plot_kobe(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_kobe <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){

  # bring in all out data ----

  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }

  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}

  # extract data
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
           plot = purrr::map(all_out, function(x) {


             # get directed fs
             gmacs_get_f(list(x)) %>%
               filter(sex == "male") %>%
               group_by(year) %>%
               summarise(f = sum(F)) -> fs

             # get mmb time series
             gmacs_get_derived_quantity_summary(list(x)) %>%
               transmute(year, mmb = ssb) -> mmb

             # first year of catch
             gmacs_get_catch_summary(list(x)) %>% pull(year) %>% min -> first_yr

             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot

             # control rule
             tibble(status = seq(0, max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1, 0.0001)) %>%
               mutate(f_ofl = case_when(status <= beta ~ 0,
                                        status > beta & status <= 1 ~ ((status - alpha) / (1 - alpha)),
                                        status > 1 ~ 1)) -> control_rule
             # bacground polygons
             # all coords are top left clockwise
             p_xlim <- max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1
             p_ylim <- max(fs$f[mmb$year >= first_yr])/ftarg * 1.1
             glite <- tibble(x = c(1, p_xlim, p_xlim, 1),
                             y = c(1, 1, 0, 0))
             ylite <- tibble(x = c(1, 1, beta, beta, 1, p_xlim, p_xlim, 1),
                             y = c(1, 0, 0, ((beta - alpha) / (1 - alpha)), p_ylim, p_ylim, 1, 1),
                             group = c(1, 1, 1, 1, 2, 2, 2, 2))
             rlite <- tibble(x = c(0, 1, 1, beta, beta, 0),
                             y = c(p_ylim, p_ylim, 1, ((beta - alpha) / (1 - alpha)), 0, 0))
             vline <- tibble(x = c(1, 1), y = c(0, p_ylim))

             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(year >= first_yr) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = status, y = f_ofl), linetype = 1, size = 1)+
               geom_line(data = vline, aes(x = x, y= y), linetype = 2)+
               geom_polygon(data = glite, aes(x = x, y = y), color = NA, fill = "green", alpha = 0.3)+
               geom_polygon(data = ylite, aes(x = x, y = y, group = group), color = NA, fill = "yellow", alpha = 0.3)+
               geom_polygon(data = rlite, aes(x = x, y = y), color = NA, fill = "red", alpha = 0.3)+
               geom_point(aes(x = mmb/btarg, y = f/ftarg))+
               geom_text(aes(x = mmb/btarg, y = f/ftarg, label = substring(year, 3, 4)), size = 2, nudge_y = 0.05)+
               geom_path(aes(x = mmb/btarg, y = f/ftarg), size = 0.2)+
               geom_point(data = function(x){filter(x, year == max(x$year))},
                          aes(x = mmb/btarg, y = f/ftarg), size = 3, shape = 21, fill = "white")+
               labs(x = bquote(B/B[.(spr_targ*100)~"%"]), y = bquote("F/"~F[.(spr_targ*100)~"%"])) -> p

             if(save_plot == T) {
               ggsave(plot = p,
                      filename = file.path(plot_dir, paste0(x$model_name, "_kobe.png")),
                      height = 4, width = 5, units = "in")
             }

             return(p)

           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}


}
