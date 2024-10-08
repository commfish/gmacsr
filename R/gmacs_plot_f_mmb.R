#' Plot F and MMB
#'
#' Plot F and MMB with FOFL control rule
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

#' @return Plot of F and MMB with FOFL control rule
#' @examples gmacs_plot_f_mmb(all_out = list(model_25.0))
#'
#' @export
#'
gmacs_plot_f_mmb <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){

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

             # set up mmb vector for fofl control rule line
             b <- 0:max(mmb$mmb)

             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot

             # control rule
             tibble(b = b) %>%
               mutate(f_ofl = case_when(b / btarg <= beta ~ 0,
                                        b / btarg > beta & b/btarg <= 1 ~ ftarg * ((b/btarg - alpha) / (1 - alpha)),
                                        b / btarg > 1 ~ ftarg)) -> control_rule

             # plot annotation
             annotation <- paste0("F", spr_targ*100, " = ", round(ftarg, 3), "\nFOFL = ", round(x$f_ofl_tot, 3))

             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(f > 0) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = b, y = f_ofl), size = 1)+
               geom_text(aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5)+
               geom_text(data = function(x){filter(x, year == max(x$year))},
                         aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5, color = "firebrick")+
               geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation),
                             check_overlap = T, size = 3)+
               scale_x_continuous(labels = scales::comma)+
               labs(x = paste0("MMB (", x$wt_units, ")"), y = "F") -> p

             if(save_plot == T) {
               ggsave(plot = p,
                      filename = file.path(plot_dir, paste0(x$model_name, "_f_mmb.png")),
                      height = 4, width = 5, units = "in")
             }

             return(p)

           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}


}
