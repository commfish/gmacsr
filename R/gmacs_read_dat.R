#' Read .dat file
#'
#' Load GMACS .dat file in R
#' @param dat_file NULL. Path to .dat file.
#' @param model_name NULL. Character string to save as object in output, later to be used for plot legends. Example: "23.1b".
#' @param version NULL. Character string denoting GMACS version. Default: "2.20.31".

#' @return List of .dat file contents.
#' @examples gmacs_read_dat(dat_file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat", model_name = "23.1b")
#'
#' @export
#'
gmacs_read_dat <- function(dat_file, model_name = NULL, version = NULL) {

  # ggplot theme anticipating plotting later in the workflow
  theme_set(theme_sleek())

  if(is.null(version)){version = "2.20.31"}
  if(version %in% c("2.20.31")){
    # setup ----

    # Suppress the NA message in the coercion to double
    options(warn = -1)

    # read text file
    dat <- read.delim(dat_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()

    # get only data that rows start with a number
    dat_alpha <- filter(dat, grepl("^[A-Za-z]", dat[,1]))
    dat <- filter(dat, !is.na(as.numeric(dat[,1])))

    # model dimensions ----
    ## version
    out$version <- version
    ## model_name
    out$model_name <- model_name
    ## start year
    out$start_year <- as.numeric(dat[1, 1]); last <- 2
    ## terminal year
    out$terminal_year <- as.numeric(dat[last, 1]); last <- last + 1
    ## projection year
    ### need to add...
    ## number of seasons
    out$n_season <- as.numeric(dat[last, 1]); last <- last + 1
    ## number of fleets
    out$n_fleets <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of sexes
    out$n_sex <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of shell condition types
    out$n_shell <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of maturity types
    out$n_maturity <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of size classes
    out$n_size_bins <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season recruitment occurs
    out$recruitment_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season molt / growth occurs
    out$growth_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to calc mmb
    out$ssb_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to output N matrix
    out$n_matrix_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## max size class
    if(out$n_sex == 1) {out$max_size_bin <-  as.numeric(dat[last, 1]); last <- last + 1}
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) > 1){
      out$max_size_bin <- as.numeric(c(dat[last,1], dat[last,2])); last <- last + 1
    }
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) == 1){
      out$max_size_bin[1] <- as.numeric(dat[last, 1]); last <- last + 1
      out$max_size_bin[2] <- as.numeric(dat[last, 1]); last <- last + 1
    }
    ## size bin
    out$size_bins <- as.numeric(dat[last, 1:(out$n_size_bins+1)]); last <- last + 1

    # natural mortality taus ----

    ## natural mortality per season input
    out$nat_m_input_type <- as.numeric(dat[last, 1])
    tmp <- matrix(nrow = length(out$start_year:out$terminal_year), ncol = out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last+i, 1:out$n_season])
    }
    as_tibble(tmp) %>%
      mutate(year = out$start_year:out$terminal_year) %>%
      dplyr::select(ncol(.), 1:out$n_season) %>%
      rename_all(~c("year", paste("season", 1:out$n_season, sep = "_"))) -> out$tau; last <- last + i

    # fleets ----

    out$fleet_names <- as.character(na.omit(as.character(t(as.matrix(dat_alpha[,1:99])))))

    # season instant vs contin ----

    out$season_type <- as.numeric(dat[last+1, 1:out$n_season]); last <- last + 1

    # catch data ----

    ## input format
    out$catch_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## now of catch series
    out$n_catch_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each frame
    out$n_catch_rows <- as.numeric(dat[last+1, 1:out$n_catch_series]); last <- last + 1
    ## catch data
    tmp <- matrix(ncol = 11, nrow = sum(out$n_catch_rows))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "obs", "cv", "type", "units", "mult", "effort", "disc_m")) -> out$catch
    last <- last + nrow(tmp)


    # index data ----

    ## input format
    out$index_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_index_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index type
    out$index_type <- as.numeric(dat[last+1, 1:out$n_index_series]); last <- last + 1
    ## number of rows in total
    out$n_index_rows <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index data
    tmp <- matrix(ncol = 11, nrow = out$n_index_rows)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("series", "year", "season", "fleet", "sex", "maturity", "obs", "cv", "units", "timing", "rai_id")) -> out$index
    last <- last + nrow(tmp)

    ## number of index series
    index_check <- as.numeric(dat[last+1, 1]); last <- last + 1
    if(index_check != 999) {stop("check on rai_id column")}

    # size comps ----

    ## input format
    out$size_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_size_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each series
    out$n_size_rows <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    ## number of bins in each series
    out$n_size_bin_series <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    org_series <- rep(1:out$n_size_series,out$n_size_rows)
    tmp <- matrix(nrow = sum(out$n_size_rows), ncol = 8 + max(out$n_size_bin_series))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "type", "shell", "maturity",
                    "nsamp", out$size_bins[-length(out$size_bins)])) %>%
      mutate(org_series = org_series) %>%
      pivot_longer(9:(ncol(.)-1), names_to = "size", values_to = "obs") %>%
      transmute(org_series, year, season, fleet, sex, type, shell, maturity, nsamp, size, obs) -> out$size_comp

    last <- last + nrow(tmp)

    # growth data ----

    ## input format
    out$growth_data_type <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## input format
    out$n_growth_obs <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## growth data
    ### tagging data
    if(out$growth_data_type == 0){out$growth <- NULL}
    if(out$growth_data_type == 1){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 4)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:4])
      }
      as_tibble(tmp) %>%
        rename_all(~c("size", "increment", "sex", "cv")) -> out$growth
      last <- last + nrow(tmp)
    }
    if(out$growth_data_type == 3){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 8)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:8])
      }
      as_tibble(tmp) %>%
        rename_all(~c("release_class", "sex", "recapture_class", "years_at_liberty", "transition_matrix",
                      "recapture_fleet", "recapture_year", "nsamp")) -> out$growth
      last <- last + nrow(tmp)
    }

    # environmental data ----

    ## input format
    out$env_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of series
    out$n_env_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## eof
    if(as.numeric(dat[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}


  }
  if(version %in% c("2.20.16", "2.20.17", "2.20.19", "2.20.20")){
    # setup ----

    # Suppress the NA message in the coercion to double
    options(warn = -1)

    # read text file
    dat <- read.delim(dat_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()

    # get only data that rows start with a number
    dat_alpha <- filter(dat, grepl("^[A-Za-z]", dat[,1]))
    dat <- filter(dat, !is.na(as.numeric(dat[,1])))

    # model dimensions ----
    ## version
    out$version <- version
    ## model_name
    out$model_name <- model_name
    ## start year
    out$start_year <- as.numeric(dat[1, 1]); last <- 2
    ## terminal year
    out$terminal_year <- as.numeric(dat[last, 1]); last <- last + 1
    ## projection year
    ### need to add...
    ## number of seasons
    out$n_season <- as.numeric(dat[last, 1]); last <- last + 1
    ## number of fleets
    out$n_fleets <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of sexes
    out$n_sex <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of shell condition types
    out$n_shell <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of maturity types
    out$n_maturity <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of size classes
    out$n_size_bins <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season recruitment occurs
    out$recruitment_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season molt / growth occurs
    out$growth_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to calc mmb
    out$ssb_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to output N matrix
    out$n_matrix_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## max size class
    if(out$n_sex == 1) {out$max_size_bin <-  as.numeric(dat[last, 1]); last <- last + 1}
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) > 1){
      out$max_size_bin <- as.numeric(c(dat[last,1], dat[last,2])); last <- last + 1
    }
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) == 1){
      out$max_size_bin[1] <- as.numeric(dat[last, 1]); last <- last + 1
      out$max_size_bin[2] <- as.numeric(dat[last, 1]); last <- last + 1
    }
    ## size bin
    out$size_bins <- as.numeric(dat[last, 1:(out$n_size_bins+1)]); last <- last + 1

    # natural mortality taus ----

    ## natural mortality per season input
    out$nat_m_input_type <- as.numeric(dat[last, 1])
    tmp <- matrix(nrow = length(out$start_year:out$terminal_year), ncol = out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last+i, 1:out$n_season])
    }
    as_tibble(tmp) %>%
      mutate(year = out$start_year:out$terminal_year) %>%
      dplyr::select(ncol(.), 1:out$n_season) %>%
      rename_all(~c("year", paste("season", 1:out$n_season, sep = "_"))) -> out$tau; last <- last + i

    # fleets ----

    out$fleet_names <- as.character(na.omit(as.character(t(as.matrix(dat_alpha[,1:99])))))

    # season instant vs contin ----

    out$season_type <- as.numeric(dat[last+1, 1:out$n_season]); last <- last + 1

    # catch data ----

    ## input format
    out$catch_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## now of catch series
    out$n_catch_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each frame
    out$n_catch_rows <- as.numeric(dat[last+1, 1:out$n_catch_series]); last <- last + 1
    ## catch data
    tmp <- matrix(ncol = 11, nrow = sum(out$n_catch_rows))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "obs", "cv", "type", "units", "mult", "effort", "disc_m")) -> out$catch
    last <- last + nrow(tmp)


    # index data ----

    ## input format
    out$index_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_index_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index type
    out$index_type <- as.numeric(dat[last+1, 1:out$n_index_series]); last <- last + 1
    ## number of rows in total
    out$n_index_rows <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index data
    tmp <- matrix(ncol = 10, nrow = out$n_index_rows)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:10])
    }
    as_tibble(tmp) %>%
      rename_all(~c("series", "year", "season", "fleet", "sex", "maturity", "obs", "cv", "units", "timing")) -> out$index
    last <- last + nrow(tmp)

    # size comps ----

    ## input format
    out$size_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_size_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each series
    out$n_size_rows <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    ## number of bins in each series
    out$n_size_bin_series <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    org_series <- rep(1:out$n_size_series,out$n_size_rows)
    tmp <- matrix(nrow = sum(out$n_size_rows), ncol = 8 + max(out$n_size_bin_series))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "type", "shell", "maturity",
                    "nsamp", out$size_bins[-length(out$size_bins)])) %>%
      mutate(org_series = org_series) %>%
      pivot_longer(9:(ncol(.)-1), names_to = "size", values_to = "obs") %>%
      transmute(org_series, year, season, fleet, sex, type, shell, maturity, nsamp, size, obs) -> out$size_comp

    last <- last + nrow(tmp)

    # growth data ----

    ## input format
    out$growth_data_type <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## input format
    out$n_growth_obs <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## growth data
    ### tagging data
    if(out$growth_data_type == 0){out$growth <- NULL}
    if(out$growth_data_type == 1){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 4)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:4])
      }
      as_tibble(tmp) %>%
        rename_all(~c("size", "increment", "sex", "cv")) -> out$growth
      last <- last + nrow(tmp)
    }
    if(out$growth_data_type == 3){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 8)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:8])
      }
      as_tibble(tmp) %>%
        rename_all(~c("release_class", "sex", "recapture_class", "years_at_liberty", "transition_matrix",
                      "recapture_fleet", "recapture_year", "nsamp")) -> out$growth
      last <- last + nrow(tmp)
    }

    # environmental data ----

    ## input format
    out$env_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of series
    out$n_env_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## eof
    if(as.numeric(dat[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}


  }
  if(version == "2.20.14"){
    # setup ----

    # Suppress the NA message in the coercion to double
    options(warn = -1)

    # read text file
    dat <- read.delim(dat_file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
    # create out object
    out <- list()

    # get only data that rows start with a number
    dat_alpha <- filter(dat, grepl("^[A-Za-z]", dat[,1]))
    dat <- filter(dat, !is.na(as.numeric(dat[,1])))

    # model dimensions ----
    ## version
    out$version <- version
    ## model_name
    out$model_name <- model_name
    ## start year
    out$start_year <- as.numeric(dat[1, 1]); last <- 2
    ## terminal year
    out$terminal_year <- as.numeric(dat[last, 1]); last <- last + 1
    ## projection year
    ### need to add...
    ## number of seasons
    out$n_season <- as.numeric(dat[last, 1]); last <- last + 1
    ## number of fleets
    out$n_fleets <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of sexes
    out$n_sex <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of shell condition types
    out$n_shell <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of maturity types
    out$n_maturity <-  as.numeric(dat[last, 1]); last <- last + 1
    ## number of size classes
    out$n_size_bins <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season recruitment occurs
    out$recruitment_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season molt / growth occurs
    out$growth_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to calc mmb
    out$mmb_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## season to output N matrix
    out$n_matrix_season <-  as.numeric(dat[last, 1]); last <- last + 1
    ## max size class
    if(out$n_sex == 1) {out$max_size_bin <-  as.numeric(dat[last, 1]); last <- last + 1}
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) > 1){
      out$max_size_bin <- as.numeric(c(dat[last,1], dat[last,2])); last <- last + 1
    }
    if(out$n_sex == 2 & length(na.omit(as.numeric(dat[last,]))) == 1){
      out$max_size_bin[1] <- as.numeric(dat[last, 1]); last <- last + 1
      out$max_size_bin[2] <- as.numeric(dat[last, 1]); last <- last + 1
    }
    ## size bin
    out$size_bins <- as.numeric(dat[last, 1:(out$n_size_bins+1)]); last <- last + 1

    # natural mortality taus ----

    ## natural mortality per season input
    out$nat_m_input_type <- as.numeric(dat[last, 1])
    tmp <- matrix(nrow = length(out$start_year:out$terminal_year), ncol = out$n_season)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last+i, 1:out$n_season])
    }
    as_tibble(tmp) %>%
      mutate(year = out$start_year:out$terminal_year) %>%
      dplyr::select(ncol(.), 1:out$n_season) %>%
      rename_all(~c("year", paste("season", 1:out$n_season, sep = "_"))) -> out$tau; last <- last + i

    # fleets ----

    out$fleet_names <- as.character(na.omit(as.character(as.matrix(dat_alpha[,1:6]))))

    # season instant vs contin ----

    out$season_type <- as.numeric(dat[last+1, 1:out$n_season]); last <- last + 1

    # catch data ----

    ## input format
    out$catch_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## now of catch series
    out$n_catch_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each frame
    out$n_catch_rows <- as.numeric(dat[last+1, 1:out$n_catch_series]); last <- last + 1
    ## catch data
    tmp <- matrix(ncol = 11, nrow = sum(out$n_catch_rows))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:11])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "obs", "cv", "type", "units", "mult", "effort", "disc_m")) -> out$catch
    last <- last + nrow(tmp)


    # index data ----

    ## input format
    out$index_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_index_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index type
    out$index_type <- as.numeric(dat[last+1, 1:out$n_index_series]); last <- last + 1
    ## number of rows in total
    out$n_index_rows <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## index data
    tmp <- matrix(ncol = 10, nrow = out$n_index_rows)
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:10])
    }
    as_tibble(tmp) %>%
      rename_all(~c("series", "year", "season", "fleet", "sex", "maturity", "obs", "cv", "units", "timing")) -> out$index
    last <- last + nrow(tmp)

    # size comps ----

    ## input format
    out$size_input_format <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of index series
    out$n_size_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## number of rows in each series
    out$n_size_rows <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    ## number of bins in each series
    out$n_size_bin_series <- as.numeric(dat[last+1, 1:out$n_size_series]); last <- last + 1
    org_series <- rep(1:out$n_size_series,out$n_size_rows)
    tmp <- matrix(nrow = sum(out$n_size_rows), ncol = 8 + max(out$n_size_bin_series))
    for(i in 1:nrow(tmp)){
      tmp[i,] <- as.numeric(dat[last + i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>%
      rename_all(~c("year", "season", "fleet", "sex", "type", "shell", "maturity",
                    "nsamp", out$size_bins[-length(out$size_bins)])) %>%
      mutate(org_series = org_series) %>%
      pivot_longer(9:(ncol(.)-1), names_to = "size", values_to = "obs") %>%
      transmute(org_series, year, season, fleet, sex, type, shell, maturity, nsamp, size, obs) -> out$size_comp

    last <- last + nrow(tmp)

    # growth data ----

    ## input format
    out$growth_data_type <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## input format
    out$n_growth_obs <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## growth data
    ### tagging data
    if(out$growth_data_type == 0){out$growth <- NULL}
    if(out$growth_data_type == 1){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 4)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:4])
      }
      as_tibble(tmp) %>%
        rename_all(~c("size", "increment", "sex", "cv")) -> out$growth
      last <- last + nrow(tmp)
    }
    if(out$growth_data_type == 3){
      tmp <- matrix(nrow = out$n_growth_obs, ncol = 8)
      for(i in 1:nrow(tmp)){
        tmp[i,] <- as.numeric(dat[last+i, 1:8])
      }
      as_tibble(tmp) %>%
        rename_all(~c("release_class", "sex", "recapture_class", "years_at_liberty", "transition_matrix",
                      "recapture_fleet", "recapture_year", "nsamp")) -> out$growth
      last <- last + nrow(tmp)
    }

    # environmental data ----

    ## number of series
    out$n_env_series <- as.numeric(dat[last+1, 1]); last <- last + 1
    ## eof
    if(as.numeric(dat[last+1, 1]) == 9999){return(out)} else{stop("end of file not correct, debug")}


  }

}
