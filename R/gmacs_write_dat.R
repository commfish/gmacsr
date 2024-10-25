#' Write .dat file
#'
#' Write GMACS .dat file in R
#' @param input Named list of data needed to write file (need more info here), look at output of gmacs_read_dat()
#' @param file NULL. File path to write .dat file

#' @return File saved in specified location.
#' @examples gmacs_write_dat(input, file = "./AIGKC/models/2024/may/EAG/23.1b/EAG_23_1b.dat")
#'
#' @export
#'
gmacs_write_dat <- function(input, file = NULL){

  # create output matrix
  out <- matrix(nrow = 1e6)
  last <- 0 # location tracker

  # version ----
  out[last + 1,] <- "##==============================================================="; last <- last + 1
  if(!is.null(input$version)){out[last + 1,] <- paste("# GMACS", input$version, "Data File"); last <- last + 1}
  if(!is.null(input$model_name)){out[last + 1,] <- paste("# Model", input$model_name); last <- last + 1}
  if(is.null(version)){out[last + 1,] <- "GMACS Data File"; last <- last + 1}
  out[last + 1,] <- "##==============================================================="; last <- last + 1
  # dimensions ----
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- paste(input$start_year, "# initial (start year)"); last <- last + 1
  out[last + 1,] <- paste(input$terminal_year, "# terminal (end year)"); last <- last + 1
  out[last + 1,] <- paste(input$n_season, "# Number of seasons"); last <- last + 1
  out[last + 1,] <- paste(input$n_fleets, "# Number of distinct data groups (fleet, among fishing fleets and surveys)"); last <- last + 1
  out[last + 1,] <- paste(input$n_sex, "# Number of sexes"); last <- last + 1
  out[last + 1,] <- paste(input$n_shell, "# Number of shell condition types"); last <- last + 1
  out[last + 1,] <- paste(input$n_maturity, "# Number of maturity types"); last <- last + 1
  out[last + 1,] <- paste(input$n_size_bins, "# Number of size-classes in the model"); last <- last + 1
  out[last + 1,] <- paste(input$recruitment_season, "# Season recruitment occurs"); last <- last + 1
  out[last + 1,] <- paste(input$growth_season, "# Season molting and growth occurs"); last <- last + 1
  out[last + 1,] <- paste(input$ssb_season, "# Season to calculate SSB (changed to match Feb mating)"); last <- last + 1
  out[last + 1,] <- paste(input$n_matrix_season, "# Season for N output"); last <- last + 1
  out[last + 1,] <- "# maximum size-class (males then females)"; last <- last + 1
  out[last + 1,] <- input$max_size_bin; last <- last + 1
  out[last + 1,] <- "# size_breaks (a vector giving the break points between size intervals with dimension nclass+1, lower limits of bins)"; last <- last + 1
  out[last + 1,] <- paste(input$size_bins, collapse = " "); last <- last + 1

  # taus ----
  out[last + 1,] <- "# Natural mortality per season input type (1 = vector by season, 2 = matrix by season/year)"; last <- last + 1
  out[last + 1,] <- input$nat_m_input_type; last <- last + 1
  out[last + 1,] <- "# Proportion of the total natural mortality to be applied each season"; last <- last + 1
  for (i in 1:nrow(input$tau)){

    input$tau %>%
      dplyr::select(2:ncol(.), year) %>%
      mutate(year = paste("#", year)) %>%
      t %>%
      .[,i] %>%
      str_c(., collapse = " ") -> out[last + i,]
  }
  last <- last + nrow(input$tau)

  # fleet names ----
  out[last + 1,] <- "# fleetname"; last <- last + 1
  out[last + 1,] <- paste(input$fleet_names, collapse = " "); last <- last + 1

  # season type ----
  out[last + 1,] <- "#Season type: Set to 1 for continuous F and 0 for instantanous F"; last <- last + 1
  out[last + 1,] <- paste(input$season_type, collapse = " "); last <- last + 1

  # catch data ----
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--CATCH DATA------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$catch_input_format, "#--input catch data format (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_catch_series, "#--Number of catch data frames"); last <- last + 1
  out[last + 1,] <- "# Number of lines for each dataframe (this is not correct for retrospective analyses)"; last <- last + 1
  out[last + 1,] <- paste(input$n_catch_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "##  Type of catch: 1 = retained, 2 = discard, 0= total "; last <- last + 1
  out[last + 1,] <- "##  Units of catch: 1 = biomass, 2 = numbers"; last <- last + 1
  out[last + 1,] <- "## Mult: 1= use data as thy are, 2 = multiply by this number (e.g., lbs to kg)"; last <- last + 1
  # catch data frame
  if(input$catch_input_format == 0){
    #for(i in 1:input$n_catch_series){
    for(i in 1:input$n_catch_series){
      out[last + 1,] <- "#year season fleet sex obs cv type units mult effort discard_mortality"; last <- last + 1
      for(j in 1:input$n_catch_rows[i]){

        input$catch %>%
          mutate(series =  rep(1:input$n_catch_series, input$n_catch_rows)) %>%
          filter(series == i) %>%
          dplyr::select(-series) %>%
          t %>%
          .[,j] %>%
          str_c(., collapse = " ") -> out[last + j,]

      }
      last <- last + input$n_catch_rows[i]
    }
  }

  # index data ----

  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--RELATIVE ABUNDANCE DATA-----------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$index_input_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_index_series, "#--Number of dataframes"); last <- last + 1
  out[last + 1,] <- "# Type of 'survey' catchability (1=Selectivity; 2=Selectivity+Retention), by data frame"; last <- last + 1
  out[last + 1,] <- paste(input$index_type, collapse = " "); last <- last + 1
  out[last + 1,] <- "# Number of data rows, by data frame"; last <- last + 1
  out[last + 1,] <- "#   NOTE: this is not correct for retrospective analyses"; last <- last + 1
  out[last + 1,] <- paste(input$n_index_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "#series year season fleet sex maturity obs cv units timing"; last <- last + 1
  # index data frame
  if(input$index_input_format == 0){
    for(i in 1:input$n_index_rows){
      input$index %>%
        t %>%
        .[,i] %>%
        str_c(., collapse = " ") -> out[last + i,]
    }
  }
  last <- last + nrow(input$index)

  # size data ----

  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--SIZE COMPOSITION DATA-------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$size_input_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_size_series, "#--Number of dataframes"); last <- last + 1
  out[last + 1,] <- "# nSizeCompRows_in"; last <- last + 1
  out[last + 1,] <- paste(input$n_size_rows, collapse = " "); last <- last + 1
  out[last + 1,] <- "# nSizeCompCols_in"; last <- last + 1
  out[last + 1,] <- paste(input$n_size_bin_series, collapse = " "); last <- last + 1
  out[last + 1,] <- "## Sex: 1 = male, 2 = female, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Type of catch: 1 = retained, 2 = discard, 0 = total"; last <- last + 1
  out[last + 1,] <- "## Shell: 1 = newshell, 2 = oldshell, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Maturity: 1 = immature, 2 = mature, 0 = both"; last <- last + 1
  out[last + 1,] <- "## Stage1_EffN (nsamp): the stage-1 effective sample size (this can be modified in the CTL file)"; last <- last + 1
  out[last + 1,] <- " "; last <- last + 1

  # size data frame
  if(input$size_input_format == 0){
    for(i in 1:input$n_size_series){
      out[last + 1,] <- "# year season fleet sex type shell maturity nsamp data"; last <- last + 1
      for(j in 1:input$n_size_rows[i]){

        input$size_comp %>%
          pivot_wider(names_from = size, values_from = obs) %>%
          filter(org_series == i) %>%
          dplyr::select(-org_series) %>%
          t %>%
          .[,j] %>%
          replace(is.na(.), "0.0000") %>%
          str_c(., collapse = " ") -> out[last + j,]

      }
      last <- last + input$n_size_rows[i]
    }
  }

  # growth data ----

  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--GROWTH DATA-----------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$growth_data_type, "# GrowthObsType"); last <- last + 1
  out[last + 1,] <- paste(input$n_growth_obs, "# nGrowthObs"); last <- last + 1
  # growth data frame
  if(input$growth_data_type == 3){
    out[last + 1,] <- "# size-class-at-release, sex, size-class-at-recapture, and time-at-liberty fleet recapture_year number"; last <- last + 1
    for(i in 1:input$n_growth_obs){
      input$growth %>%
        t %>%
        .[,i] %>%
        str_c(., collapse = " ") -> out[last + i,]
    }
    last <- last + nrow(input$growth)
  }

  # environmental data ----

  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "  "; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- "##--ENVIRONMENTAL DATA----------------------------------------"; last <- last + 1
  out[last + 1,] <- "##------------------------------------------------------------"; last <- last + 1
  out[last + 1,] <- paste(input$env_format, "#--input format type (0: old format, 1: new format)"); last <- last + 1
  out[last + 1,] <- paste(input$n_env_series, "#--number of environmental indices "); last <- last + 1

  if(input$n_env_series > 0) {stop("Don't know how to read environmental indices!!!")}

  # eof ----

  out[last + 1,] <- "# eof"; last <- last + 1
  out[last + 1,] <- "9999"; last <- last + 1
  # write
  writeLines(out[!is.na(out[,1]),], file)

}
