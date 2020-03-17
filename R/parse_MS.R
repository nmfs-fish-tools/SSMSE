# Parse management strageties.


#' Parse management strategy options
#' 
#' This function matches each management strategy with its correct method. And
#' checks for errors.
#' @template MS
#' @param EM_out_dir Relative or absolute path to the estimation model, if using a 
#'   model outside of the SSMSE package. Note that this value should be NULL if
#'   \code{MS} has a value other than \code{"EM"}.
#' @param init_loop Logical. If this is the first initialization loop of the 
#'   MSE, \code{init_loop} should be TRUE. If it is in further loops, it should
#'   be FALSE.
#' @param OM_dat An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @template verbose
#' @param nyrs_assess The number of years between assessments. E.g., if an
#'   assessment is conducted every 3 years, put 3 here. A single integer value.
#' @param dat_yrs Which years should be added to the new model? Ignored if
#'  init_loop is TRUE.
#' @param dat_str A optional list including which years and fleets should be 
#'  added from the OM into the EM for different types of data. If NULL, the data
#'  structure will try to be infered from the pattern found for each of the 
#'  datatypes within EM_datfile. Ignored if init_loop is TRUE.
#' @author Kathryn Doering
#' @importFrom r4ss SS_readstarter SS_writestarter SS_writedat

parse_MS <- function(MS, EM_out_dir = NULL, init_loop = TRUE, 
                     OM_dat, verbose = FALSE, nyrs_assess, dat_yrs,
                     dat_str) {
  if(verbose) {
    message("Parsing the management strategy.")
  }
  # input checks ----
  valid_MS <- c("EM", "no_catch", "last_yr_catch")
  if (!MS %in% valid_MS) {
    stop("MS was input as ", MS, ", which is not valid. Valid options: ",
         valid_MS)
  }
  if(!is.null(EM_out_dir)) check_dir(EM_out_dir) # make sure contains a valid model
  # parsing management strategies ----
  # EM ----
  if(MS == "EM") {
    check_dir(EM_out_dir)
    new_datfile_name <- "init_dat.ss"
    if(init_loop) {
    # copy over raw data file from the OM to EM folder
    SS_writedat(OM_dat, 
                      file.path(EM_out_dir, new_datfile_name), 
                      overwrite = TRUE, 
                      verbose = FALSE)
    # change the name of data file.
    start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"), 
                                  verbose = FALSE)
    orig_datfile_name <- start$datfile # save the original data file name.
    start$datfile <- new_datfile_name
    SS_writestarter(start, file.path(EM_out_dir), verbose = FALSE,
                    overwrite = TRUE, warn = FALSE)
    # make sure the data file has the correct formatting (use existing data 
    #file in the EM directory to make sure)??
    new_EM_dat <- change_dat(OM_datfile = new_datfile_name,
                             EM_datfile = orig_datfile_name,
                             EM_dir = EM_out_dir,
                             do_checks = TRUE,
                             verbose = verbose)
    } else {
      if(!is.null(dat_str)) {
        dat_str_sub <- lapply(dat_str,
                              function(df, y) df[df[,1] %in% y, ],
                              y = dat_yrs - nyrs_assess)
      } else {
        dat_str_sub <- NULL
      }
      new_EM_dat <- add_new_dat(OM_dat = OM_dat,
                                 EM_datfile = new_datfile_name,
                                 dat_str = dat_str_sub,
                                 EM_dir = EM_out_dir,
                                 do_checks = TRUE,
                                 new_datfile_name = new_datfile_name,
                                 verbose = verbose)
    }
    # manipulate the forecasting file.
    # make sure enough yrs can be forecasted.
    fcast <- SS_readforecast(file.path(EM_out_dir, "forecast.ss"),
                             readAll = TRUE,
                             verbose = FALSE)
    # check that it can be used in the EM. fleets shoul
    check_EM_forecast(fcast,
      n_flts_catch = length(which(new_EM_dat[["fleetinfo"]][, "type"] %in%
                                    c(1,2))))
    if(init_loop) {
    fcast <- change_yrs_fcast(fcast, 
                              make_yrs_rel = TRUE, 
                              nyrs_fore = nyrs_assess, 
                              mod_styr = new_EM_dat[["styr"]], 
                              mod_endyr = new_EM_dat[["endyr"]])
    } else {
      fcast <- change_yrs_fcast(fcast, 
                       make_yrs_rel = FALSE,
                       nyrs_increment = nyrs_assess,
                       mod_styr = new_EM_dat[["styr"]],
                       mod_endyr = new_EM_dat[["endyr"]])
    }                     
    SS_writeforecast(fcast, dir = EM_out_dir, writeAll = TRUE, overwrite = TRUE,
                     verbose = FALSE)
    # given all checks are good, run the EM
    # check convergence (figure out way to error if need convergence)
    # get the future catch using the management strategy used in the SS model.
    run_EM(EM_dir = EM_out_dir, verbose = verbose, check_converged = TRUE)
    new_catch_list <- get_EM_catch_df(EM_dir = EM_out_dir, dat = new_EM_dat)
  }
  # last_yr_catch ----
  # no_catch ----
  if(MS %in% c("last_yr_catch", "no_catch")) {
    if(OM_dat[["N_discard_fleets"]] > 0) {
      warning("The management strategy used ", MS, " assumes no discards, ", 
              "although there are discards included in the OM. Please use ", 
              "MS = 'EM' if you want to include discards in your management ", 
              "strategy.")
    }
    new_catch_list <- get_no_EM_catch_df(OM_dat$catch,
                      yrs = (OM_dat$endyr+1):(OM_dat$endyr+nyrs_assess),
                      MS = MS)
  }
  # check output before returning
  check_catch_df(new_catch_list[["catch"]])
  # TODO add check for discard?
  new_catch_list
}

#' Get the EM catch data frame
#' 
#' Get the data frame of catch for the next iterations when using a Stock
#' Synthesis Estimation model from the Report.sso file.
#' @param EM_dir Path to the EM files
#' @param dat A SS datfile read into R using \code{r4ss::SS_readdat()}
#' @return A data frame of future catch
get_EM_catch_df <- function(EM_dir, dat) {
  rpt <- readLines(file.path(EM_dir, "Report.sso"))
  start <- grep("TIME_SERIES", rpt)
  start <- start[length(start)]+1
  end <- grep("SPR_series", rpt)
  end <- end[length(end)]-1
  # sanity checks to catch false assumptions about number of times the 
  # expressions occur
  assertive.properties::assert_is_of_length(start, 1)
  assertive.properties::assert_is_of_length(end, 1)
  # make into a data frame.
  future_catch <- rpt[start:end]
  hdr <- strsplit(future_catch[1], split = " ", fixed = TRUE)[[1]]
  hdr <- hdr[-grep("^$", hdr)]
  catch_dat <- strsplit(future_catch[-1], split = " ", fixed = TRUE)
  catch_dat <- lapply(catch_dat, function(x) x[x != ""])
  catch_df <- do.call("rbind", catch_dat)
  colnames(catch_df) <- hdr
  catch_df <- utils::type.convert(as.data.frame(catch_df), as.is = TRUE)
  fcast_catch_df <- catch_df[catch_df$Era == "FORE", ]
  # get the fleets and the units on catch 
  units <- dat[["fleetinfo"]]
  units$survey_number <- seq_len(nrow(units))
  flt_units <- units[units$type %in% c(1,2), c("survey_number", "units")] 
  # for multi area model, need to add area
  # may also need to consider if the catch multiplier is used..
  # match catch with the units
  unit_key <- data.frame(unit_name = c("B", "N"), units = 1:2) 
  flt_units <- merge(flt_units, unit_key, all.x = TRUE, all.y = FALSE)
  # get the se
  se <- get_input_value(dat$catch, method = "most_common_value", 
                        colname = "catch_se", group = "fleet")
  df_list <- vector(mode = "list", length = nrow(flt_units))
  bio_df_list <- vector(mode = "list", length = nrow(flt_units))
  F_df_list <- vector(mode = "list", length = nrow(flt_units))
  for(fl in seq_len(nrow(flt_units))) {
    # find which row to get fleet unit catch from. Right now, assume selected = retained,
    # i.e., no discards.
    tmp_col_lab <- paste0("retain(", flt_units$unit_name[fl], "):_", 
                         flt_units$survey_number[fl])
    #Get the retained biomass for all fleets regardless of units to allow checking with population Biomass
    tmp_col_lab_bio <- paste0("retain(B):_", 
                          flt_units$survey_number[fl])
    #Get the fleet appical F's to allow identification of unrealistically high fishing effort which may be a
    #better check for MSE than just single year catch larger than the population.
    tmp_col_lab_F <- paste0("F:_", 
                          flt_units$survey_number[fl])
    #find the se that matches for the fleet
    tmp_catch_se <- se[se$fleet == flt_units$survey_number[fl], "catch_se"]
    if(length(tmp_catch_se) == 0) {
      if(all(fcast_catch_df[, tmp_col_lab] == 0)) {
        tmp_catch_se <- 0.1 #assign an arbitrary value, b/c no catch
      } else {
        stop("Problem finding catch se to add to future catch df for fleet ", 
             flt_units$survey_number[fl], ".")
      }
    }
    df_list[[fl]] <- data.frame(year     = fcast_catch_df$Yr,
                                seas     = fcast_catch_df$Seas, 
                                fleet    = flt_units$survey_number[fl], 
                                catch    = fcast_catch_df[, tmp_col_lab],
                                catch_se = tmp_catch_se)
    
    bio_df_list[[fl]] <- data.frame(year     = fcast_catch_df$Yr,
                                seas     = fcast_catch_df$Seas, 
                                fleet    = flt_units$survey_number[fl], 
                                catch    = fcast_catch_df[, tmp_col_lab_bio])
    
    F_df_list[[fl]] <- data.frame(year     = fcast_catch_df$Yr,
                                seas     = fcast_catch_df$Seas, 
                                fleet    = flt_units$survey_number[fl], 
                                catch    = fcast_catch_df[, tmp_col_lab_F])
  }
  catch_df <- do.call("rbind", df_list)
  catch_bio_df <- do.call("rbind", bio_df_list)
  catch_F_df <- do.call("rbind", F_df_list)
  
  # get discard, if necessary
  if(dat[["N_discard_fleets"]] > 0) {
    # discard units: 1, biomass/number according to set in catch
    # 2, value are fraction (biomass/numbers ) of total catch discarded
    # 3, values are in numbers(thousands)
    se_dis <- get_input_value(dat$discard_data, method = "most_common_value", 
                          colname = "Std_in", group = "Flt")
    dis_df_list <- vector(mode = "list", 
                          length = nrow(dat[["discard_fleet_info"]]))
    for (i in seq_along(dat[["discard_fleet_info"]][, "Fleet"])) {
      tmp_flt <- dat[["discard_fleet_info"]][i, "Fleet"]
      # tmp_units_code can be 1, 2, or 3.
      tmp_units_code <- dat[["discard_fleet_info"]][i, "units"]
      # get the discard units
      tmp_discard_units <- ifelse(dat[["fleetinfo"]][tmp_flt, "units"] == 1, "B" , "N")
      if(tmp_units_code == 3) tmp_discard_units <- "N"
      tmp_cols <- paste0(c("retain(", "sel("), tmp_discard_units, "):_", 
                         tmp_flt)
      tmp_discard_amount <- fcast_catch_df[,tmp_cols[2]] -
                            fcast_catch_df[,tmp_cols[1]]
      if(tmp_units_code == 2) { # get discard as a fractional value.
        tmp_discard_amount <- tmp_discard_amount/fcast_catch_df[,tmp_cols[2]]
      }
      if(sum(tmp_discard_amount) == 0) {
        dis_df_list[[i]] <- NULL
      } else {
        # check that an se was created for that fleet (a sanity check)
        if(length(se_dis[se_dis$Flt == tmp_flt, "Std_in"]) == 0) {
          stop("A standard error value for fleet ", tmp_flt, "could not be ",
               "determined because there was no discard data for that fleet in ",
               "the data file input to the EM. Please add discarding data for ",
               "the fleet to the OM data file or contact the developers for ",
               "assistance with this problem.")
        }
        dis_df_list[[i]] <- data.frame( Yr = fcast_catch_df$Yr ,
                              Seas = fcast_catch_df$Seas,
                              Flt = tmp_flt,
                              Discard = tmp_discard_amount,
                              Std_in = se_dis[se_dis$Flt == tmp_flt, "Std_in"] )
      }
    }
    dis_df <- do.call("rbind", dis_df_list)
  } else {
    dis_df <- NULL
  }
  new_dat_list <- list(catch = catch_df,
                       discards = dis_df,
                       catch_bio = catch_bio_df,
                       catch_F = catch_F_df)
}

#' Get the data frame of catch for the next iterations when not using an
#' estimation model.
#'
#'@param catch Catch dataset from the OM, as read in using the catch
#' dataframe (a list component) created using r4ss::SS_readdat
#'@param yrs A vector of years for each year
#'@param MS Can be either "no_catch" or "last_yr_catch"
#'@return A dataframe of future catch.
get_no_EM_catch_df <- function(catch, yrs, MS = "last_yr_catch") {
  # input checks
  if(! MS %in% c("last_yr_catch","no_catch")) {
    stop("MS specified as '", MS, "', but must be either 'last_yr_catch' or ", 
         "'no_catch'")
  }
  assertive.types::assert_is_data.frame(catch)
  assertive.properties::assert_is_atomic(yrs)
  # get the catch values by MS.
  l_yr <- max(catch$year)
  catch_by_fleet <- catch[catch$year == l_yr, c("fleet", "seas", "catch")]
  if(MS == "no_catch") catch_by_fleet$catch <- 0
  # find combinations of catch needed seas and fleet
  flt_combo <- unique(catch[,c("seas","fleet")])
  se <- get_input_value(catch, method = "most_common_value", colname = "catch_se", 
                  group = "fleet")
  tmp_df_list <- vector(mode = "list", length = length(yrs)*nrow(flt_combo))
  tmp_bio_df_list <- vector(mode = "list", length = length(yrs)*nrow(flt_combo))
  pos <- 1
  for(y in yrs) {
    for(flt in seq_len(nrow(flt_combo))) {
      #get the catch value
      tmp_catch <- catch_by_fleet[catch_by_fleet$fleet == flt_combo$fleet[flt] &
                                  catch_by_fleet$seas == flt_combo$seas[flt],
                                  "catch"]
      #get the catch_se
      tmp_catch_se <- se[se$fleet == flt_combo$fleet[flt], "catch_se"]
      tmp_df_list[[pos]] <- data.frame(year = y,
                                       seas = flt_combo$seas[flt], 
                                       fleet = flt_combo$fleet[flt], 
                                       catch = tmp_catch, 
                                       catch_se = tmp_catch_se)
      pos <- pos + 1
    }
  }
  df <- do.call("rbind", tmp_df_list)
  return_list <- list(catch = df,
                      discards = NULL)# assume no discards if using simple MS.
}