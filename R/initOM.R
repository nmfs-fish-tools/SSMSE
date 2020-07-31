# Functions associated with initialization of the OM.

#' Create the OM by adding dummy data
#'
#' This function manipulates the OM as needed so that it can be used as an
#'  operating model.
#' @author Kathryn Doering & Nathan Vaughan
#' @param OM_out_dir The full path to the directory in which the OM is run.
#' @param overwrite Overwrite existing files with matching names?
#' @param add_dummy_dat Should dummy data be added to indices and comps for each
#'  year so that expected values and sampling is obtained for years other than
#'  those that already have data? Defaults to FALSE.
#' @param nyrs_assess The number of years between assessments. This is used to
#'  structure the forecast file for use in the OM.
#' @param writedat Should a new datafile be written? Only used if add_dummy_dat
#'  is \code{TRUE}.
#' @param rec_devs Vector of recruitment deviations for simulation.
#' @param verify_OM Should the model be run without estimation and some basic
#'  checks done to verify that the OM can run? Defaults to TRUE.
#' @template verbose
#' @return A new datafile as read in for r4ss, but with dummy data added.
#' @import r4ss
create_OM <- function(OM_out_dir,
                      overwrite = TRUE,
                      add_dummy_dat = FALSE,
                      writedat = TRUE,
                      verbose = FALSE,
                      nyrs_assess = NULL,
                      rec_devs = NULL,
                      verify_OM = TRUE) {
  start <- r4ss::SS_readstarter(file.path(OM_out_dir, "starter.ss"),
                                verbose = FALSE)
  # modify starter to use as OM ----
  start$init_values_src <- 1
  start$detailed_age_structure <- 1
  start$last_estimation_phase <- 0
  start$depl_basis <- 0
  start$depl_denom_frac <- 1
  start$SPR_basis <- 0
  start$F_report_units <- 0
  start$F_report_basis <- 0
  start$F_age_range <- NULL
  r4ss::SS_writestarter(start, dir = OM_out_dir, verbose = FALSE,
                        overwrite = TRUE, warn = FALSE)
  # run model to get standardized output ----
  run_ss_model(OM_out_dir, "-maxfn 0 -phase 50 -nohess", debug_par_run = TRUE,
               verbose = verbose)
  # read in files to use ----
  dat <- r4ss::SS_readdat(file = file.path(OM_out_dir, start$datfile),
                          verbose = FALSE, section = 1)
  forelist <- r4ss::SS_readforecast(file = file.path(OM_out_dir, "forecast.ss"),
                                    readAll = TRUE, verbose = FALSE)
  ctl <- r4ss::SS_readctl(file.path(OM_out_dir, start$ctlfile), verbose = FALSE,
                          use_datlist = TRUE, datlist = dat)
  outlist <- r4ss::SS_output(OM_out_dir, verbose = FALSE, printstats = FALSE,
                             covar = FALSE)
  parlist <- r4ss::SS_readpar_3.30(parfile = file.path(OM_out_dir, "ss.par"),
                                   datsource = dat, ctlsource = ctl,
                                   verbose = FALSE)
  
  # modify forecast file ----
  currentNforecast <- forelist[["Nforecastyrs"]]
  forelist[["Nforecastyrs"]] <- nyrs_assess
  forelist[["FirstYear_for_caps_and_allocations"]] <- dat[["endyr"]] + nyrs_assess + 1
  forelist[["InputBasis"]] <- 3
  forelist[["ControlRuleMethod"]] <- 1
  forelist[["BforconstantF"]] <- 0.001
  forelist[["BfornoF"]] <- 0.0001
  forelist[["Flimitfraction"]] <- 1 
  
  # convert forecast year selectors to standard form
  for(i in 1:6) {
    x <- forelist[["Fcast_years"]][i]
    if (forelist[["Fcast_years"]][i] == dat[["styr"]]) {
      forelist[["Fcast_years"]][i] <- -999
    } else if (forelist[["Fcast_years"]][i] == dat[["endyr"]]) {
      forelist[["Fcast_years"]][i] <- 0
    } else if (forelist[["Fcast_years"]][i] > dat[["styr"]] &
               forelist[["Fcast_years"]][i] < dat[["endyr"]]) {
      forelist[["Fcast_years"]][i] <- forelist[["Fcast_years"]][i] - dat[["endyr"]] # make it relative to endyr
    } else {
      stop("Year in fcast file out of range. Please change to be within ",
           "start and end yrs. Check Fcast_years")
    }
  }
  # put together a Forecatch dataframe using retained catch as a starting point for the OM
  # this would only matter if an EM assessment is not run in the first year. 
  units_of_catch <- dat$fleetinfo[dat$fleetinfo$type %in% c(1, 2), "units"]
  names(units_of_catch) <- as.character(which(dat$fleetinfo$type %in% c(1, 2)))
  ret_catch <- get_retained_catch(timeseries = outlist$timeseries,
        units_of_catch = units_of_catch)
  temp_fore <- ret_catch[ret_catch$Era == "FORE",
                         c("Yr", "Seas", "Fleet", "retained_catch")]
  row.names(temp_fore) <- NULL
  names(temp_fore) <- c("Year", "Seas", "Fleet", "Catch or F")
  # only put values in that are in the Fcas year range.
  forelist$ForeCatch <- temp_fore[
    is.element(temp_fore$Year, (dat$endyr + 1):(dat$endyr + nyrs_assess)), ]

  # modify ctl file ----
  # in the context of an OM, do not want to use the bias adjustment ramp, so just
  # turn off and make the recdevs years always the same.
  # UPDATE NOTE: For the OM we do not want bias adjustment in the future. However we
  # do want the historic period to be consistent with the original assessment model.
  # We therefore need to add advanced options if not already specified. I am also 
  # updating the extend EM process to fix the main recdevs end year. This way all new
  # recdevs become late phase/forecast recdevs which are not subject to sum to zero 
  # constraints or bias adjustment.
  if(ctl[["recdev_adv"]]==0){
    ctl[["recdev_adv"]] <- 1
    ctl[["recdev_early_start"]] <- 0
    ctl[["recdev_early_phase"]] <- -4
    ctl[["Fcast_recr_phase"]] <- 0
    ctl[["lambda4Fcast_recr_like"]] <- 1
    ctl[["last_early_yr_nobias_adj"]] <- ctl[["MainRdevYrFirst"]]-1
    ctl[["first_yr_fullbias_adj"]] <- ctl[["MainRdevYrFirst"]]
    ctl[["last_yr_fullbias_adj"]] <- ctl[["MainRdevYrLast"]]
    ctl[["first_recent_yr_nobias_adj"]] <- ctl[["MainRdevYrLast"]]+1
    ctl[["max_bias_adj"]] <- 0.8
    ctl[["period_of_cycles_in_recr"]] <- 0
    ctl[["min_rec_dev"]] <- -10
    ctl[["max_rec_dev"]] <- 10
    ctl[["N_Read_recdevs"]] <- 0
    ctl[["recdev_input"]] <- NULL
  }
  
  if(ctl[["recdev_early_start"]]<=0){
   first_year <- ctl[["MainRdevYrFirst"]] + ctl[["recdev_early_start"]]
  }else if(ctl[["recdev_early_start"]]<ctl[["MainRdevYrFirst"]]){
    first_year <- ctl[["recdev_early_start"]]
  }else(
    first_year <- ctl[["MainRdevYrFirst"]]
  )
  # modify par file ----
  all_recdevs <- as.data.frame(rbind(parlist[["recdev1"]], parlist[["recdev2"]], parlist[["recdev_forecast"]]))
  # get recdevs for all model yeasrs
  all_recdevs <- all_recdevs[all_recdevs$year >= first_year & all_recdevs$year <= (dat$endyr + forelist$Nforecastyrs), ]
  #new_recdevs_df <- data.frame(year = dat$styr:dat$endyr, recdev = NA)
  new_recdevs_df <- data.frame(year = first_year:ctl[["MainRdevYrLast"]], recdev = NA)
  fore_recdevs_df <- data.frame(year = (ctl[["MainRdevYrLast"]]+1):(dat$endyr + forelist$Nforecastyrs), recdev = NA)
  for (i in seq_along(first_year:(dat$endyr + forelist$Nforecastyrs))) {
    
    tmp_yr <- (first_year:(dat$endyr + forelist$Nforecastyrs))[i]
    if(tmp_yr<=ctl[["MainRdevYrLast"]]){
      step<-i
    if(length(all_recdevs[all_recdevs$year == tmp_yr, "year"]) == 0) {
      new_recdevs_df[i,"recdev"] <- 0 # just assume no recdevs
    } else {
      new_recdevs_df[i,"recdev"] <-
        all_recdevs[all_recdevs$year == tmp_yr, "recdev"]
    }
    }else if(tmp_yr<=dat$endyr){
      if(length(all_recdevs[all_recdevs$year == tmp_yr, "year"]) == 0) {
        fore_recdevs_df[(i-step),"recdev"] <- 0
      } else {
        fore_recdevs_df[(i-step),"recdev"] <-
          all_recdevs[all_recdevs$year == tmp_yr, "recdev"]
      }
    }else{
      temp_recs<-get_rec_devs_matrix(yrs = tmp_yr,
                                 rec_devs = rec_devs)
      if (length(temp_recs[,"recdev"])==1)  {
        fore_recdevs_df[(i-step),"recdev"] <- temp_recs[1, "recdev"]  
      }else if(length(all_recdevs[all_recdevs$year == tmp_yr, "year"]) == 0) {
        fore_recdevs_df[(i-step),"recdev"] <- 0
      } else {
        fore_recdevs_df[(i-step),"recdev"] <-
          all_recdevs[all_recdevs$year == tmp_yr, "recdev"]
      }
    }
  }
  new_recdevs_mat <- as.matrix(new_recdevs_df)
  new_fore_recdevs_mat <- as.matrix(fore_recdevs_df)
  if(!is.null(parlist[["recdev1"]])) {
    parlist[["recdev1"]] <- new_recdevs_mat
  } else if (!is.null(parlist[["recdev2"]])) {
    parlist[["recdev2"]] <- new_recdevs_mat
  } else {
    stop("no recdevs in initial OM model")
  }
  
  # use report.sso time series table to find the F's to put into the parlist.
  F_list <- get_F(timeseries = outlist$timeseries,
    fleetnames = dat$fleetinfo[dat$fleetinfo$type %in% c(1, 2), "fleetname"])
  # modify the init_F and F_rate in parlist if used.
  # note that F_list[["F_rate"]] and F_list[["F_init]] are NULL if they had 0
  # rows.
  parlist[["F_rate"]] <- F_list[["F_rate"]][, c("year", "seas", "fleet", "F")]
  parlist[["init_F"]] <- F_list[["init_F"]]
  # add recdevs to the parlist
  parlist[["recdev_forecast"]] <- new_fore_recdevs_mat
    # get_rec_devs_matrix(yrs = (ctl[["MainRdevYrLast"]] + 1):(dat$endyr + forelist$Nforecastyrs),
    #                     rec_devs = rec_devs)
  # want no implementation error for the forecast, so function adds in 0s.
  parlist[["Fcast_impl_error"]] <-
    get_impl_error_matrix(yrs = (dat$endyr + 1):(dat$endyr + forelist$Nforecastyrs))

  
  ctl[["F_Method"]] <- 2 # Want all OMs to use F_Method = 2.
  ctl[["F_setup"]] <- c(0.05, 1, 0) # need to specify some starting value Fs, although not used in OM
  ctl[["F_iter"]] <- NULL # make sure list components used by other F methods are NULL:
  ctl[["F_setup2"]] <- NULL # make sure list components used by other F methods are NULL:
  
  # write all files
  r4ss::SS_writectl(ctllist = ctl, outfile = file.path(OM_out_dir, start$ctlfile),
                    overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writeforecast(mylist = forelist, dir = OM_out_dir, writeAll = TRUE,
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist,
                         outfile = file.path(OM_out_dir, "ss.par"),
                         overwrite = TRUE)

  if (add_dummy_dat) {
    # TODO: develop code to do this for other types of data (mean length at age)
    # get minimum and maximum years for the model (in dat)
    # add in dummy values to CPUE, length comp, age comp by using -fleet.
    # CPUE
    # TODO: refactor and write a function.
    old_CPUE <- dat$CPUE
    new_CPUE <- old_CPUE # initialize, because want all old values, plus some new ones
    CPUE_seas_flt <- unique(dat$CPUE[, c("seas", "index")])
    dat_yrs <- dat$styr:dat$endyr
    # get a df of the se values to use.
    se_log_val <- get_input_value(data = old_CPUE,
                                  method = "most_common_value",
                                  colname = "se_log",
                                  group = "index")
    if (verbose) {
      message("Input uncertainty for CPUE OM currently can only have a single ",
              "value per fleet. All CPUE data added to the operating model is ",
               "assigned the most common value of se_log for each fleet.")
    }
    for (i in 1:nrow(CPUE_seas_flt)) {
      # get currently  used values
      tmp_seas <- CPUE_seas_flt[i, "seas"]
      tmp_flt <- CPUE_seas_flt[i, "index"]
      # below is the data for the season and fleet combo that already in the OM.
      tmp_CPUE <- old_CPUE[(old_CPUE$seas == tmp_seas & old_CPUE$index == tmp_flt), ]
      # find which years need to be added
      tmp_miss_yrs <- dat_yrs[!dat_yrs %in% tmp_CPUE$year]
      # get the standard error from the old CPUE. For now, just use the
      # most common method (hard coded)
      # tmp_se_log_val should be a single value
      tmp_se_log_val <- se_log_val[se_log_val$index == tmp_flt, "se_log"]
      assertive.properties::assert_is_atomic(tmp_se_log_val)
      assertive.properties::assert_is_of_length(tmp_se_log_val, 1)
      # TODO: make se_log more general by allowing more options for "method: (
      # may need to read in a user value))
      tmp_df <- data.frame(year = tmp_miss_yrs,
                           seas = tmp_seas,
                           index = -tmp_flt,
                           obs = 1,
                           se_log = tmp_se_log_val,
                           stringsAsFactors = FALSE)
      # add these to the CPUE
      new_CPUE <- rbind(new_CPUE, tmp_df)
    }
    # overwrite the old lines
    dat$CPUE <- new_CPUE
    # length comp
    if (dat$use_lencomp == 1) {
      old_lencomp <- dat$lencomp
      new_lencomp <- old_lencomp
      meta_cols <- c("Seas", "FltSvy", "Gender", "Part")
      lcomp_combo <- unique(dat$lencomp[, meta_cols])
      len_Nsamp_val <- get_input_value(data = old_lencomp,
                                       method = "most_common_value",
                                       colname = "Nsamp",
                                       group = "FltSvy")
      if (verbose) {
        message("Input uncertainty for lencomp OM currently can only have a ",
                "single value for each fleet. All lencomp data added to the ",
                "operating model is assigned the most common value of Nsamp ",
                "for each fleet.")
      }
      for (i in seq_len(nrow(lcomp_combo))) {
        # get currently  used values (write a lapply function to make more concise)
        tmp_metacols <- vapply(meta_cols,
                               function(col, i, lcomp_combo) lcomp_combo[i, col],
                               FUN.VALUE = 1,
                               i = i, lcomp_combo = lcomp_combo,
                               USE.NAMES = TRUE)
        tmp_lencomp <- old_lencomp[old_lencomp$Seas == tmp_metacols["Seas"] &
                                 old_lencomp$FltSvy == tmp_metacols["FltSvy"] &
                                 old_lencomp$Gender == tmp_metacols["Gender"] &
                                 old_lencomp$Part == tmp_metacols["Part"], ]
        # find which years need to be added
        tmp_miss_yrs_lencomp <- dat_yrs[!dat_yrs %in% tmp_lencomp$Yr]
        # get the Nsamp
        # TODO: add more options for getting sample size.
        tmp_len_Nsamp_val <- len_Nsamp_val[
          len_Nsamp_val$FltSvy == tmp_metacols["FltSvy"], "Nsamp"]
        assertive.properties::assert_is_atomic(tmp_len_Nsamp_val)
        assertive.properties::assert_is_of_length(tmp_len_Nsamp_val, 1)
        # used suppressWarning because creates an unwanted warning that can be
        # safely ignored
        suppressWarnings(tmp_df_lencomp <- data.frame(Yr = tmp_miss_yrs_lencomp,
                                     Seas = tmp_metacols["Seas"],
                                     FltSvy = -tmp_metacols["FltSvy"],
                                     Gender = tmp_metacols["Gender"],
                                     Part = tmp_metacols["Part"],
                                     Nsamp = tmp_len_Nsamp_val
                                     ))
        tmp_df_dat <- matrix(1,
                           nrow = nrow(tmp_df_lencomp),
                           ncol = ncol(old_lencomp) - 6)
        colnames(tmp_df_dat) <- colnames(old_lencomp)[7:ncol(old_lencomp)]
        tmp_df_dat <- as.data.frame(tmp_df_dat)
        tmp_df_lencomp <- cbind(tmp_df_lencomp, tmp_df_dat)
        # add these new dummy lines to the lencomp
        new_lencomp <- rbind(new_lencomp, tmp_df_lencomp)
      }
      # overwrite the old lines
      dat$lencomp <- new_lencomp
    }
    # agecomp
    old_agecomp <- dat$agecomp
    new_agecomp <- old_agecomp
    meta_cols_agecomp <- c("Seas", "FltSvy", "Gender", "Part", "Ageerr",
                           "Lbin_lo", "Lbin_hi")
    agecomp_combo <- unique(dat$agecomp[, meta_cols_agecomp])
    # get the Nsamp
    # TODO: add more options for getting sample size.
    age_Nsamp_val <- get_input_value(data = old_agecomp,
                                     method = "most_common_value",
                                     colname = "Nsamp",
                                     group = "FltSvy")
    if (verbose) {
      message("Input uncertainty for agecomp OM currently can only have a ",
              "single value per fleet. All lencomp data added to the operating",
              " model is assigned the most common value of Nsamp for each ",
              "fleet.")
    }
    for (i in 1:nrow(agecomp_combo)) {
      # get currently  used values (write a lapply function to make more concise)
      tmp_metacols <- vapply(meta_cols_agecomp,
                             function(col, i, agecomp_combo) agecomp_combo[i, col],
                             FUN.VALUE = 1,
                             i = i, agecomp_combo = agecomp_combo,
                             USE.NAMES = TRUE)
      tmp_agecomp <- old_agecomp[
                        old_agecomp$Seas == tmp_metacols["Seas"] &
                        old_agecomp$FltSvy == tmp_metacols["FltSvy"] &
                        old_agecomp$Gender == tmp_metacols["Gender"] &
                        old_agecomp$Part == tmp_metacols["Part"] &
                        old_agecomp$Ageerr == tmp_metacols["Ageerr"] &
                        old_agecomp$Lbin_lo == tmp_metacols["Lbin_lo"] &
                        old_agecomp$Lbin_hi == tmp_metacols["Lbin_hi"],
                        ]
      # find which years need to be added
      tmp_miss_yrs_agecomp <- dat_yrs[!dat_yrs %in% tmp_agecomp$Yr]
      tmp_age_Nsamp_val <- age_Nsamp_val[
        age_Nsamp_val$FltSvy == tmp_metacols["FltSvy"], "Nsamp"]
      assertive.properties::assert_is_atomic(tmp_age_Nsamp_val)
      assertive.properties::assert_is_of_length(tmp_age_Nsamp_val, 1)

      # used suppressWarning because creates an unwanted warning that can be
      # safely ignored
      suppressWarnings(tmp_df_agecomp <- data.frame(Yr = tmp_miss_yrs_agecomp,
                                   Seas = tmp_metacols["Seas"],
                                   FltSvy = -tmp_metacols["FltSvy"],
                                   Gender = tmp_metacols["Gender"],
                                   Part = tmp_metacols["Part"],
                                   Ageerr = tmp_metacols["Ageerr"],
                                   Lbin_lo = tmp_metacols["Lbin_lo"],
                                   Lbin_hi = tmp_metacols["Lbin_hi"],
                                   Nsamp = tmp_age_Nsamp_val
      ))
      tmp_df_dat <- matrix(1,
                            nrow = nrow(tmp_df_agecomp),
                            ncol = ncol(old_agecomp) - 9)
      colnames(tmp_df_dat) <- colnames(old_agecomp)[10:ncol(old_agecomp)]
      tmp_df_dat <- as.data.frame(tmp_df_dat)
      tmp_df_agecomp <- cbind(tmp_df_agecomp, tmp_df_dat)
      # add these new dummy lines to the lencomp
      new_agecomp <- rbind(new_agecomp, tmp_df_agecomp)
    }
    # overwrite the old lines
    dat$agecomp <- new_agecomp
    if (writedat) {
      SS_writedat(dat, file.path(OM_out_dir, start$datfile),
                  overwrite = overwrite,
                  verbose = FALSE)
    }
  }
  if (verify_OM) {
    # check that model runs and produces a control.ss_new file
    if (file.exists(file.path(OM_out_dir, "control.ss_new"))) {
      file.remove(file.path(OM_out_dir, "control.ss_new"))
    }
    run_ss_model(OM_out_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose,
                 debug_par_run = TRUE)
    if (!file.exists(file.path(OM_out_dir, "control.ss_new"))) {
      stop("The OM model created is not valid; it did not run and produce a\n",
           "control.ss_new file. Please try running the OM model created\n",
           "with the create_OM function manually with SS to diagnose the\n",
           "problem.")
    }
    # check the names of F parameters in the Parameters section of the report
    # file.
    test_output <- r4ss::SS_output(OM_out_dir, forecast = FALSE, verbose = FALSE,
                             warn = FALSE, covar = FALSE, readwt = FALSE,
                             printstats = FALSE)
    # check F's in the assumed order.
    par_df <- test_output$parameters
    init_F_pars <- par_df[grep("^InitF_", par_df$Label), ]
    if (NROW(init_F_pars) != length(F_list[["init_F"]])) {
      stop("Wrong number of init_F parameters assumed by create_OM function.")
    }
    if (NROW(init_F_pars) > 0) {
      if (!all(init_F_pars$Label == names(F_list[["init_F"]]))) {
       stop("Names of init_F parameters assumed by create_OM function and in\n",
            "the PARAMETERS table of Report.sso function do not match.")
      }
    }
    F_rate_pars <- par_df[grep("^F_fleet_", par_df$Label), ]
    if (NROW(F_rate_pars) != NROW(F_list[["F_rate"]])) {
      stop("Wrong number of F_rate parameters assumed by create_OM function.")
    }
    if (NROW(F_rate_pars) > 0) {
      if (!all(F_rate_pars$Label == F_list$F_rate$name)) {
        stop("Names of F_rate parameters assumed by create_OM function and in\n",
             "the PARAMETERS table of Report.sso function do not match.")
      }
    }
  }
  invisible(dat)
}

#' Initial run of the OM
#'
#' This function is used to initialize the OM and get either expected values
#' or bootstrap.
#' @param OM_dir The full path to the OM directory
#' @param boot Return the bootstrap dataset? If TRUE, function returns the
#'   number bootstrapped dataset specified in \code{nboot}. If FALSE, it returns
#'   the expected values.
#' @param nboot The number bootstrapped data set. This value is only used if
#'   \code{boot = TRUE}. Note that this numbering does NOT correspond with the
#'   numbering in section of r4ss::SS_readdat. E.g., specifying section = 3 in
#'   SS_readdat is equivalent to specifying nboot = 1.
#' @param init_run Is this the initial iteration of the OM? Defaults to FALSE.
#' @template verbose
#' @param debug_par_run If set to TRUE, and the run fails, a new folder called
#'  error_check will be created, and the model will be run from control start
#'  values instead of ss.par. The 2 par files are then compared to help debug
#'  the issue with the model run. Defaults to TRUE.
#' @author Kathryn Doering
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
run_OM <- function(OM_dir,
                        boot = TRUE,
                        nboot = 1,
                        init_run = FALSE,
                        verbose = FALSE,
                        debug_par_run = TRUE) {
  # make sure OM generates the correct number of data sets.
  if (boot) {
    max_section <- nboot + 2
  } else {
    max_section <- 2
  }
  if (init_run == TRUE) {
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"),
                                verbose = FALSE)
  start$N_bootstraps <- max_section
  r4ss::SS_writestarter(start, dir = OM_dir, verbose = FALSE, overwrite = TRUE,
                        warn = FALSE)
  }
  # run SS and get the data set
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose,
               debug_par_run = debug_par_run)

  dat <- r4ss::SS_readdat(file.path(OM_dir, "data.ss_new"),
                          section = max_section,
                          verbose = FALSE)
  # If using bootstrap, do not want to use bootstrapped catch. Instead, replace
  # with the expected catch values.
  # TODO: may want to add check if using F method 1 or 3 that the expected vals
  # and the input values match. May not match for method 2.
  if (max_section > 2) {
    if (verbose) message("Adding expected catch to the bootstrapped dataset.")
    exp_vals <- r4ss::SS_readdat(file.path(OM_dir, "data.ss_new"),
                                 section = 2,
                                 verbose = FALSE)
    dat$catch <- exp_vals$catch
  }
  return(dat)
}

#' Get the sampling scheme in a data file.
#'
#' Determine what the default sampling scheme is for a given data file.
#' Produces a list object with the sampling scheme, which can be modified, if
#' desired.
#' @param dat An SS data file
#' @param dat_types Types of data to include
# get the initial sampling values
get_init_samp_scheme <- function(dat,
                                 dat_types = c("CPUE", "lencomp", "agecomp")) {
  # TODO: write this. Can be used for EM and OM.
}
