# Functions associated with initialization of the OM.

#' Create the OM
#' 
#' This function manipulates the OM as needed so that it can be used as an
#'  operating model.
#' @author Kathryn Doering & Nathan Vaughan
#' @param OM_out_dir The full path to the directory in which the OM is run.
#' @param overwrite Overwrite existing files with matching names?
#' @param nyrs_assess The number of years between assessments. This is used to
#'  structure the forecast file for use in the OM.
#' @param writedat Should a new datafile be written?
#' @param rec_devs Vector of recruitment deviations for simulation.
#' @param verify_OM Should the model be run without estimation and some basic
#'  checks done to verify that the OM can run? Defaults to TRUE.
#' @param sample_struct_hist The historical sample structure, if specified by 
#'  the user. Defaults to NULL, which means to use the same sample structure as
#'  the historical data.
#' @param seed input seed to allow reproducible SS results.
#' @template verbose
#' @return A modified datafile
#' @import r4ss
create_OM <- function(OM_out_dir,
                      overwrite = TRUE,
                      writedat = TRUE,
                      verbose = FALSE,
                      nyrs_assess = NULL,
                      rec_devs = NULL,
                      verify_OM = TRUE,
                      sample_struct_hist = NULL,
                      seed = NULL) {
  start <- r4ss::SS_readstarter(file.path(OM_out_dir, "starter.ss"),
    verbose = FALSE
  )
  # modify starter to use as OM ----
  if (is.null(seed)) {
    seed <- stats::runif(1, 1, 99999999)
  }
  start[["init_values_src"]] <- 1
  start[["detailed_age_structure"]] <- 1
  start[["last_estimation_phase"]] <- 0
  start[["depl_basis"]] <- 0
  start[["depl_denom_frac"]] <- 1
  start[["SPR_basis"]] <- 0
  start[["F_report_units"]] <- 0
  start[["F_report_basis"]] <- 0
  start[["F_age_range"]] <- NULL
  start[["ALK_tolerance"]] <- 0
  start[["seed"]] <- seed
  r4ss::SS_writestarter(start,
    dir = OM_out_dir, verbose = FALSE,
    overwrite = TRUE, warn = FALSE
  )
  # run model to get standardized output ----
  run_ss_model(OM_out_dir, "-maxfn 0 -phase 50 -nohess",
    debug_par_run = TRUE,
    verbose = verbose
  )
  # read in files to use ----
  dat <- r4ss::SS_readdat(
    file = file.path(OM_out_dir, start[["datfile"]]),
    verbose = FALSE, section = 1
  )
  forelist <- r4ss::SS_readforecast(
    file = file.path(OM_out_dir, "forecast.ss"),
    readAll = TRUE, verbose = FALSE
  )
  ctl <- r4ss::SS_readctl(file.path(OM_out_dir, start[["ctlfile"]]),
    verbose = FALSE,
    use_datlist = TRUE, datlist = dat
  )
  outlist <- r4ss::SS_output(OM_out_dir,
    verbose = FALSE, printstats = FALSE,
    covar = FALSE
  )
  parlist <- r4ss::SS_readpar_3.30(
    parfile = file.path(OM_out_dir, "ss.par"),
    datsource = dat, ctlsource = ctl,
    verbose = FALSE
  )
  # model checks ----
  if(ctl[["F_Method"]] == 1) {
    stop("SSMSE cannot work with models that use F method 1 (Pope's ",
         "approximation). Please use F method 2 or 3 instead (3 is ",  
         "recommended over method 1).")
  }
  

  # modify forecast file ----
  currentNforecast <- forelist[["Nforecastyrs"]]
  forelist[["Nforecastyrs"]] <- nyrs_assess
  forelist[["FirstYear_for_caps_and_allocations"]] <- dat[["endyr"]] + nyrs_assess + 1
  forelist[["InputBasis"]] <- 3
  forelist[["ControlRuleMethod"]] <- 1
  forelist[["BforconstantF"]] <- 0.001
  forelist[["BfornoF"]] <- 0.0001
  forelist[["Flimitfraction"]] <- 1

  # convert forecast year selectors to absolute form
  for (i in 1:6) {
    x <- forelist[["Fcast_years"]][i]
    if (x == -999) {
      forelist[["Fcast_years"]][i] <- dat[["styr"]]
    } else if (x <= 0) {
      forelist[["Fcast_years"]][i] <- dat[["endyr"]] + x
    } else if (x < dat[["styr"]] | x > dat[["endyr"]]) {
      stop("Forecast year should be <=0 or between start year and end year")
    }
  }
  # put together a Forecatch dataframe using retained catch as a starting point for the OM
  # this would only matter if an EM assessment is not run in the first year.
  units_of_catch <- dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "units"]
  names(units_of_catch) <- as.character(which(dat[["fleetinfo"]][["type"]] %in% c(1, 2)))
  ret_catch <- get_retained_catch(
    timeseries = outlist[["timeseries"]],
    units_of_catch = units_of_catch
  )
  temp_fore <- ret_catch[
    ret_catch[["Era"]] == "FORE",
    c("Yr", "Seas", "Fleet", "retained_catch")
  ]
  row.names(temp_fore) <- NULL
  names(temp_fore) <- c("Year", "Seas", "Fleet", "Catch or F")
  # only put values in that are in the Fcas year range.
  forelist[["ForeCatch"]] <- temp_fore[
    is.element(temp_fore[["Year"]], (dat[["endyr"]] + 1):(dat[["endyr"]] + nyrs_assess)),
  ]

  # modify ctl file ----
  # in the context of an OM, do not want to use the bias adjustment ramp, so just
  # turn off and make the recdevs years always the same.
  # UPDATE NOTE: For the OM we do not want bias adjustment in the future. However we
  # do want the historic period to be consistent with the original assessment model.
  # We therefore need to add advanced options if not already specified. I am also
  # updating the extend EM process to fix the main recdevs end year. This way all new
  # recdevs become late phase/forecast recdevs which are not subject to sum to zero
  # constraints or bias adjustment.
  if (ctl[["recdev_adv"]] == 0) {
    ctl[["recdev_adv"]] <- 1
    ctl[["recdev_early_start"]] <- 0
    ctl[["recdev_early_phase"]] <- -4
    ctl[["Fcast_recr_phase"]] <- 0
    ctl[["lambda4Fcast_recr_like"]] <- 0
    ctl[["last_early_yr_nobias_adj"]] <- ctl[["MainRdevYrFirst"]] - 1
    ctl[["first_yr_fullbias_adj"]] <- ctl[["MainRdevYrFirst"]]
    ctl[["last_yr_fullbias_adj"]] <- ctl[["MainRdevYrLast"]]
    ctl[["first_recent_yr_nobias_adj"]] <- ctl[["MainRdevYrLast"]] + 1
    ctl[["max_bias_adj"]] <- 0.8
    ctl[["period_of_cycles_in_recr"]] <- 0
    ctl[["min_rec_dev"]] <- -10
    ctl[["max_rec_dev"]] <- 10
    ctl[["N_Read_recdevs"]] <- 0
    ctl[["recdev_input"]] <- NULL
  }
  
  # modify par file ----
  # note: don't include early recdevs in in all_recdevs
  all_recdevs <- as.data.frame(rbind(parlist[["recdev1"]], parlist[["recdev2"]], parlist[["recdev_forecast"]]))
  # get recdevs for all model yeasrs
  all_recdevs <- all_recdevs[all_recdevs[["year"]] >= ctl[["MainRdevYrFirst"]] & all_recdevs[["year"]] <= (dat[["endyr"]] + forelist[["Nforecastyrs"]]), ]
  # new_recdevs_df <- data.frame(year = dat[["styr"]]:dat[["endyr"]], recdev = NA)
  new_recdevs_df <- data.frame(year = ctl[["MainRdevYrFirst"]]:ctl[["MainRdevYrLast"]], recdev = NA)
  fore_recdevs_df <- data.frame(year = (ctl[["MainRdevYrLast"]] + 1):(dat[["endyr"]] + forelist[["Nforecastyrs"]]), recdev = NA)
  for (i in seq_along(ctl[["MainRdevYrFirst"]]:(dat[["endyr"]] + forelist[["Nforecastyrs"]]))) {
    tmp_yr <- (ctl[["MainRdevYrFirst"]]:(dat[["endyr"]] + forelist[["Nforecastyrs"]]))[i]
    if (tmp_yr <= ctl[["MainRdevYrLast"]]) {
      step <- i
      if (length(all_recdevs[all_recdevs[["year"]] == tmp_yr, "year"]) == 0) {
        new_recdevs_df[i, "recdev"] <- 0 # just assume no recdevs
      } else {
        new_recdevs_df[i, "recdev"] <-
          all_recdevs[all_recdevs[["year"]] == tmp_yr, "recdev"]
      }
    } else if (tmp_yr <= dat[["endyr"]]) {
      if (length(all_recdevs[all_recdevs[["year"]] == tmp_yr, "year"]) == 0) {
        fore_recdevs_df[(i - step), "recdev"] <- 0
      } else {
        fore_recdevs_df[(i - step), "recdev"] <-
          all_recdevs[all_recdevs[["year"]] == tmp_yr, "recdev"]
      }
    } else {
      temp_recs <- get_rec_devs_matrix(
        yrs = tmp_yr,
        rec_devs = rec_devs
      )
      if (length(temp_recs[, "recdev"]) == 1) {
        fore_recdevs_df[(i - step), "recdev"] <- temp_recs[1, "recdev"]
      } else if (length(all_recdevs[all_recdevs[["year"]] == tmp_yr, "year"]) == 0) {
        fore_recdevs_df[(i - step), "recdev"] <- 0
      } else {
        fore_recdevs_df[(i - step), "recdev"] <-
          all_recdevs[all_recdevs[["year"]] == tmp_yr, "recdev"]
      }
    }
  }
  new_recdevs_mat <- as.matrix(new_recdevs_df)
  new_fore_recdevs_mat <- as.matrix(fore_recdevs_df)
  if (!is.null(parlist[["recdev1"]])) {
    parlist[["recdev1"]] <- new_recdevs_mat
  } else if (!is.null(parlist[["recdev2"]])) {
    parlist[["recdev2"]] <- new_recdevs_mat
  } else {
    stop("no recdevs in initial OM model")
  }

  # use report.sso time series table to find the F's to put into the parlist.
  F_list <- get_F(
    timeseries = outlist[["timeseries"]],
    fleetnames = dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "fleetname"]
  )
  # modify the init_F and F_rate in parlist if used.
  # note that F_list[["F_rate"]] and F_list[["F_init]] are NULL if they had 0
  # rows.
  parlist[["F_rate"]] <- F_list[["F_rate"]][, c("year", "seas", "fleet", "F")]
  parlist[["init_F"]] <- F_list[["init_F"]]
  # add recdevs to the parlist
  parlist[["recdev_forecast"]] <- new_fore_recdevs_mat
  # get_rec_devs_matrix(yrs = (ctl[["MainRdevYrLast"]] + 1):(dat[["endyr"]] + forelist[["Nforecastyrs"]]),
  #                     rec_devs = rec_devs)
  # want no implementation error for the forecast, so function adds in 0s.
  parlist[["Fcast_impl_error"]] <-
    get_impl_error_matrix(yrs = (dat[["endyr"]] + 1):(dat[["endyr"]] + forelist[["Nforecastyrs"]]))


  ctl[["F_Method"]] <- 2 # Want all OMs to use F_Method = 2.
  ctl[["F_setup"]] <- c(0.05, 1, 0) # need to specify some starting value Fs, although not used in OM
  ctl[["F_iter"]] <- NULL # make sure list components used by other F methods are NULL:
  ctl[["F_setup2"]] <- NULL # make sure list components used by other F methods are NULL:

  # write all files
  r4ss::SS_writectl(
    ctllist = ctl, outfile = file.path(OM_out_dir, start[["ctlfile"]]),
    overwrite = TRUE, verbose = FALSE
  )
  r4ss::SS_writeforecast(
    mylist = forelist, dir = OM_out_dir, writeAll = TRUE,
    overwrite = TRUE, verbose = FALSE
  )
  r4ss::SS_writepar_3.30(
    parlist = parlist,
    outfile = file.path(OM_out_dir, "ss.par"),
    overwrite = TRUE
  )
  # modify dat file ----
  # remove the sampling components not needed
  dat <- rm_sample_struct_hist(sample_struct = sample_struct_hist, dat = dat)
  # Add in the historical sampling structure, as defined by the user
  dat <- add_sample_struct(sample_struct = sample_struct_hist, dat = dat, 
                           nyrs_extend = 0)
  
  # make sure tail compression is off.
  # turn off tail compression
  if (isTRUE(any(dat[["len_info"]][["mintailcomp"]] >= 0)) |
    isTRUE(any(dat[["age_info"]][["mintailcomp"]] >= 0))) {
    warning(
      "Tail compression was on for some fleets in length comp and/or age ",
      "comp for the operating model, but needs to be",
      "turned off in an operating model. Turning off tail compression.",
      " Note that this may change expected values for historical age or ",
      " length composition."
    )
    if (!is.null(dat[["len_info"]])) dat[["len_info"]][["mintailcomp"]] <- -1
    if (!is.null(dat[["age_info"]])) dat[["age_info"]][["mintailcomp"]] <- -1
  }

  if (writedat) {
    SS_writedat(dat, file.path(OM_out_dir, start[["datfile"]]),
      overwrite = overwrite,
      verbose = FALSE
    )
  }
  if (verify_OM) {
    # check that model runs and produces a control.ss_new file
    if (file.exists(file.path(OM_out_dir, "control.ss_new"))) {
      file.remove(file.path(OM_out_dir, "control.ss_new"))
    }
    run_ss_model(OM_out_dir, "-maxfn 0 -phase 50 -nohess",
      verbose = verbose,
      debug_par_run = TRUE
    )
    # TODO: maybe add the following check into the debug par run arg of run_ss_model?
    check_par <- readLines(file.path(OM_out_dir, "ss.par"))
    check_sum_val <- check_par[grep("checksum999", check_par)+1]
    if(as.numeric(check_sum_val) != 999) {
      stop("The OM model created is not valid; likely, the par file was not of", 
           "the correct length because checksum999 of output is not 999.", 
           "Please open an issue in the SSMSE repository for assistance.")
    }
    if (!file.exists(file.path(OM_out_dir, "control.ss_new"))) {
      stop(
        "The OM model created is not valid; it did not run and produce a\n",
        "control.ss_new file. Please try running the OM model created\n",
        "with the create_OM function manually with SS to diagnose the\n",
        "problem."
      )
    }
    # check the names of F parameters in the Parameters section of the report
    # file.
    test_output <- r4ss::SS_output(OM_out_dir,
      forecast = FALSE, verbose = FALSE,
      warn = FALSE, covar = FALSE, readwt = FALSE,
      printstats = FALSE
    )
    # check F's in the assumed order.
    par_df <- test_output[["parameters"]]
    init_F_pars <- par_df[grep("^InitF_", par_df[["Label"]]), ]
    if (NROW(init_F_pars) != length(F_list[["init_F"]])) {
      stop("Wrong number of init_F parameters assumed by create_OM function.")
    }
    if (NROW(init_F_pars) > 0) {
      if (!all(init_F_pars[["Label"]] == names(F_list[["init_F"]]))) {
        stop(
          "Names of init_F parameters assumed by create_OM function and in\n",
          "the PARAMETERS table of Report.sso function do not match."
        )
      }
    }
    F_rate_pars <- par_df[grep("^F_fleet_", par_df[["Label"]]), ]
    if (NROW(F_rate_pars) != NROW(F_list[["F_rate"]])) {
      stop("Wrong number of F_rate parameters assumed by create_OM function.")
    }
    if (NROW(F_rate_pars) > 0) {
      if (!all(F_rate_pars[["Label"]] == F_list[["F_rate"]][["name"]])) {
        stop(
          "Names of F_rate parameters assumed by create_OM function and in\n",
          "the PARAMETERS table of Report.sso function do not match."
        )
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
#' @param seed A random seed for SS to enable reproducible SS results.
#' @author Kathryn Doering
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
run_OM <- function(OM_dir,
                   boot = TRUE,
                   nboot = 1,
                   init_run = FALSE,
                   verbose = FALSE,
                   debug_par_run = TRUE,
                   seed = NULL) {
  # make sure OM generates the correct number of data sets.
  if (boot) {
    max_section <- nboot + 2
  } else {
    max_section <- 2
  }
  if (is.null(seed)) {
    seed <- stats::runif(1, 1, 9999999)
  }

  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"),
    verbose = FALSE
  )
  start[["N_bootstraps"]] <- max_section
  start[["seed"]] <- seed
  r4ss::SS_writestarter(start,
    dir = OM_dir, verbose = FALSE, overwrite = TRUE,
    warn = FALSE
  )

  # run SS and get the data set
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess",
    verbose = verbose,
    debug_par_run = debug_par_run
  )

  dat <- r4ss::SS_readdat(file.path(OM_dir, "data.ss_new"),
    section = max_section,
    verbose = FALSE
  )
  # If using bootstrap, do not want to use bootstrapped catch. Instead, replace
  # with the expected catch values.
  # TODO: may want to add check if using F method 1 or 3 that the expected vals
  # and the input values match. May not match for method 2.
  if (max_section > 2) {
    if (verbose) message("Adding expected catch to the bootstrapped dataset.")
    exp_vals <- r4ss::SS_readdat(file.path(OM_dir, "data.ss_new"),
      section = 2,
      verbose = FALSE
    )
    dat[["catch"]] <- exp_vals[["catch"]]
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


#' Remove the historical sampling structure
#' 
#' @param sample_struct_hist The historical sampling structure as specified by
#'  the user. Can be NULL, in which case no values will be removed. Uses the
#'  names as in the r4ss dataframes. 
#' @param dat The data file, as read in using r4ss
rm_sample_struct_hist <- function(sample_struct_hist, dat) {
  if(is.null(sample_struct_hist)) {
    return(dat)
  }
  # remove the CPUE
  dat[["CPUE"]] <- rm_vals(return_obj = dat,
                           compare_obj = sample_struct_hist, 
                           name_in_obj = "CPUE",
                           colnames = c("year", "seas", "index"))
  dat[["lencomp"]] <- rm_vals(return_obj = dat,
                              compare_obj = sample_struct_hist, 
                              name_in_obj = "lencomp",
                              colnames = c("Yr", "Seas", "FltSvy", "Gender",
                                           "Part"))
  dat[["agecomp"]] <- rm_vals(return_obj = dat,
                              compare_obj = sample_struct_hist, 
                              name_in_obj = "agecomp",
                              colnames = c("Yr", "Seas", "FltSvy", "Gender", 
                                           "Part", "Ageerr", "Lbin_lo", 
                                           "Lbin_hi"))
  dat
}

#' remove vals in 2 list components with the same name
#' 
#' From 2 list components with the same name, remove vals that aren't in the 
#' compare object
#' 
#' @param return_obj the object (containing list component of name in obj) that
#'  will be modified. Only combinations of the columns found in compare object
#'   will be retained
#' @param compare_obj the object (containing list component of name_in_obj) that
#'  return_obj will be compared to
#' @param name_in_obj the name of the list elements to use; the same name must
#'  be in return_obj and compare_obj. This list element must be a data frame
#'  with the same column names
#' @param colnames The column names within the name_in_obj list components to
#'  compare.
#' @return return_obj[[name_in_obj]], modified to only include elements present
#'  in compare_obj[[name_in_obj]].
#' @author Kathryn Doering
rm_vals <- function(return_obj, compare_obj, name_in_obj, colnames) {
  # return early if nothing to compare.
  if(is.null(compare_obj[[name_in_obj]]) | is.null(return_obj[[name_in_obj]])) {
    return(return_obj[[name_in_obj]])
  }
  return_obj[[name_in_obj]] <- combine_cols(dat_list = return_obj,
                                            list_item = name_in_obj,
                                            colnames = colnames)
  compare_obj[[name_in_obj]] <- combine_cols(dat_list = compare_obj,
                                            list_item = name_in_obj,
                                            colnames = colnames)
  to_keep <- intersect(compare_obj[[name_in_obj]][["combo"]],
                       return_obj[[name_in_obj]][["combo"]])
  to_return <-  return_obj[[name_in_obj]][
    return_obj[[name_in_obj]][["combo"]] %in% to_keep,
    !(colnames(return_obj[[name_in_obj]]) %in% "combo")]
  to_return
}