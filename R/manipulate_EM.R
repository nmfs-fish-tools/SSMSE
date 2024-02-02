# functions to manipulate the estimation model.

#' Change dataset from OM into format for EM
#' @param OM_datfile Filename of the datfile produced by the OM within the
#'  EM_dir.
#' @param EM_datfile Filename of the datfile from the original EM within the
#'  EM_dir.
#' @param EM_dir Absolute or relative path to the Estimation model directory.
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @template verbose
#' @author Kathryn Doering
#' @importFrom r4ss SS_readstarter SS_readdat SS_writedat SS_writestarter
#' @return the new EM data file. Side effect is saving over the OM_dat file in
#'   EM_dir.
#' @examples
#' \dontrun{
#' # TODO: Add example
#' }
change_dat <- function(OM_datfile, EM_datfile, EM_dir, do_checks = TRUE,
                       verbose = FALSE) {
  EM_dir <- normalizePath(EM_dir)

  # checks
  assertive.types::assert_is_a_string(OM_datfile)
  assertive.types::assert_is_a_string(EM_dir)
  check_dir(EM_dir)
  assertive.types::assert_is_a_bool(do_checks)
  assertive.types::assert_is_a_bool(verbose)

  # read in the dat files
  EM_dat <- SS_readdat(file.path(EM_dir, EM_datfile), verbose = FALSE)
  OM_dat <- SS_readdat(file.path(EM_dir, OM_datfile), verbose = FALSE)

  # remove extra years of data in the OM data file.
  new_EM_dat <- get_EM_dat(
    OM_dat = OM_dat, EM_dat = EM_dat,
    do_checks = do_checks
  )

  # write out the modified files that can be used in future EM run
  SS_writedat(new_EM_dat, file.path(EM_dir, OM_datfile),
    verbose = FALSE,
    overwrite = TRUE
  )

  return(new_EM_dat)
}

#' Change the OM data to match the format of the original EM data
#'
#' This does the technical part of changing the EM data. Note this may be unnecessary
#' @param OM_dat An SS data file read in by as a list read in using r4ss from
#'  the operating model
#' @param EM_dat An SS data file read in by as a list read in using r4ss from
#'  the estimation model
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @author Kathryn Doering
#' @return A data list in the same format that can be read/written by r4ss that
#'  has index. lcomps, and age comps from OM_dat, but with the same structure as
#'  EM_dat.
get_EM_dat <- function(OM_dat, EM_dat, do_checks = TRUE) {
  new_dat <- EM_dat # start by copying over to get the correct formatting.
  # TODO: add in code to copy over mean size and mean size at age obs.
  # add in index
  if (do_checks) {
    check_OM_dat(OM_dat, EM_dat)
  }
  dat <- list(OM_dat = OM_dat, EM_dat = EM_dat)
  CPUEs <- lapply(dat, function(x) {
    tmp <- combine_cols(x, "CPUE", c("year", "seas", "index"))
  })
  # match 1 way: match each EM obs with an OM obs. extract only these OM obs.
  matches <- which(CPUEs[[1]][, "combo"] %in% CPUEs[[2]][, "combo"])
  # extract only the rows of interest and get rid of the "combo" column
  new_dat[["CPUE"]] <- CPUEs[[1]][matches, -ncol(CPUEs[[1]])]
  # add in lcomps
  if (OM_dat[["use_lencomp"]] == 1) {
    lcomps <- lapply(dat, function(x) {
      tmp <- combine_cols(
        x, "lencomp",
        c("Yr", "Seas", "FltSvy", "Gender", "Part")
      )
    })
    matches_l <- which(lcomps[[1]][, "combo"] %in% lcomps[[2]][, "combo"])
    new_dat[["lencomp"]] <- lcomps[[1]][matches_l, -ncol(lcomps[[1]])]
  }
  # add in age comps
  if (!is.null(dat[["agecomp"]])) {
    acomps <- lapply(dat, function(x) {
      tmp <- combine_cols(
        x, "agecomp",
        c("Yr", "Seas", "FltSvy", "Gender", "Part", "Lbin_lo", "Lbin_hi")
      )
    })
    matches_a <- which(acomps[[1]][, "combo"] %in% acomps[[2]][, "combo"])
    new_dat[["agecomp"]] <- acomps[[1]][matches_a, -ncol(acomps[[1]])]
  }
  # TODO: check this for other types of data, esp. mean size at age, k
  # and mean size.
  if (!is.null(dat[["meanbodywt"]])) {
    meansize <- lapply(dat, function(x) {
      tmp <- combine_cols(
        x, "meanbodywt",
        c("Year", "Seas", "Fleet", "Partition", "Type", "Std_in")
      )
    })
    matches_meansize <- which(meansize[[1]][, "combo"] %in% meansize[[2]][, "combo"])
    new_dat[["meanbodywt"]] <- meansize[[1]][matches_meansize, -ncol(meansize[[1]])]
  }
  if (!is.null(dat[["MeanSize_at_Age_obs"]])) {
    size_at_age <- lapply(dat, function(x) {
      tmp <- combine_cols(
        x, "MeanSize_at_Age_obs",
        c("Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr")
      )
    })
    matches_size_at_age <- which(size_at_age[[1]][, "combo"] %in% size_at_age[[2]][, "combo"])
    new_dat[["size_at_age"]] <- size_at_age[[1]][matches_size_at_age, -ncol(size_at_age[[1]])]
  }
  # return
  new_dat
}


#' Run the estimation model
#'
#' Runs the estimation model and performs checks if desired.
#'
#' @param EM_dir Absolute or relative path to the estimation model directory
#' @param hess Get the hessian during model run? Defaults to FALSE. Not
#'  estimating the hessian will speed up the run, but no estimates of error will
#'  be generated.
#' @param check_converged Perform checks to see if the model converged? Defaults
#'  to TRUE.
#' @param set_use_par Should input values be read from the .par file? If TRUE,
#' will change setting in the starter file; otherwise, will use the setting
#' already in the starter file, which may or may not read from the .par file.
#' @template verbose
#' @export
#' @author Kathryn Doering
#' @importFrom r4ss SS_readforecast SS_writeforecast SS_readstarter SS_writestarter SS_read_summary
run_EM <- function(EM_dir,
                   hess = FALSE,
                   check_converged = TRUE,
                   set_use_par = FALSE,
                   verbose = FALSE) {
  EM_dir <- normalizePath(EM_dir)
  # checks
  check_dir(EM_dir)
  # set up to run the EM
  if (set_use_par == TRUE) {
    start <- SS_readstarter(file.path(EM_dir, "starter.ss"), verbose = FALSE)
    start[["init_values_src"]] <- 1
    SS_writestarter(start,
      dir = EM_dir, overwrite = TRUE, verbose = FALSE
    )
  }
  if (hess == TRUE) {
    options <- ""
  } else {
    options <- "-nohess"
  }
  run_ss_model(EM_dir, options, verbose = verbose)
  if (check_converged == TRUE) {
    # TODO: add additional checks for convergence, and if additional model runs
    # should be done. perhaps user defined?
    warn <- readLines(file.path(EM_dir, "warning.sso"))
    grad_warn <- grep("^Final gradient\\:\\s+\\d*\\.\\d*\\sis larger than final_conv\\:", warn)
    if (length(grad_warn) > 0) {
      warning(
        "Estimation model did not converge this iteration based on the",
        " convergence criterion set in the starter.ss file."
      )
    }
  }
}

#' Add new data to an existing EM dataset
#'
#' This should be used for the feedback loops when an EM is used.
#' @param OM_dat An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @param EM_datfile Datafile name run in previous iterations with the EM.
#'  Assumed to exist in EM_dir.
#' @template sample_struct
#' @param EM_dir Absolute or relative path to the Estimation model directory.
#' @param nyrs_assess The number of years between assessments. E.g., if an
#'  assessment is conducted every 3 years, put 3 here. A single integer value.
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @param new_datfile_name An optional name of a file to write the new datafile
#'  to. If NULL, a new datafile will not be written.
#' @template verbose
#' @return A new SS datafile containing the data in EM_datfile with new data
#' from OM_dat appended
#' @importFrom r4ss SS_readdat SS_writedat
#' @importFrom stats na.omit
#' @author Kathryn Doering
add_new_dat <- function(OM_dat,
                        EM_datfile,
                        sample_struct,
                        EM_dir,
                        nyrs_assess,
                        do_checks = TRUE,
                        new_datfile_name = NULL,
                        verbose = FALSE) {
  if (do_checks) {
    # TODO: do input checks: check OM_dat is valid r4ss list, check data. only do if
    # do_checks = TRUE?
    if (OM_dat[["type"]] != "Stock_Synthesis_data_file") {
      r4ss_obj_err("OM_dat", "data list")
    }
  }
  # Read in EM_datfile
  EM_dat <- SS_readdat(file.path(EM_dir, EM_datfile), verbose = FALSE)
  new_EM_dat <- EM_dat
  new_EM_dat[["endyr"]] <- new_EM_dat[["endyr"]] + nyrs_assess #  OM_dat[["endyr"]] # want to be the same as the OM
  # add the data from OM_dat into EM_dat
  # checks in relation to OM_dat: check that years, fleets, etc. ar valid

  # extract data from OM_dat based on valid data structure
  extracted_dat <-
    mapply(
      function(df, df_name, OM_dat) {
        OM_df <- OM_dat[[df_name]]
        # get rid of negative fleet values from OM
        if (is.integer(OM_df[1, 3]) | is.numeric(OM_df[1, 3])) {
          OM_df[, 3] <- abs(OM_df[, 3])
        } else if (is.character(OM_df[1, 3])) {
          OM_df[, 3] <- as.character(abs(as.integer(OM_df[, 3])))
        }
        by_val <- switch(df_name,
          "catch" = c("year", "seas", "fleet"),
          "CPUE" = c("year", "seas", "index"),
          "lencomp" = c("Yr", "Seas", "FltSvy", "Gender", "Part"),
          "agecomp" = c(
            "Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr",
            "Lbin_lo", "Lbin_hi"
          ),
          "meanbodywt" = c("Year", "Seas", "Fleet", "Partition", "Type"),
          "MeanSize_at_Age_obs" = c(
            "Yr", "Seas", "FltSvy", "Gender", "Part",
            "AgeErr"
          )
        )
        new_dat <- merge(df, OM_df, by = by_val, all.x = TRUE, all.y = FALSE)
        # Sample sizes are likely different from user inputs if there is
        # variance adjustment values or extra SD. Keep the Nsamp.y value
        # (from the bootstrap sample) To be consistent with other bootstrap
        # model values
        if ("catch_se.y" %in% colnames(new_dat)) {
          new_dat[["catch_se.x"]] <- NULL
          colnames(new_dat)[which(colnames(new_dat) == "catch_se.y")] <- "catch_se"
        }
        if ("se_log.y" %in% colnames(new_dat)) {
          new_dat[["se_log.x"]] <- NULL
          colnames(new_dat)[which(colnames(new_dat) == "se_log.y")] <- "se_log"
        }
        if ("Nsamp.y" %in% colnames(new_dat)) {
          new_dat[["Nsamp.x"]] <- NULL
          colnames(new_dat)[which(colnames(new_dat) == "Nsamp.y")] <- "Nsamp"
        }
        if ("Std_in.y" %in% colnames(new_dat)) {
          new_dat[["Std_in.x"]] <- NULL
          colnames(new_dat)[which(colnames(new_dat) == "Std_in.y")] <- "Std_in"
        }
        if ("Ignore.y" %in% colnames(new_dat)) {
          new_dat[["Ignore.y"]] <- NULL
          colnames(new_dat)[which(colnames(new_dat) == "Ignore.x")] <- "Ignore"
        }
        if ("N_" %in% colnames(new_dat)) {
          n_col <- which(colnames(new_dat) == "N_")
          new_dat <- new_dat[, -n_col]
        }
        # warn if there were matches not found for OM_df, but remove to continue
        if (any(is.na(new_dat))) {
          warning(
            "Some values specified in sample_struct (list component ", df_name,
            ") were not found in OM_dat, so they will not be added to ",
            "the EM_dat."
          )
          new_dat <- na.omit(new_dat)
        }
        new_dat
      },
      df = sample_struct, df_name = names(sample_struct),
      MoreArgs = list(OM_dat = OM_dat),
      SIMPLIFY = FALSE, USE.NAMES = TRUE
    )
  # insert this data into the EM_datfile
  for (n in names(extracted_dat)) {
    new_EM_dat[[n]] <- rbind(new_EM_dat[[n]], extracted_dat[[n]])
  }
  # write the new datafile if new_datfile_name isn't NULL
  if (!is.null(new_datfile_name)) {
    SS_writedat(new_EM_dat,
      file.path(EM_dir, new_datfile_name),
      overwrite = TRUE,
      verbose = FALSE
    )
  }
  new_EM_dat
}

#' Change the years in the forecast file
#'
#' This is both to increment years forward and/or to change absolute years to
#' relative years.
#' @param fore A forecasting file read into R using r4ss::SS_readforecast()
#' @param make_yrs_rel Should the absolute years in the forecast file be changed
#'  to relative years? Defaults to TRUE.
#' @param nyrs_increment The number of years to increment forecasting period years.
#'   If NULL (the default value), will not be incremented.
#' @param nyrs_fore The number of years of forecasting to do. If NULL, do not
#'  change the number of forecasting years already specified in \code{fore}
#' @param mod_styr The first year of the model
#' @param mod_endyr The last year of the model \code{fore} assumes when read in.
#'  Note that the assumed model year will be different for the output if
#'  nyrs_increment is not NULL.
#' @author Kathryn Doering
#' @importFrom  assertive.base assert_is_identical_to_true
#' @return A forecasting file as an R list object
change_yrs_fcast <- function(fore,
                             make_yrs_rel = TRUE,
                             nyrs_increment = NULL,
                             nyrs_fore = NULL,
                             mod_styr,
                             mod_endyr) {
  if (make_yrs_rel == TRUE) {
    # x is the year
    # styr is the model start year
    # endyr is the model end year
    make_yrs_rel <- function(x, styr, endyr) {
      if (x > 0) { # means these are absolute years and not relative.
        if (x == styr) {
          x <- -999
        } else if (x == endyr) {
          x <- 0
        } else if (x > styr & x < endyr) {
          x <- x - endyr # make it relative to endyr
        } else {
          stop(
            "Year in fcast file out of range. Please change to be within ",
            "start and end yrs. Check Bmark_years, Fcast_years"
          )
        }
      }
      x
    }
    # change benchmark years
    new_bmark_yrs <- lapply(fore[["Bmark_years"]],
      make_yrs_rel,
      styr = mod_styr,
      endyr = mod_endyr
    )
    new_bmark_yrs <- unlist(new_bmark_yrs)
    names(new_bmark_yrs) <- names(fore[["Bmark_years"]])
    fore[["Bmark_years"]] <- new_bmark_yrs
    # change forecast years
    new_fcast_yrs <- lapply(fore[["Fcast_years"]],
      make_yrs_rel,
      styr = mod_styr,
      endyr = mod_endyr
    )
    new_fcast_yrs <- unlist(new_fcast_yrs)
    names(new_fcast_yrs) <- names(fore[["Fcast_years"]])
    fore[["Fcast_years"]] <- new_fcast_yrs
  }
  if (!is.null(nyrs_increment)) {
    # first year for caps and allocations
    fore[["FirstYear_for_caps_and_allocations"]] <-
      fore[["FirstYear_for_caps_and_allocations"]] + nyrs_increment
    assert_is_identical_to_true(
      fore[["FirstYear_for_caps_and_allocations"]] > mod_endyr
    )
    # deal with allocation
    if (fore[["N_allocation_groups"]] > 0) {
      tmp_allocation <- fore[["allocation_among_groups"]]
      if (any(tmp_allocation[["Year"]] < mod_endyr)) {
        if (length(tmp_allocation[["Year"]]) == 1) { # increment forward if only one assignment
          fore[["allocation_among_groups"]][["Year"]] <-
            fore[["allocation_among_groups"]][["Year"]] + nyrs_increment
        } else {
          # TODO: develop smarter ways to deal with Time varying allocation
          stop(
            "Time-varying allocation in the forecasting file cannot yet be",
            " used in SSMSE. Please request development of this feature."
          )
        }
      }
    }
  }
  if (!is.null(nyrs_fore)) {
    fore[["Nforecastyrs"]] <- nyrs_fore
  }
  # get rid of Forecatch, if any. Add a warning to the user about this.
  # may beed to treat this differently in the futured
  # TODO: Implementing lag in assessment data (i.e. I run an 2020 assessment with
  # only data to 2018 and providing management advice for 2021) will require the
  # use of the ForeCatch input as well as a method to update what values to input.
  if (!is.null(fore[["ForeCatch"]])) {
    warning("Removing ForeCatch from the EM forecasting file.")
    fore[["ForeCatch"]] <- NULL
  }
  fore
}
