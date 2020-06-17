# Functions to check input. Hopefully, these can be general and use common
# checks across multiple functions.

#' Check the catch dataframe
#'
#' Ensure the catch data frame has the correct column names in the correct order
#' and the correct number of column names.
#' @param df The catch dataframe to test
#' @author Kathryn Doering
check_catch_df <- function(df) {

  catch_colnames <- c("year", "seas", "fleet", "catch", "catch_se")
  input_colnames <- colnames(df)
  if (length(catch_colnames) != length(input_colnames)) {
    stop("The catch data frame does not have the correct number of column ",
         "names. The column names should be: ",
         paste0(catch_colnames, collapse = ", "), ". However, they are: ", paste0(input_colnames, collapse = ", "), ".")
  }
  if (any(catch_colnames != input_colnames)) {
    stop("The catch data frame does not have the correct column names in the ",
         "correct order. The column names should be: ",
         paste0(catch_colnames, collapse = ", "), ". However, they are: ", paste0(input_colnames, collapse = ", "), ".")
  }
  invisible(df)
}

#' Check that the directory for an OM is valid
#'
#' Check that the directory contains starter and forecast SS files.
#' @param dir Input to check. Should be a directory name that should contain an
#'  SS model that can be used as an OM.
#'  @author Kathryn Doering
check_dir <- function(dir) {
  # chack that the dir contains expected SS model files
  all_files <- list.files(dir)
  errors <- NULL
  if (!"starter.ss" %in% all_files) {
    errors <- c(errors, "starter.ss")
  }
  if (!"forecast.ss" %in% all_files) {
    errors <- c(errors, "forecast.ss")
  }
  if (!is.null(errors)) {
    stop("The file(s): ", paste(errors, collapse = ", "), " is/are missing ",
        "from the directory ", dir, ", which suggests that it is not a valid ",
        "SS directory. Please change to a directory containing a valid SS ",
        "model.")
  }
  invisible(dir)
}

#' check that an OM data set has at least the same data as an estimation model
#' @param OM_dat A data set read in using r4ss::SS_readdat from an operating
#'  model. Note that it should span the same years as EM_dat.
#' @param EM_dat A data set read in using r4ss::SS_readdata from an estimation
#'  model. Note that it should span the same years as EM_dat
#' @author Kathryn Doering
check_OM_dat <- function(OM_dat, EM_dat) {
  # check start and end years match
  if (OM_dat$styr != EM_dat$styr | OM_dat$endyr != EM_dat$endyr) {
    stop("OM_dat and EM_dat should have the same start and end years. However, ",
         "OM_dat has styr = ", OM_dat$styr, " and endyr = ", OM_dat$endyr,
         ", while EM_dat has styr = ", EM_dat$styr, " and endyr = ",
         EM_dat$endyr)
  }
  check_avail_dat(EM_dat = EM_dat, OM_dat = OM_dat, list_item = "catch",
                  colnames = c("year", "seas", "fleet"))
  check_avail_dat(EM_dat = EM_dat, OM_dat = OM_dat, list_item = "CPUE",
                  colnames = c("year", "seas", "index"))
  # check for mean size and mean size at age ,etc (for now, warn that cannot sample.)
  # TODO: add in capabilities to deal with this type of data and remove stop msgs
  if (OM_dat$use_meanbodywt == 1 | EM_dat$use_meanbodywt == 1) {
    stop("Models with mean body size observations cannot yet be used as OMs ",
         "or EMs in SSMSE")
  }
  if (OM_dat$use_MeanSize_at_Age_obs == 1 | EM_dat$use_meanbodywt == 1) {
    stop("Models with mean size-at-age observations cannot yet be used as OMs ",
         "or EMs in SSMSE")
  }
  # check population length bins
  # check lcomp bins and lcomp bins (if exists)
  if (EM_dat$use_lencomp == 1) {
    if (OM_dat$use_lencomp != 1) {
      stop("The EM expects length composition data, but the OM does not have ",
           "any. Please add length composition to the ")
    }
    # check there are the correct number of columns and same names
    if (paste0(colnames(OM_dat$lencomp), collapse = "") !=
       paste0(colnames(EM_dat$lencomp), collapse = "")) {
      stop("Column names for length composition were not the same for the OM ",
           "and EM. Please make the length comp bins the same.")
    }
    # check there is the same data for Years, Seas, FltSvy available
    check_avail_dat(EM_dat = EM_dat, OM_dat = OM_dat, list_item = "lencomp",
                    colnames = c("Yr", "Seas", "FltSvy"))
    # there may be more rigorous checks to do (checking that sex and partion
    # is the same?
  }
  # check age comp
  if (paste0(colnames(OM_dat$agecomp), collapse = "") !=
     paste0(colnames(EM_dat$agecomp), collapse = "")) {
    stop("Column names for age composition were not the same for the OM ",
         "and EM. Please make the age comp bins the same.")
  }
  check_avail_dat(EM_dat = EM_dat, OM_dat = OM_dat, list_item = "agecomp",
                  colnames = c("Yr", "Seas", "FltSvy"))
  invisible(OM_dat)
}

#'  check all index years/fleets in EM available in OM. (but not vice versa)
#'  a general function that can be used
#' @param EM_dat An SS data file read in using r4ss for an EM
#' @param OM_dat An SS data file read in using r4ss for an OM
#' @param list_item A component in both EM_dat and OM_dat to check values for.
#' This should be a single string value.
#' @param colnames The column names of data to append together.
#' @author Kathryn Doering
check_avail_dat <- function(EM_dat, OM_dat,
                            list_item = "CPUE",
                            colnames = c("year", "seas", "index")) {
  EM_item <- EM_dat[[list_item]]
  OM_item <- OM_dat[[list_item]]
  combo_EM <- combo_OM <- NULL
  for (n in colnames) {
    combo_EM <- c(paste0(combo_EM, EM_item[, n], "_"))
    combo_OM <- c(paste0(combo_OM, OM_item[, n], "_"))
  }
  if (any(!(combo_EM %in% combo_OM))) {
    stop("The OM_dat does not include all values of ",
         paste0(colnames, collapse = ", "), " needed for ", list_item, ".")
  }
}

#' Check sample_struct_list
#'
#' Check that list object sample_struct_list has the expected form, including the
#' correct names, correct column names (as in r4ss), and that all values in the
#' dataframes are integer or numeric. This does not check for if numeric or
#' interger values make sense given the model used.
#' @param sample_struct The list to check. Should be a list including which years and
#'  fleets should be added from the OM into the EM for different types of data.
#' @param valid_names The list to compare sample_struct to.
#' @author Kathryn Doering
check_sample_struct <- function(sample_struct, 
  valid_names = list(catch = c("Yr", "Seas", "FltSvy", "SE"),
                     CPUE = c("Yr", "Seas", "FltSvy", "SE"),
                     lencomp = c("Yr", "Seas", "FltSvy", "Sex", "Part", "Nsamp"),
                     agecomp = c("Yr", "Seas", "FltSvy", "Sex", "Part",
                                 "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp"))
  ) {
  # list components should have same names as in r4ss
  # check no repeat names
  if (length(unique(names(sample_struct))) != length(names(sample_struct))) {
    stop("There are repeated names in sample_struct. Please make sure each list ",
         "component has a unique name.")
  }
  # Check correct names and column names
  error <- mapply(function(x, x_name, valid_names) {
    #
    col_names <- colnames(x)
    # find the valid_names that matches x_names
    err <- NULL
    if (!x_name %in% names(valid_names)) {
      err <- "wrong list name"
    } else {
      valid_cols <- valid_names[[which(names(valid_names) == x_name)]]
      if (any(!col_names %in% valid_cols)) {
        err <- "wrong column names in list component"
      }
    }
    err
  },
  x = sample_struct, x_name = names(sample_struct),
  MoreArgs = list(valid_names = valid_names),
  SIMPLIFY = FALSE)

  lapply(error, function(e, v) {
    if (!is.null(e)) {
      stop("Invalid input for sample_struct due to ", e, ". Please check that all",
           " names are not anything other than ",
           paste0(names(v), collapse = ", "), " and have the column names",
           ":\n",
           paste0(paste0(names(v), ": ", v), collapse = "\n"))
    }
    invisible("no_error")
  }, v = valid_names)
  # check that all values can be coerced to numeric
  lapply(sample_struct, function(dataframe) {
    apply(dataframe, 2, function(col) {
      if (!is.numeric(col) & !is.integer(col) & length(col)>=1) {
        stop(paste0("Some values in dat_str are not integers or numeric. Please check ",
             "that all values in the list components (dataframes) of dat_str",
             "are either integer or numeric. values are = ",col))
      }
      if(any(is.na(col))) {
        stop("Some values in sample_struct are NA. Please remove or replace ", 
             "with numeric or integer values.")
      }
    })
  })
  invisible("no_error")
}

#' Error if object is not an r4ss object
#' @param obj_name Object name that is not an r4ss object to print in the error
#' @param type Type that obj_name was expected to be, but is not,
#' @author Kathryn Doering
r4ss_obj_err <- function(obj_name = "object ", type = "list") {
  stop(obj_name, " was found to not be an r4ss ", type, ". Please read in ",
       obj_name, " using r4ss read functions.")
}

#' Check structure of the object scen_list
#'
#' Check the structure that is input to \code{\link{run_SSMSE}}.
#' @author Kathryn Doering
#' @param list A list to check
#' @template verbose
check_scen_list <- function(list, verbose = FALSE) {
  # some columns are required, but others are optional. Check that the required
  # columns are there, and warn if the optional ones arent, if verbose.
  # TODO: write this function. Did not want to write until we decide on input
  warning("No check for scen_list structure yet implemented. Use at your own ",
   "risk.")
  assertive.types::assert_is_list(list)
  invisible(list)
}

#' Check structure of forecast is suitable to use in the EM
#' @param fore A forecast list read in using r4ss::SS_readforecast
#' @param n_flts_catch The number of fleets with catch. If NULL, this function
#' will skip a check requiring this input.
#' @author Kathryn Doering
#' @return Function mainly used for side effects, but returns TRUE invisibly if
#'  no errors created.
check_EM_forecast <- function(fore, n_flts_catch = NULL) {
  msg <- NULL
  # check benchmark on
  if (fore[["benchmarks"]] == 0) {
    msg <- c(msg, "Benchmarks set to 0, but need to be turned on.")
  }
  # check forecasting on
  if (fore[["Forecast"]] <= 0) {
    msg <- c(msg, "Forecast set to -1 or 0, but needs to be turned on. ")
  }
  # check allocation defined if the fleets with catch > 1
  if (!is.null(n_flts_catch)) {
    if (fore[["N_allocation_groups"]] == 0 & n_flts_catch > 1) {
      msg <- c(msg, "Fleets with catch > 1, so allocation must be defined.")
    }
  }
  # check rebuilder off (in the future, the option to use rebuilder could be
  # added?)
  if (fore[["Do_West_Coast_gfish_rebuilder_output"]] == 1) {
    msg <- c(msg, "Rebuilder turned on; must be turned off to use model with SSMSE.")
  }
  if (!is.null(msg)) {
    stop("EM forecast file has issues and needs changes:",
         paste(msg, collapse = " "))
  }
  invisible(TRUE)
}
