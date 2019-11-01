# Functions to extend the OM beyond the initial years.

#' Extend the OM forward using next years' catch
#' 
#' Add in the catch values for the next years, extend the OM forward for the 
#' appropriate number of years.
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g., 
#'  year,	season, fleet,	catch,	catch_se).
#' length of the number of years (only works when catch is for 1 fleet)
#' @param OM_dir The full path to the OM directory.
#' @param nyrs_extend An integer value of years to extend the model forward. Defaults
#'  to an arbitrary value of 3.
#' @param dummy_dat_scheme The sampling scheme for dummy data. A list of lists
#' @param verbose Want verbose output? Defaults to FALSE.
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
extend_OM <- function(catch,
                      OM_dir, 
                      dummy_dat_scheme = NULL,
                      nyrs_extend = 3,
                      verbose = FALSE) {
  #input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  #TODO: add function to check dummy_dat_scheme
  # read in the starter file to get OM file names
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"), 
                                verbose = verbose)
  # extend the number of yrs in the model and add in catch
  dat <- r4ss::SS_readdat(file.path(OM_dir, start$datfile), verbose = verbose,
                          section = 1)
  if(max(catch$year) > (dat$endyr + nyrs_extend)) {
    stop("The maximum year input for catch is ", max(catch$year),", but the ",
         " nyrs_extend used in function extend_OM only extends the model to the year ", 
         (dat$endyr+nyrs_extend), ". Please either remove years of catch data or ",
         "the end year of the model longer.")
  }
  dat$endyr <- dat$endyr + nyrs_extend
  dat$catch <- rbind(dat$catch, catch)
  # add in dummy data: just do for indices, comps for now. Always do this in
  # case the EM needs this input (should be okay to remove if not needed?)
  if(!is.null(dummy_dat_scheme)) {
    stop("Code to add in dummy data lines have not been implemented")
    dummy_dat <- get_dummy_dat(dummy_dat_scheme = dummy_dat_scheme)
    #TODO: change to using mapply here instead
    dat$CPUE <- rbind(dat$CPUE, dummy_dat$CPUE)
    dat$lencomp <- rbind(dat$lencomp, dummy_dat$lencomp)
    dat$agecomp <- rbind(dat$agecomp, dummy_dat$agecomp)
  }

  # write the new data file
  r4ss::SS_writedat(dat, 
                    outfile = file.path(OM_dir, start$datfile),
                    overwrite = TRUE,
                    verbose = verbose)
  invisible(catch)
}

#' Get the dummy data for a data type
#' 
#' @param dummy_dat_scheme The years, fleets, data types etc. to add
get_dummy_dat <- function(dummy_dat_scheme) {
  #TODO: write this function
  dummy_dat_scheme
}

#' Check future catch smaller than the last year's population size.
#'
#' Note that it could still be possible to take out too much catch from the 
#' population, so this may not catch all instances of too much catch
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g., 
#'  year,	season, fleet,	catch,	catch_se).
#' length of the number of years (only works when catch is for 1 fleet)
#' @param OM_dir The full path to the OM directory.
#' @param catch_units What units is the catch in? "bio" for biomass or "num" for
#'   numbers? Defaults to "bio".
#' @importFrom r4ss SS_read_summary
check_future_catch <- function(catch, OM_dir, catch_units = "bio") {
  #input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  summary <- r4ss::SS_read_summary(file.path(OM_dir, "ss_summary.sso"))
  if(is.null(summary)) {
    stop("File ss_summary.sso was not found in directory: ", OM_dir, ". Please",
         " add the file to the directory or change OM_dir to one with this ",
         "file.")
  }
  #TODO: check that can you always get biomass for any model? Probalby not if
  # catch units are in numbers. Any other scenarios when this is true?
  if(catch_units == "bio") {
    tot_bio <- summary$biomass[grep("TotBio", rownames(summary$biomass)), ]
    #this should be the last model year, but could make it more robust
    tot_bio_lyear <- tot_bio[nrow(tot_bio), ]
    yr <- as.numeric(strsplit(rownames(tot_bio_lyear), 
                                      split = "_", 
                                      fixed = TRUE)[[1]][2])
    if(yr > min(catch$year)) {
      stop("The highest year for which TotBio in ss_summary.sso is available (in", 
          " the dir ", OM_dir, " is ", yr, " which is higher than the minimum year",
           " value in catch, which is ", min(catch$year), ". The catch should ", 
          "only contain values in the future compared to the model summary.")
    }
    if(any(catch$catch > tot_bio_lyear$Value)) {
      stop("Some input values for future catch are higher than the most recent",
           " year's total biomass. Recent total biomass: ",
           tot_bio_lyear$Value, "; future catch: ", 
           paste0(catch$catch, collapse = ", "))
      #TODO: Maybe write a warning and work around instead of stop?
    }
  } else {
    stop("Function not yet implemented when catch is not in biomass.")
  }
  # return catch invisibly
  invisible(catch)
}
