# Functions to extend the OM beyond the initial years.

#' Extend the OM forward using next years' catch
#'
#' Add in the catch values for the next years, extend the OM forward for the
#' appropriate number of years.
#' @author Kathryn Doering & Nathan Vaughan
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g.,
#'  year,	season, fleet,	catch,	catch_se).
#' length of the number of years (only works when catch is for 1 fleet)
#' @param discards A dataframe of discard values an dassociated information to
#'  add to the OM. The column names are as in an SS datafile.
#' @param OM_dir The full path to the OM directory.
#' @param sample_struct The sample structure dataframe.
#' @param nyrs_extend An integer value of years to extend the model forward. Defaults
#'  to an arbitrary value of 3.
#' @param write_dat Should the datafile be overwritten? Defaults to TRUE.
#' @param rec_devs The recruitment deviations
#' @param impl_error The implementation error
#' @template verbose
#' @return A new dat list object (format as created by r4ss::SS_readdat) that
#'  has been extended forward  as if read in by r4ss function SS_readdat
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
extend_OM <- function(catch,
                      discards,
                      OM_dir,
                      sample_struct = NULL,
                      nyrs_extend = 3,
                      write_dat = TRUE,
                      rec_devs = NULL,
                      impl_error = NULL,
                      verbose = FALSE) {

  # input checks
  check_catch_df(catch)
  check_dir(OM_dir)

  # TODO: add function to check dummy_dat_scheme (if has more detailed input)
  # read in the starter file to get OM file names
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"),
                                verbose = FALSE)
  # extend the number of yrs in the model and add in catch
  dat <- r4ss::SS_readdat(file.path(OM_dir, start$datfile), verbose = FALSE,
                          section = 1)
  # read in control file
  ctl <- r4ss::SS_readctl(file = file.path(OM_dir, start$ctlfile),
                          version = '3.30', use_datlist = TRUE, datlist = dat,
                          verbose = FALSE)
  # read in parameter file
  parlist <- r4ss::SS_readpar_3.30(parfile = file.path(OM_dir, "ss.par"),
                                   datsource = dat, ctlsource = ctl,
                                   verbose = FALSE)
  # read in forecast file
  forelist <- r4ss::SS_readforecast(file = file.path(OM_dir, "forecast.ss"),
                                    readAll = TRUE, verbose = FALSE)

  if (max(catch$year) > (dat$endyr + nyrs_extend)) {
    stop("The maximum year input for catch is ", max(catch$year), ", but the ",
         " nyrs_extend used in function extend_OM only extends the model to the year ",
         (dat$endyr + nyrs_extend), ". Please either remove years of catch data or ",
         "the end year of the model longer.")
  }

  # first run OM with catch as projection to calculate the true F required to achieve EM catch in OM
  # Apply implementation error to the catches before it is added to the OM
  # modify forecast file ----
  forelist$Nforecastyrs <- nyrs_extend # should already have this value, but just in case.
  forelist$ForeCatch <- catch[, c("year", "seas", "fleet", "catch")]
  # note: may need to change the colnames.
  # this is the true catch. TODO should impl_error be added or multiplied?
  # NV: Yes as written this should be multiplied as impl_error defaults to 1.
  # We could always change the default to 0 and change to addition if people prefered.
  forelist$ForeCatch[, "catch"] <- forelist$ForeCatch[, "catch"] *
    impl_error[seq_along(NROW(forelist$ForeCatch[, "catch"]))]

  # modify par file ----
  parlist[["recdev_forecast"]] <- get_rec_devs_matrix(
    yrs = (dat$endyr + 1):(dat$endyr + forelist$Nforecastyrs),
    rec_devs = rec_devs)
  # implementation error should always be 0 in the OM
  parlist[["Fcast_impl_error"]] <-
    get_impl_error_matrix(yrs = (dat$endyr + 1):(dat$endyr + forelist$Nforecastyrs))

  # write out the changed files ----
  r4ss::SS_writeforecast(mylist = forelist, dir = OM_dir, writeAll = TRUE,
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
                         overwrite = TRUE, verbose = FALSE)
  # Run SS with the new catch set as forecast targets. This will use SS to
  # calculate the F required in the OM to achieve these catches.
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose,
               debug_par_run = TRUE)
  # Load the SS results
  outlist <- r4ss::SS_output(OM_dir, verbose = FALSE, printstats = FALSE,
                             covar = FALSE, warn = FALSE, readwt = FALSE)
  # Extract the achieved F and Catch for the forecast period
  F_list <- get_F(timeseries = outlist$timeseries,
    fleetnames = dat$fleetinfo[dat$fleetinfo$type %in% c(1, 2), "fleetname"])
  units_of_catch <- dat$fleetinfo[dat$fleetinfo$type %in% c(1, 2), "units"]
  names(units_of_catch) <- as.character(which(dat$fleetinfo$type %in% c(1, 2)))
  ret_catch <- get_retained_catch(timeseries = outlist$timeseries,
    units_of_catch = units_of_catch)
  # Check that SS created projections with the intended catches before updating model
  # make sure retained catch is close to the same as the input catch.
  fcast_ret_catch <- ret_catch[ret_catch$Era == "FORE", c("Yr", "Seas", "Fleet", "retained_catch")]
  names(fcast_ret_catch) <- c("year", "seas", "fleet", "retained_catch_fcast")
  catch_diff_df <- merge(catch,fcast_ret_catch, all = TRUE)
  catch_diff <- catch_diff_df[, "retained_catch_fcast"] - catch_diff_df[, "catch"]
  if (all(catch[, "catch"] != 0)) {
    if (max(abs(catch_diff) / abs(catch_diff_df[, "catch"])) > 0.0001) {
      stop("Forecasted retained catch - ",
           paste0(catch_diff_df[, "retained_catch_fcast"], collapse = ", "),
           " - don't match those expected - ",
           paste0(catch_diff_df[, "catch"], collapse = ", "), "
           : NOTE: This can often occure in scenarios where
           the stock has collapsed at some point and SS has
           unintentionally projected with an unrealistic value.")
      # KD: can we offer any solutions on where to go from here in this error msg?
      # This check is helpful, though!
      # NV: My best guess is this will only trip if SS messes up after the catches
      # crash the stock. In those cases projections can do all sorts of weird things
      # like put in negative F's and create super stock recoveries. I think we should
      # have some sort of default max F that overrides the catch the user could modify.
      # something like apical F can't exceed 2 times the historic maximum or something
      # under the assumption that in the real world there are effort capacity limits.

    }
  } else { # a second check when there is 0 catch.This is fairly arbitrary and
    # perhaps should be more stringent.
   if (max(abs(catch_diff)) > 0.1) {
     stop("Forecasted retained catch - ",
          paste0(catch_diff_df[, "retained_catch_fcast"], collapse = ", "),
          " - don't match those expected - ",
          paste0(catch_diff_df[, "catch"], collapse = ", "))
   }
  }

  # extend the number of yrs in the model and add in catch ----
  # modify forecast file - do this to make the forecasting elements simpler for
  # the second run of the OM.
  forelist$Nforecastyrs <- 1
  forelist$ForeCatch <- NULL
  # change parfile
  # recdevs
  # recdev1 is created when using do_rec_devs method 1; recdev2 is created when
  # using do_rec_devs method 2,3,or 4;
  if (!is.null(parlist$recdev1)) {
    parlist$recdev1 <- rbind(parlist$recdev1,
                             parlist$recdev_forecast[seq_len(nyrs_extend), ])
  } else if (!is.null(parlist$recdev2)) {
    parlist$recdev2 <- rbind(parlist$recdev2,
                             parlist$recdev_forecast[seq_len(nyrs_extend), ])
  }
  # implementation error
  parlist$Fcast_impl_error <- get_impl_error_matrix(
    yrs = (dat$endyr + nyrs_extend + 1))
  # recdevs
  parlist$recdev_forecast <-
    get_rec_devs_matrix(yrs = (dat$endyr + nyrs_extend + 1),
                        rec_devs = 0)
  # F values
  add_F_rate <- F_list[["F_rate_fcast"]][,
                    setdiff(colnames(F_list[["F_rate_fcast"]]), "name")]
  add_F_rate <- add_F_rate[add_F_rate$year %in%
                             (dat$endyr + 1):(dat$endyr + nyrs_extend), ]
  parlist$F_rate <- rbind(parlist$F_rate, add_F_rate)

  # change data file
  dat$catch <- rbind(dat$catch, catch)
  if (dat$N_discard_fleets > 0) {
    dat$discard_data <- rbind(dat$discard_data, discards)
  }
  dat$endyr <- dat$endyr + nyrs_extend

  # modify ctl file ----
  ctl[["MainRdevYrLast"]] <- dat$endyr
  ctl[["last_yr_fullbias_adj"]] <- dat$endyr
  ctl[["first_recent_yr_nobias_adj"]] <- dat$endyr

  # write out the changed files, except for dat.
  r4ss::SS_writectl(ctllist = ctl, outfile = file.path(OM_dir, start$ctlfile),
                    overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writeforecast(mylist = forelist, dir = OM_dir, writeAll = TRUE,
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist,
                         outfile = file.path(OM_dir, "ss.par"), overwrite = TRUE)
  # add in future years data that the EM will need.
  if (!is.null(sample_struct)) {
    tmp_CPUE <- sample_struct[["CPUE"]]
    if(!is.null(tmp_CPUE)) {
      tmp_CPUE <- tmp_CPUE[tmp_CPUE$year >= (dat$endyr - nyrs_extend + 1) &
                           tmp_CPUE$year <= dat$endyr, ]
      if(nrow(tmp_CPUE) > 0) {
        tmp_CPUE$obs <- 1 #dummy observation
        tmp_CPUE <- tmp_CPUE[, c("year", "seas", "index", "obs", "se_log")]
        tmp_CPUE$index <- -abs(tmp_CPUE$index)
        dat$CPUE <- rbind(dat$CPUE, tmp_CPUE)
      }
    }

    # This method of adding new data doesn't work if len comp is not already 
    # turned on. Add warninig for now, but could potentially turn on len comp 
    # for the user in the OM?
    if (dat$use_lencomp == 0 & !is.null(sample_struct[["lencomp"]])) {
      warning("Length composition is not specified in the OM, but the lencomp ",
              "sampling was requested through sample_struct. Please turn on ",
              "length comp in the OM to allow lencomp sampling.")
    }
    if (dat$use_lencomp == 1 & !is.null(sample_struct[["lencomp"]])) {
      tmp_lencomp <- sample_struct[["lencomp"]]
      tmp_lencomp <- tmp_lencomp[tmp_lencomp$Yr >= (dat$endyr - nyrs_extend + 1) &
                             tmp_lencomp$Yr <= dat$endyr, ]
      if(nrow(tmp_lencomp) > 0 ) {
        # get col names
        lencomp_dat_colnames <- colnames(dat$lencomp)[7:ncol(dat$lencomp)]
        tmp_df_dat <- matrix(1,
                             nrow = nrow(tmp_lencomp),
                             ncol = length(lencomp_dat_colnames))
        colnames(tmp_df_dat) <- lencomp_dat_colnames
        tmp_lencomp <- cbind(tmp_lencomp, as.data.frame(tmp_df_dat))
        tmp_lencomp$FltSvy <- -abs(tmp_lencomp$FltSvy) # make sure negative
        dat$lencomp <- rbind(dat$lencomp, tmp_lencomp)
      }
    }
    # TODO: can write code that adds age comp obs when dat$agecomp is NULL.
    if(is.null(dat$agecomp) & !is.null(sample_struct[["agecomp"]])) {
      warning("Age composition is not specified in the OM, but the agecomp ",
              "sampling was requested through sample_struct. Please turn on ",
              "age comp in the OM by adding at least  to allow agecomp ", 
              "sampling.")
    }
    if(!is.null(dat$agecomp) & !is.null(sample_struct[["agecomp"]])) {
      tmp_agecomp <- sample_struct[["agecomp"]]
      tmp_agecomp <- tmp_agecomp[tmp_agecomp$Yr >= (dat$endyr - nyrs_extend + 1) &
                                   tmp_agecomp$Yr <= dat$endyr, ]
      if(nrow(tmp_agecomp) > 0) {
        # get col names
        agecomp_dat_colnames <- colnames(dat$agecomp)[10:ncol(dat$agecomp)]
        tmp_df_dat <- matrix(1,
                             nrow = nrow(tmp_agecomp),
                             ncol = length(agecomp_dat_colnames))
        colnames(tmp_df_dat) <- agecomp_dat_colnames
        tmp_agecomp <- cbind(tmp_agecomp, as.data.frame(tmp_df_dat))
        tmp_agecomp$FltSvy <- -abs(tmp_agecomp$FltSvy) # make sure negative
        dat$agecomp <- rbind(dat$agecomp, tmp_agecomp)
      }
    }
  }
  # write the new data file
  if (write_dat) {
    r4ss::SS_writedat(dat,
                      outfile = file.path(OM_dir, start$datfile),
                      overwrite = TRUE,
                      verbose = FALSE)
  }
    invisible(dat)
  # maybe should just move the final run of the OM here, since we can no longer
  # isolate this function to prevent it from running SS?
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
#' @param datfile The optional name (as a character string) of the datafile,
#'  presumed to exist in \code{OM_dir}. Defaults to NULL, and if is NULL, the
#'  function will get the datfile name from the starter.ss file in \code{OM_dir}.
#' @param catch_units What units is the catch in? "bio" for biomass or "num" for
#'   numbers? Defaults to "bio".
#' @author Kathryn Doering
#' @importFrom r4ss SS_read_summary SS_readstarter SS_readdat
check_future_catch <- function(catch, OM_dir, catch_units = "bio",
                               datfile = NULL) {
  # TODO: add checks for discards???
  # input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  summary <- r4ss::SS_read_summary(file.path(OM_dir, "ss_summary.sso"))
  if (is.null(datfile)) {
    start <- SS_readstarter(file.path(OM_dir, "starter.ss"), verbose = FALSE)
    dat <- SS_readdat(file.path(OM_dir, start$datfile), verbose = FALSE,
                        section = 1)
  } else {
    dat <- SS_readdat(file.path(OM_dir, datfile), verbose = FALSE,
                        section = 1)
  }
  if (is.null(summary)) {
    stop("File ss_summary.sso was not found in directory: ", OM_dir, ". Please",
         " add the file to the directory or change OM_dir to one with this ",
         "file.")
  }
  if (is.null(dat)) {
    stop("Datafile was not found in directory: ", OM_dir, ". Please",
         " add the file to the directory or change OM_dir to one with this ",
         "file.")
  }
  # TODO: check that can you always get biomass for any model? Probalby not if
  # catch units are in numbers. Any other scenarios when this is true?
  if (catch_units == "bio") {
    tot_bio_lyear <-
      summary$biomass[
        grep(paste0("TotBio_", dat$endyr), rownames(summary$biomass)), ]
    if (dat$endyr >= min(catch$year)) {
      stop("The highest year for which TotBio in ss_summary.sso is available (in",
          " the dir ", OM_dir, " is ", dat$endyr, " which is equal to or higher than ",
          "the minimum year value in catch, which is ", min(catch$year), ". ",
          "The catch should only contain values in the future compared to the ",
          "model summary.")
    }
    if (any(catch$catch > tot_bio_lyear$Value)) {
      stop("Some input values for future catch are higher than the most recent",
           " year's total biomass. Recent total biomass: ",
           tot_bio_lyear$Value, "; future catch: ",
           paste0(catch$catch, collapse = ", "))
      # TODO: Maybe write a warning and work around instead of stop?
    }
  } else {
    stop("Function not yet implemented when catch is not in biomass.")
  }
  # return catch invisibly
  invisible(catch)
}
