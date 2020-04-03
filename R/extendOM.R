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
#' @param dummy_dat_scheme The sampling scheme for dummy data. Current options
#'  are NULL, which means no new dummy data will be added CPUE, length comps, or
#'  age comps, or \code{"all"}, which means that new years of data will be added 
#'  for CPUE, length comps (if using), and age comps, for all new years and 
#'  combinations of fleet and season. For length comps and age comps, a dummy
#'  value will also be added for each combination of other metadata values,
#'  including (but not limited to ) sex, partition, and bin high and low values.
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
                      dummy_dat_scheme = NULL,
                      nyrs_extend      = 3,
                      write_dat        = TRUE,
                      rec_devs         = NULL,
                      impl_error        = NULL,
                      verbose          = FALSE) {
  #input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  
  #TODO: add function to check dummy_dat_scheme (if has more detailed input)
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
  
  if(max(catch$year) > (dat$endyr + nyrs_extend)) {
    stop("The maximum year input for catch is ", max(catch$year),", but the ",
         " nyrs_extend used in function extend_OM only extends the model to the year ", 
         (dat$endyr+nyrs_extend), ". Please either remove years of catch data or ",
         "the end year of the model longer.")
  }
  
  #first run OM with catch as projection to calculate the true F required to achieve EM catch in OM
  #Apply implementation error to the catches before it is added to the OM
  forelist$Nforecastyrs <- nyrs_extend + 1
  forelist$ForeCatch <- catch[, 1:4]
  forelist$ForeCatch[,4] <- forelist$ForeCatch[, 4] * impl_error
  #parlist$recdev_forecast[,1]<-(dat$endyr + nyrs_extend+1):(dat$endyr + 2*nyrs_extend)
  temp_recdev <- matrix(NA, nrow = forelist$Nforecastyrs, ncol = 2)
  temp_recdev[, 1] <- (dat$endyr+1):(dat$endyr+forelist$Nforecastyrs)
  temp_recdev[, 2] <- c(rec_devs, -sum(rec_devs))
  temp_impl_error <- temp_recdev
  temp_impl_error[, 2] <- rep(0, forelist$Nforecastyrs)
  parlist$recdev_forecast <- temp_recdev
  parlist$Fcast_impl_error <- temp_impl_error
  colnames(parlist$recdev_forecast) <- c("year","recdev")
  colnames(parlist$Fcast_impl_error) <- c("year","impl_error")
  
  
  r4ss::SS_writeforecast(mylist = forelist, dir = OM_dir, writeAll = TRUE,
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist,outfile = file.path(OM_dir, "ss.par"),
                         overwrite = TRUE, verbose = FALSE)
  
  #Run SS with the new catch set as forecast targets. This will use SS to 
  #calculate the F required in the OM to achieve these catches.
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose)
  
  #Load the SS results 
  outlist <- r4ss::SS_output(OM_dir, verbose = FALSE, printstats = FALSE)
  #Extract the achieved F and Catch for the projection period
  temp_F <- outlist$timeseries
  base_F <- temp_F[temp_F$Area==1, ]
  agg_F <- aggregate(temp_F[,-3], list(Yr=temp_F[,2], Seas = temp_F[,4]), sum)
  temp_F <- cbind(base_F[, c(2,3,4)], agg_F[, -c(1,2,3,4,5,6,7,8,9,10,11,12)])
  rm(base_F, agg_F)
  units_catch <- dat$units_of_catch
  units_catch <- ifelse(units_catch == 1, 3, 6)
  temp_F <- temp_F[, c(1,2,3,(((1:dat$Nfleet))*8+3),
                       (((1:dat$Nfleet)-1)*8+3+units_catch[1:dat$Nfleet]))]
  
  temp_df <- NULL
  for(i in 1:dat$Nfleet) {
    temp_fleet <- cbind(temp_F[, c(1:3)], rep(i, length(temp_F[,1])), 
                        temp_F[,(i+3)], temp_F[, (i+3+dat$Nfleet)])
    temp_df <- rbind(temp_df, temp_fleet)
  }
  temp_df <- temp_df[order(temp_df[, 1], temp_df[, 3], temp_df[, 4]), ]
  names(temp_df) <- c("year", "Era", "seas", "fleet", "F", "Catch_retained")
  
  #Check that SS created projections with the intended catches before updating model
  catch_diff <- temp_df[is.element(temp_df[, 1], catch[, 1]), 6] - catch[, 4]
  if(max(abs(catch_diff)/abs(catch[,4])) > 0.0001) {
    stop("Catch projections - ", 
         temp_df[is.element(temp_df[,1], catch[, 1]), 6],
         " - don't match those expected - ",catch[, 4])
  }
  
  # extend the number of yrs in the model and add in catch
  dat$catch <- rbind(dat$catch, catch)
  dat$discard_data <- rbind(dat$discard_data, discards)
  if(!is.null(parlist$recdev1)) {
    parlist$recdev1 <- rbind(parlist$recdev1, 
                             parlist$recdev_forecast[1:nyrs_extend, ])
  }else if(!is.null(parlist$recdev2)) {
    parlist$recdev2 <- rbind(parlist$recdev2,
                             parlist$recdev_forecast[1:nyrs_extend,])
  }
  parlist$Fcast_impl_error <- 
    parlist$Fcast_impl_error[length(parlist$Fcast_impl_error[,1]), ,
                             drop = FALSE]
  parlist$recdev_forecast <- parlist$Fcast_impl_error
  parlist$F_rate <- 
    rbind(parlist$F_rate, 
      temp_df[is.element(temp_df[,1],((dat$endyr+1):(dat$endyr + nyrs_extend))),
              c(1,3,4,5)])
  dat$endyr <- dat$endyr + nyrs_extend
  forelist$Nforecastyrs <- 1
  forelist$ForeCatch <- NULL
  ctl$MainRdevYrLast <- dat$endyr
  r4ss::SS_writectl(ctllist = ctl, outfile = file.path(OM_dir, start$ctlfile),
                    overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writeforecast(mylist = forelist, dir = OM_dir, writeAll = TRUE,
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist,
                         outfile = file.path(OM_dir, "ss.par"), overwrite = TRUE)
  # add in dummy data: just do for indices, comps for now. Always do this in
  # case the EM needs this input (should be okay to remove if not needed?)
  if(!is.null(dummy_dat_scheme)) {
    if(dummy_dat_scheme == "all") {
      # figure out which unique combos of fleets and seas exist in each of
      # CPUE, lencomp, agecomp, then get the dummy values
      CPUE_combo <- dat$CPUE[, c("seas", "index")]
      CPUE_combo$index <- abs(CPUE_combo$index)
      CPUE_combo <- unique(CPUE_combo)
      # get sample size to use
      # TODO make this more general
      se_log_val <- get_input_value(data = dat[["CPUE"]], 
                                    method = "most_common_value", 
                                    colname = "se_log", 
                                    group = "index")
      if(verbose) {
        message("Input uncertainty for extending OM CPUE currently can only ",
                "have a single value for each fleet. All CPUE data added to ",
                "the operating model is assigned the most common value of ",
                "se_log for each fleet.")
      }
      for (i in 1:nrow(CPUE_combo)) { # loop through combinations and add
        tmp_se_log_val <- se_log_val[se_log_val$index == CPUE_combo$index[i], 
                                     "se_log"]
        assertive.properties::assert_is_atomic(tmp_se_log_val)
        assertive.properties::assert_is_of_length(tmp_se_log_val, 1)
        tmp_CPUE_df <- data.frame(year = (dat$endyr-nyrs_extend+1):dat$endyr, 
                                  seas = CPUE_combo$seas[i], 
                                  index = -CPUE_combo$index[i],
                                  obs = 1, 
                                  se_log = tmp_se_log_val) # need to make this more general.
       dat$CPUE <- rbind(dat$CPUE, tmp_CPUE_df)
      }
      if(dat$use_lencomp == 1) {
        meta_cols_lencomp <- c("Seas", "FltSvy", "Gender", "Part")
        lencomp_combo <- dat$lencomp[, meta_cols_lencomp]
        lencomp_combo$FltSvy <- abs(lencomp_combo$FltSvy)
        lencomp_combo <- unique(lencomp_combo)
        # get col names to use later
        lencomp_dat_colnames <- colnames(dat$lencomp)[7:ncol(dat$lencomp)]
        #TODO: add more options for extending len comp
        len_Nsamp_val <- get_input_value(data = dat[["lencomp"]], 
                                      method = "most_common_value", 
                                      colname = "Nsamp", 
                                      group = "FltSvy")
        if(verbose) {
          message("Input uncertainty for extending OM lencomp currently can ",
                   "only have a single value for each fleet. All lencomp data ",
                   "added to the operating model is assigned the most common ",
                   "value of Nsamp for each fleet.")
        }
        for(i in 1:nrow(lencomp_combo)) {
          tmp_len_Nsamp_val <- len_Nsamp_val[len_Nsamp_val$"FltSvy" == lencomp_combo[i, "FltSvy"], 
                                             "Nsamp"]
          assertive.properties::assert_is_atomic(tmp_len_Nsamp_val)
          assertive.properties::assert_is_of_length(tmp_len_Nsamp_val, 1)
          lencomp_df <- data.frame(Yr = (dat$endyr-nyrs_extend+1):dat$endyr, 
                                   Seas = lencomp_combo[i, "Seas"], 
                                   FltSvy = -lencomp_combo[i, "FltSvy"],
                                   Gender = lencomp_combo[i, "Gender"],
                                   Part = lencomp_combo[i, "Part"],
                                   Nsamp = tmp_len_Nsamp_val
                                   )
          #get col names
          tmp_df_dat <- matrix(1, 
                               nrow = nrow(lencomp_df),
                               ncol = length(lencomp_dat_colnames))
          colnames(tmp_df_dat) <- lencomp_dat_colnames
          lencomp_df <- cbind(lencomp_df, as.data.frame(tmp_df_dat))
          dat$lencomp <- rbind(dat$lencomp, lencomp_df)
        }
      }
      meta_cols_agecomp <- c("Seas", "FltSvy", "Gender", "Part", "Ageerr", 
                             "Lbin_lo", "Lbin_hi")
      agecomp_combo <- dat$agecomp[, meta_cols_agecomp]
      agecomp_combo$FltSvy <- abs(agecomp_combo$FltSvy)
      agecomp_combo <- unique(agecomp_combo)
      # get col names to use later
      agecomp_dat_colnames <- colnames(dat$agecomp)[10:ncol(dat$agecomp)]
      age_Nsamp_val <- get_input_value(data = dat[["agecomp"]], 
                                       method = "most_common_value", 
                                       colname = "Nsamp", 
                                       group = "FltSvy")
      if(verbose) {
        message("Input uncertainty for extending OM agecomp currently can only",
                " have a single value for each fleet. All agecomp data added ",
                "to the operating model is assigned the most common value of ",
                "Nsamp for each fleet.")
      }
      for(i in 1:nrow(agecomp_combo)) {
        tmp_age_Nsamp_val <- age_Nsamp_val[
        age_Nsamp_val$FltSvy == agecomp_combo[i, "FltSvy"], "Nsamp"]
        assertive.properties::assert_is_atomic(tmp_age_Nsamp_val)
        assertive.properties::assert_is_of_length(tmp_age_Nsamp_val, 1)
        agecomp_df <- data.frame(Yr = (dat$endyr-nyrs_extend+1):dat$endyr, 
                                 Seas    =  agecomp_combo[i, "Seas"], 
                                 FltSvy  = -agecomp_combo[i, "FltSvy"],
                                 Gender  =  agecomp_combo[i, "Gender"],
                                 Part    =  agecomp_combo[i, "Part"],
                                 Ageerr  =  agecomp_combo[i, "Ageerr"],
                                 Lbin_lo =  agecomp_combo[i, "Lbin_lo"],
                                 Lbin_hi =  agecomp_combo[i, "Lbin_hi"],
                                 Nsamp = tmp_age_Nsamp_val
        )
        tmp_df_dat <- matrix(1, 
                             nrow = nrow(agecomp_df),
                             ncol = length(agecomp_dat_colnames))
        colnames(tmp_df_dat) <- agecomp_dat_colnames
        agecomp_df <- cbind(agecomp_df, as.data.frame(tmp_df_dat))
        dat$agecomp <- rbind(dat$agecomp, agecomp_df)
      }
    } else {
      stop("Code to add in dummy data lines for a specific scheme has not yet", 
           "been implemented. Please use dummy_dat_scheme = 'all'.")
    }
  }
  # write the new data file
  if(write_dat) {
    r4ss::SS_writedat(dat, 
                      outfile = file.path(OM_dir, start$datfile),
                      overwrite = TRUE,
                      verbose = FALSE)
  }
    invisible(dat)
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
  #TODO: add checks for discards???
  #input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  summary <- r4ss::SS_read_summary(file.path(OM_dir, "ss_summary.sso"))
  if(is.null(datfile)) {
    start <- SS_readstarter(file.path(OM_dir, "starter.ss"), verbose = FALSE)
    dat   <- SS_readdat(file.path(OM_dir, start$datfile), verbose = FALSE, 
                        section = 1)
  } else {
    dat   <- SS_readdat(file.path(OM_dir, datfile), verbose = FALSE, 
                        section = 1)
  }
  if(is.null(summary)) {
    stop("File ss_summary.sso was not found in directory: ", OM_dir, ". Please",
         " add the file to the directory or change OM_dir to one with this ",
         "file.")
  }
  if(is.null(dat)) {
    stop("Datafile was not found in directory: ", OM_dir, ". Please",
         " add the file to the directory or change OM_dir to one with this ",
         "file.")
  }
  #TODO: check that can you always get biomass for any model? Probalby not if
  # catch units are in numbers. Any other scenarios when this is true?
  if(catch_units == "bio") {
    tot_bio_lyear <- 
      summary$biomass[
        grep(paste0("TotBio_", dat$endyr), rownames(summary$biomass)), ]
    if(dat$endyr >= min(catch$year)) {
      stop("The highest year for which TotBio in ss_summary.sso is available (in", 
          " the dir ", OM_dir, " is ", dat$endyr, " which is equal to or higher than ",
          "the minimum year value in catch, which is ", min(catch$year), ". ",
          "The catch should only contain values in the future compared to the ",
          "model summary.")
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
