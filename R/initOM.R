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
#' @param nyrs_assess The number of years to assess over. Used to help structure the 
#' OM projection file.
#' @param writedat Should a new datafile be written? Only used if add_dummy_dat
#'  is \code{TRUE}.
#' @param rec_devs Vector of recruitment deviations for simulation.
#' @template verbose
#' @return A new datafile as read in for r4ss, but with dummy data added.
#' @importFrom r4ss SS_readdat SS_writedat SS_readstarter SS_writestarter
create_OM <- function(OM_out_dir,
                      overwrite     = FALSE,
                      add_dummy_dat = FALSE,
                      writedat      = TRUE,
                      verbose       = FALSE,
                      nyrs_assess   = NULL,
                      rec_devs      = NULL) {
  
  #OM_out_dir<-"C:/Users/Nathan/Documents/NOAA/Red grouper/SEDAR61_BaseV/Projections/F30SPR"
  start <- r4ss::SS_readstarter(file.path(OM_out_dir, "starter.ss"), verbose = FALSE)
  start$init_values_src <- 1
  start$detailed_age_structure <- 1
  start$last_estimation_phase <- 0
  start$depl_basis <- 0
  start$depl_denom_frac <- 1
  start$SPR_basis <- 0
  start$F_report_units <- 0
  start$F_report_basis <- 0
  start$F_age_range <- NULL
  r4ss::SS_writestarter(start, dir = OM_out_dir, verbose = FALSE, overwrite = TRUE,
                        warn = FALSE)
  
  run_ss_model(OM_out_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose)
  
  dat <- r4ss::SS_readdat(file=file.path(OM_out_dir, start$datfile), 
                          verbose = FALSE, section = 1)
  forelist <- r4ss::SS_readforecast(file=file.path(OM_out_dir, "forecast.ss"),
                                    readAll=TRUE, verbose = FALSE)
  currentNforecast <- forelist$Nforecastyrs
  forelist$Nforecastyrs <- nyrs_assess+1
  forelist$FirstYear_for_caps_and_allocations <- dat$endyr + nyrs_assess + 1
  forelist$InputBasis <- 3
  
  ctl <- r4ss::SS_readctl(file.path(OM_out_dir, start$ctlfile), verbose = FALSE, 
                          use_datlist = TRUE, datlist = dat)
  
  outlist <- r4ss::SS_output(OM_out_dir, verbose = FALSE, printstats = FALSE)
  
  parlist <- r4ss::SS_readpar_3.30(parfile = file.path(OM_out_dir, "ss.par"),
                                   datsource = dat, ctlsource = ctl,
                                   verbose = FALSE)
  # use forecasting output to find F.
  temp_df <- get_F(timeseries = outlist$timeseries, 
                  units_of_catch = dat$units_of_catch,
                  Nfleet = dat$Nfleet)
  # modify the init_F and F_rate in parlist if there is retained catch.
  if(length(temp_df[temp_df$Era=="TIME" & temp_df$Catch_retained!=0,1])>0){
    parlist$F_rate<-temp_df[temp_df$Era=="TIME" & temp_df$Catch_retained!=0,c(1,3,4,5)]
  }
  if(length(temp_df[temp_df$Era=="INIT" & temp_df$Catch_retained!=0,5])>0){
    parlist$init_F<-temp_df[temp_df$Era=="INIT" & temp_df$Catch_retained!=0,5]
  }
  # add recdevs to the parlist
  # TODO: check that these are implicated correctly
  parlist$recdev_forecast <- 
    get_rec_devs_matrix(yrs = (dat$endyr+1):(dat$endyr+forelist$Nforecastyrs),
                        rec_devs = rec_devs)
  # add implementation error to the parlist (for now, none is added)
  parlist$Fcast_impl_error <-
    get_impl_error_matrix(yrs = (dat$endyr+1):(dat$endyr+forelist$Nforecastyrs),
                          impl_error = 0)
  #ctl file changes needed for F.
  ctl$F_Method <- 2 # Want all OMs to use F_Method = 2.
  ctl$F_iter <- NULL
  ctl$F_setup <- c(0.05,1,0)
  ctl$F_setup2 <-NULL
  # forecast file changes needed
  temp_fore<-temp_df[temp_df$Era=="FORE",c(1,3,4,6)]
  row.names(temp_fore)<-NULL
  names(temp_fore)<-c("Year","Seas","Fleet","Catch or F")
  forelist$ForeCatch<-temp_fore[is.element(temp_fore$Year,(dat$endyr+1):(dat$endyr+nyrs_assess+1)),]
  # write all files
  r4ss::SS_writectl(ctllist=ctl,outfile = file.path(OM_out_dir, start$ctlfile),
                    overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writeforecast(mylist=forelist, dir=OM_out_dir, writeAll = TRUE, 
                         overwrite = TRUE, verbose = FALSE)
  r4ss::SS_writepar_3.30(parlist = parlist,
                         outfile = file.path(OM_out_dir, "ss.par"),
                         overwrite = TRUE)
  
  if(add_dummy_dat) {
    # TODO: develop code to do this for other types of data (mean length at age)
    # get minimum and maximum years for the model (in dat)
    # add in dummy values to CPUE, length comp, age comp by using -fleet.
    # CPUE
    #TODO: refactor and write a function.
    old_CPUE <- dat$CPUE
    new_CPUE <- old_CPUE # initialize, because want all old values, plus some new ones
    CPUE_seas_flt <- unique(dat$CPUE[, c("seas", "index")])
    dat_yrs <- dat$styr:dat$endyr
    # get a df of the se values to use.
    se_log_val <- get_input_value(data = old_CPUE, 
                                  method = "most_common_value", 
                                  colname = "se_log", 
                                  group = "index")
    if(verbose) {
      message("Input uncertainty for CPUE OM currently can only have a single ", 
              "value per fleet. All CPUE data added to the operating model is ",
               "assigned the most common value of se_log for each fleet.")
    }
    for(i in 1:nrow(CPUE_seas_flt)) {
      #get currently  used values
      tmp_seas <- CPUE_seas_flt[i, "seas"]
      tmp_flt  <- CPUE_seas_flt[i, "index"]
      # below is the data for the season and fleet combo that already in the OM.
      tmp_CPUE <-  old_CPUE[(old_CPUE$seas == tmp_seas & old_CPUE$index == tmp_flt), ]
      # find which years need to be added
      tmp_miss_yrs <- dat_yrs[!dat_yrs %in% tmp_CPUE$year]
      # get the standard error from the old CPUE. For now, just use the 
      # most common method (hard coded)
      #tmp_se_log_val should be a single value
      tmp_se_log_val <- se_log_val[se_log_val$index == tmp_flt, "se_log"]
      assertive.properties::assert_is_atomic(tmp_se_log_val)
      assertive.properties::assert_is_of_length(tmp_se_log_val, 1)
      #TODO: make se_log more general by allowing more options for "method: (
      # may need to read in a user value))
      tmp_df <- data.frame(year  = tmp_miss_yrs,
                     seas  = tmp_seas,
                     index = -tmp_flt, 
                     obs   = 1,
                     se_log = tmp_se_log_val,
                     stringsAsFactors = FALSE)
      # add these to the CPUE
      new_CPUE <- rbind(new_CPUE, tmp_df)
    }
    #overwrite the old lines
    dat$CPUE <- new_CPUE
    #length comp
    if(dat$use_lencomp == 1) {
      old_lencomp <- dat$lencomp
      new_lencomp <- old_lencomp
      meta_cols <- c("Seas", "FltSvy", "Gender", "Part")
      lcomp_combo <- unique(dat$lencomp[, meta_cols])
      len_Nsamp_val <- get_input_value(data = old_lencomp, 
                                       method = "most_common_value", 
                                       colname = "Nsamp", 
                                       group = "FltSvy")
      if(verbose) {
        message("Input uncertainty for lencomp OM currently can only have a ",
                "single value for each fleet. All lencomp data added to the ",
                "operating model is assigned the most common value of Nsamp ",
                "for each fleet.")
      }
      for(i in seq_len(nrow(lcomp_combo))) {
        #get currently  used values (write a lapply function to make more concise)
        tmp_metacols <- vapply(meta_cols, 
                               function(col, i, lcomp_combo) lcomp_combo[i, col],
                               FUN.VALUE = 1,
                               i = i, lcomp_combo = lcomp_combo, 
                               USE.NAMES = TRUE)
        tmp_lencomp <-  old_lencomp[old_lencomp$Seas == tmp_metacols["Seas"] & 
                                 old_lencomp$FltSvy == tmp_metacols["FltSvy"] &
                                 old_lencomp$Gender == tmp_metacols["Gender"] &
                                 old_lencomp$Part == tmp_metacols["Part"], ]
        # find which years need to be added
        tmp_miss_yrs_lencomp <- dat_yrs[!dat_yrs %in% tmp_lencomp$Yr]
        # get the Nsamp
        #TODO: add more options for getting sample size.
        tmp_len_Nsamp_val <- len_Nsamp_val[
          len_Nsamp_val$FltSvy == tmp_metacols["FltSvy"], "Nsamp"]
        assertive.properties::assert_is_atomic(tmp_len_Nsamp_val )
        assertive.properties::assert_is_of_length(tmp_len_Nsamp_val, 1)
        # used suppressWarning because creates an unwanted warning that can be 
        # safely ignored
        suppressWarnings(tmp_df_lencomp <- data.frame(Yr     = tmp_miss_yrs_lencomp,
                                     Seas   = tmp_metacols["Seas"],
                                     FltSvy = -tmp_metacols["FltSvy"], 
                                     Gender = tmp_metacols["Gender"],
                                     Part   = tmp_metacols["Part"],
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
      #overwrite the old lines
      dat$lencomp <- new_lencomp
    }
    #agecomp
    old_agecomp <- dat$agecomp
    new_agecomp <- old_agecomp
    meta_cols_agecomp <- c("Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo", 
                   "Lbin_hi")
    agecomp_combo <- unique(dat$agecomp[, meta_cols_agecomp])
    # get the Nsamp
    #TODO: add more options for getting sample size.
    age_Nsamp_val <- get_input_value(data = old_agecomp, 
                                     method = "most_common_value", 
                                     colname = "Nsamp", 
                                     group = "FltSvy")
    if(verbose) {
      message("Input uncertainty for agecomp OM currently can only have a ",
              "single value per fleet. All lencomp data added to the operating",
              " model is assigned the most common value of Nsamp for each ", 
              "fleet.")
    }
    for(i in 1:nrow(agecomp_combo)) {
      #get currently  used values (write a lapply function to make more concise)
      tmp_metacols <- vapply(meta_cols_agecomp, 
                             function(col, i, agecomp_combo) agecomp_combo[i, col],
                             FUN.VALUE = 1,
                             i = i, agecomp_combo = agecomp_combo, 
                             USE.NAMES = TRUE)
      tmp_agecomp <-  old_agecomp[
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
      suppressWarnings(tmp_df_agecomp <- data.frame(Yr      = tmp_miss_yrs_agecomp,
                                   Seas    = tmp_metacols["Seas"],
                                   FltSvy  = -tmp_metacols["FltSvy"], 
                                   Gender  = tmp_metacols["Gender"],
                                   Part    = tmp_metacols["Part"],
                                   Ageerr  = tmp_metacols["Ageerr"],
                                   Lbin_lo = tmp_metacols["Lbin_lo"],
                                   Lbin_hi = tmp_metacols["Lbin_hi"],
                                   Nsamp   = tmp_age_Nsamp_val
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
    #overwrite the old lines
    dat$agecomp <- new_agecomp
    if(writedat) {
      SS_writedat(dat, file.path(OM_out_dir, start$datfile), overwrite = overwrite, 
                  verbose = FALSE)
    }
  }
  
  # validate using model as an OM? may want to make a seperate function that can
  # validate if the model can be used as an OM or not.
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
#' @author Kathryn Doering
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
run_OM <- function(OM_dir, 
                        boot = TRUE,
                        nboot = 1, 
                        init_run = FALSE, 
                        verbose = FALSE) {
  # make sure OM generates the correct number of data sets.
  if (boot) {
    max_section <- nboot + 2
  } else {
    max_section <- 2
  }
  if(init_run == TRUE) {
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"), 
                                verbose = FALSE)
  start$N_bootstraps <- max_section
  r4ss::SS_writestarter(start, dir = OM_dir, verbose = FALSE, overwrite = TRUE,
                        warn = FALSE)
  }
  # run SS and get the data set
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess", verbose = verbose)

  dat <- r4ss::SS_readdat(file.path(OM_dir,"data.ss_new"), 
                          section = max_section, 
                          verbose = FALSE)
  # If using bootstrap, do not want to use bootstrapped catch. Instead, replace
  # with the expected catch values.
  # TODO: may want to add check if using F method 1 or 3 that the expected vals
  # and the input values match. May not match for method 2.
  if (max_section > 2) {
    if(verbose) message("Adding expected catch to the bootstrapped dataset.")
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
                                 dat_types = c("CPUE","lencomp", "agecomp")) {
  #TODO: write this. Can be used for EM and OM.
}
