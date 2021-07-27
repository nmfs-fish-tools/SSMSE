# Functions associated with initialization of the OM.

#' Create the OM
#' 
#' This function manipulates the OM as needed so that it can be used as an
#'  operating model.
#' @author Kathryn Doering & Nathan Vaughan
#' @param OM_out_dir The full path to the directory in which the OM is run.
#' @param overwrite Overwrite existing files with matching names?
#' @param nyrs Number of years beyond the years included in the OM to run the
#'   MSE. A single integer value.
#' @param nyrs_assess The number of years between assessments. This is used to
#'  structure the forecast file for use in the OM.
#' @param nscen The scenario number
#' @param scen_name The scenario name
#' @param niter the iteration number
#' @param writedat Should a new datafile be written?
#' @param future_om_dat An optional data_frame including changes that should
#'  be made after the end year of the input model. Including parameter variations,
#'  recruitment deviations, and implementation errors.
#' @param verify_OM Should the model be run without estimation and some basic
#'  checks done to verify that the OM can run? Defaults to TRUE.
#' @param sample_struct_hist The historical sample structure, if specified by 
#'  the user. Defaults to NULL, which means to use the same sample structure as
#'  the historical data.
#' @param sample_struct Input sampling structure to ensure future data are listed in OM
#'  with correct SE.
#' @param seed input seed to allow reproducible SS results.
#' @template verbose
#' @return A modified datafile
#' @import r4ss
create_OM <- function(OM_out_dir,
                      overwrite = TRUE,
                      writedat = TRUE,
                      verbose = FALSE,
                      nyrs = NULL,
                      nyrs_assess = NULL,
                      nscen = 1, 
                      scen_name = NULL,
                      niter = 1,
                      future_om_dat = NULL,
                      verify_OM = TRUE,
                      sample_struct_hist = NULL,
                      sample_struct = NULL,
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
  
  forelist[["benchmarks"]] <- 0 # 
  forelist[["Forecast"]] <- 0 # 
  forelist[["Nforecastyrs"]] <- 1 # 
  forelist[["FirstYear_for_caps_and_allocations"]] <- dat[["endyr"]] + nyrs + 2 # 
  
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

  if (ctl[["recdev_early_start"]] <= 0) {
    first_year <- ctl[["MainRdevYrFirst"]] + ctl[["recdev_early_start"]]
  } else if (ctl[["recdev_early_start"]] < ctl[["MainRdevYrFirst"]]) {
    first_year <- ctl[["recdev_early_start"]]
  } else {
    (
      first_year <- ctl[["MainRdevYrFirst"]]
    )
  }
  
  # modify par file ----
  all_recdevs <- as.data.frame(rbind(parlist[["recdev1"]], parlist[["recdev2"]], parlist[["recdev_forecast"]]))
  # get recdevs for all model years
  all_recdevs <- all_recdevs[all_recdevs[["year"]] >= first_year & all_recdevs[["year"]] <= (dat[["endyr"]]), ] # 
  
  new_recdevs_df <- data.frame(year = first_year:ctl[["MainRdevYrLast"]], recdev = NA)
  fore_recdevs_df <- data.frame(year = (ctl[["MainRdevYrLast"]] + 1):(dat[["endyr"]] + nyrs + 1), recdev = NA) # 
  temp_yrs<-(first_year:(dat[["endyr"]] + nyrs + 1))
  for (i in seq_along(temp_yrs)) { # 
    tmp_yr <- temp_yrs[i] #
    if (tmp_yr <= ctl[["MainRdevYrLast"]]) {
      step <- i
      if (length(all_recdevs[all_recdevs[["year"]] == tmp_yr, "year"]) == 0) {
        new_recdevs_df[i, "recdev"] <- 0 # just assume no rec devs
      } else {
        new_recdevs_df[i, "recdev"] <-
          all_recdevs[all_recdevs[["year"]] == tmp_yr, "recdev"]
      }
    } else {
      if (length(all_recdevs[all_recdevs[["year"]] == tmp_yr, "year"]) == 0) {
        fore_recdevs_df[(i - step), "recdev"] <- 0
      } else {
        fore_recdevs_df[(i - step), "recdev"] <-
          all_recdevs[all_recdevs[["year"]] == tmp_yr, "recdev"]
      }
    } 
  }
  
  # add recdevs to the parlist
  new_recdevs_mat <- as.matrix(new_recdevs_df)
  new_fore_recdevs_mat <- as.matrix(fore_recdevs_df)
  if (!is.null(parlist[["recdev1"]])) {
    parlist[["recdev1"]] <- new_recdevs_mat
  } else if (!is.null(parlist[["recdev2"]])) {
    parlist[["recdev2"]] <- new_recdevs_mat
  } else {
    stop("no recdevs in initial OM model. something is wrong")
  }

  parlist[["recdev_forecast"]] <- new_fore_recdevs_mat
  
  # use report.sso time series table to find the F's to put into the parlist.
  F_list <- get_F(
    timeseries = outlist[["timeseries"]],
    fleetnames = dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "fleetname"]
  )
  
  # SINGLE_RUN_MODS:
  update_F_years <- (dat[["endyr"]]+1):(dat[["endyr"]]+nyrs)
  
  default_Catch<-data.frame(year=sort(rep(update_F_years,(length(unique(dat[["catch"]][,"seas"]))*length(unique(dat[["catch"]][,"fleet"]))))),
                            seas=rep(sort(rep(unique(dat[["catch"]][,"seas"]),length(unique(dat[["catch"]][,"fleet"])))),length(update_F_years)),
                            fleet=rep(sort(unique(dat[["catch"]][,"fleet"])),(length(unique(dat[["catch"]][,"seas"]))*length(update_F_years))),
                            catch=rep(0.001,(length(unique(dat[["catch"]][,"seas"]))*length(unique(dat[["catch"]][,"fleet"]))*length(update_F_years))),
                            catch_se=rep(0.01,(length(unique(dat[["catch"]][,"seas"]))*length(unique(dat[["catch"]][,"fleet"]))*length(update_F_years)))
                            )
  
  new_catch <- dat[["catch"]][((dat[["catch"]][,"year"]<=dat[["endyr"]] & dat[["catch"]][,"year"]>=(-dat[["endyr"]]))|dat[["catch"]][,"year"]==-9999),,drop=FALSE]
  new_catch <- rbind(new_catch,default_Catch)
  
  new_catch <- new_catch[order(new_catch[, "fleet"], new_catch[, "year"], new_catch[, "seas"]), ]
  
  update_SE_catch <- function(new_catch,sample_struct_in){
    temp_samp <- sample_struct_in[sample_struct_in[,"year"]==new_catch[1] &
                                  sample_struct_in[,"seas"]==new_catch[2] &
                                  sample_struct_in[,"fleet"]==new_catch[3], "catch_se"]
    if(length(temp_samp)==1){
      new_SE <- temp_samp
    }else{
      new_SE <- new_catch[5]
    }
    return(new_SE)
  }
  if(!is.null(sample_struct[["catch"]])){
    new_catch[,"catch_se"] <- apply(new_catch,1,update_SE_catch,sample_struct_in = sample_struct[["catch"]])
  }
  
  dat[["catch"]] <- new_catch
  
  
  
  default_F <- F_list[["F_rate"]][F_list[["F_rate"]][,"year"]==dat[["endyr"]],c("year", "seas", "fleet", "F")]  
  new_F_rate <- rbind(F_list[["F_rate"]][, c("year", "seas", "fleet", "F")],F_list[["F_rate_fcast"]][,c("year", "seas", "fleet", "F")])  
  rownames(new_F_rate) <- c(F_list[["F_rate"]][, c("name")],F_list[["F_rate_fcast"]][,c("name")])  
  
  for(i in update_F_years){  
    for(j in unique(new_F_rate[,"seas"])){    
      for(k in unique(new_F_rate[,"fleet"])){      
        temp_F_rate <- new_F_rate[new_F_rate[,"year"]==i & new_F_rate[,"seas"]==j & new_F_rate[,"fleet"]==k,,drop=FALSE]        
        if(length(temp_F_rate[,1])==0){        
          if(length(default_F[default_F[,"seas"]==j & default_F[,"fleet"]==k,"F"])==0){          
            temp_F_rate[1,] <- c(i,j,k,0)
            rownames(temp_F_rate[1,]) <-  paste0("F_fleet_", k, "_YR_", i, "_s_", j)
            default_F <- rbind(default_F,temp_F_rate[1,,drop=FALSE])       
            new_F_rate <- rbind(new_F_rate,temp_F_rate[1,,drop=FALSE])             
          }else{           
            temp_F_rate[1,] <- c(i,j,k,default_F[default_F[,"seas"]==j & default_F[,"fleet"]==k,"F"][1])  
            rownames(temp_F_rate[1,]) <-  paste0("F_fleet_", k, "_YR_", i, "_s_", j)
            new_F_rate <- rbind(new_F_rate,temp_F_rate[1,,drop=FALSE])             
          }
        }else{   
          if(length(default_F[default_F[,"seas"]==j & default_F[,"fleet"]==k,"F"])==0){   
            default_F<-rbind(default_F,temp_F_rate[1,,drop=FALSE]) 
          }else{         
            default_F[default_F[,"seas"]==j & default_F[,"fleet"]==k,] <- temp_F_rate[1,,drop=FALSE]   
          }
        }  
      } 
    }
  } 
  new_F_rate <- new_F_rate[order(new_F_rate[, "fleet"], new_F_rate[, "year"], new_F_rate[, "seas"]), ]
  
  rownames(new_F_rate) <- paste0(
    "F_fleet_", new_F_rate[["fleet"]], "_YR_", new_F_rate[["year"]], "_s_",
    new_F_rate[["seas"]]
  )
  
  parlist[["F_rate"]]<-new_F_rate  
  
  parlist[["init_F"]] <- F_list[["init_F"]]
  
  parlist[["Fcast_impl_error"]] <- get_impl_error_matrix(yrs = (dat[["endyr"]] + nyrs +1 ):(dat[["endyr"]] + nyrs + forelist[["Nforecastyrs"]]))

  ctl[["F_Method"]] <- 2 # Want all OMs to use F_Method = 2.
  ctl[["F_setup"]] <- c(0.05, 1, 0) # need to specify some starting value Fs, although not used in OM
  ctl[["F_iter"]] <- NULL # make sure list components used by other F methods are NULL:
  ctl[["F_setup2"]] <- NULL # make sure list components used by other F methods are NULL:

  single_run_files <- add_OM_devs(ctl=ctl, dat=dat, parlist=parlist, timeseries=outlist[["timeseries"]], future_om_dat=future_om_dat) 
  
  dat<-single_run_files[["data"]] # SINGLE_RUN_MODS: 
  ctl<-single_run_files[["control"]] # SINGLE_RUN_MODS: 
  parlist<-single_run_files[["parameter"]] # SINGLE_RUN_MODS: 
  impl_error<-single_run_files[["impl_error"]]
  
  if(is.null(impl_error)){
    impl_error <- data.frame("year"=(dat[["endyr"]]+1):(dat[["endyr"]]+nyrs),
                             "error"=rep(1,nyrs))
  }
  
  dat[["endyr"]] <- dat[["endyr"]] + nyrs # SINGLE_RUN_MODS: 
  
  # modify dat file ----
  # remove the sampling components not needed
  dat <- rm_sample_struct_hist(sample_struct = sample_struct_hist, dat = dat)
  # Add in the historical sampling structure, as defined by the user
  dat <- add_sample_struct(sample_struct = sample_struct_hist, dat = dat, 
                           nyrs_extend = 0)
  dat <- add_sample_struct(sample_struct = sample_struct, dat = dat, 
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
    if (!file.exists(file.path(OM_out_dir, "control.ss_new"))) {
      stop(
        "The OM model created is not valid; it did not run and produce a\n",
        "control.ss_new file. Please try running the OM model created\n",
        "with the create_OM function manually with SS to diagnose the\n",
        "problem."
      )
    }
    # check model runs without producing nans in the data file
    tmp_new_dat <- readLines(file.path(OM_out_dir, "data.ss_new"))
    nan_vals <- grep("nan", tmp_new_dat)
    if(length(nan_vals) > 0) {
      stop("NAN values present in the data.ss_new om file, suggesting an issue ",
           "setting up the OM. See ", file.path(OM_out_dir, "data.ss_new"))
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
    if (NROW(F_rate_pars) != NROW(parlist[["F_rate"]])) {
      stop("Wrong number of F_rate parameters assumed by create_OM function.")
    }
    if (NROW(F_rate_pars) > 0) {
      if (!all(F_rate_pars[["Label"]] == rownames(parlist[["F_rate"]]))) {
        stop(
          "Names of F_rate parameters assumed by create_OM function and in\n",
          "the PARAMETERS table of Report.sso function do not match."
        )
      }
    }
  }
  
  ouput_list<-list(dat=dat,impl_error=impl_error)
  return(ouput_list)
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