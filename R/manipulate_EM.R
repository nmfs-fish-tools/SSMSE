# functions to manipulate the estimation model.

#' Change dataset from OM into format for EM
#' @param OM_datafile Relative or absolute path with filename to the
#' datafile to use.
#' @param EM_dir Absolute or relative path to the Estimation model directory.
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @template verbose
#' @importFrom r4ss SS_readstarter SS_readdat SS_writedat SS_writestarter 
#' @examples \dontrun{
#' #TODO: Add example
#' }
change_data <- function(OM_datafile, EM_dir, do_checks = TRUE, verbose = FALSE) {
  EM_dir <- normalizePath(EM_dir)
  # checks
  check_dir(EM_dir)
  # get the name of the original datafile
  start <- SS_readstarter(file.path(EM_dir, "starter.ss"), verbose = verbose)
  orig_dat <- SS_readdat(file.path(EM_dir, start$datfile), verbose = verbose)
  OM_dat <- SS_readdat(file.path(EM_dir, OM_datafile), verbose = verbose)
  # remove extra years of data in the OM data file, maybe something in ss3sim?
  new_EM_dat <- get_EM_dat(OM_dat = OM_dat, EM_dat = orig_dat, 
                           do_checks = do_checks)
  
  # write out the modified files that can be used in future EM run
  SS_writedat(new_EM_dat, file.path(EM_dir, "init_dat.ss"), verbose = verbose)
  start$datfile <- "init_dat.ss"
  SS_writestarter(start, dir = EM_dir, verbose = verbose, overwrite = TRUE)
  invisible(OM_datafile) # b/c function written for side effects.
}

#' Change the OM data to match the format of the EM data
#'
#' This does the technical part of changing the EM data
#' @param OM_dat An SS data file read in by as a list read in using r4ss from 
#'  the operating model 
#' @param EM_dat An SS data file read in by as a list read in using r4ss from 
#'  the estimation model
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @return A data list in the same format that can be read/written by r4ss that
#'  has index. lcomps, and age comps from OM_dat, but with the same structure as 
#'  EM_dat.
get_EM_dat <- function(OM_dat, EM_dat, do_checks = TRUE) {
  new_dat <- EM_dat #start by copying over to get the correct formatting.
  #TODO: add in code to copy over mean size and mean size at age obs.
  # add in index
  if(do_checks) {
  check_OM_dat(OM_dat, EM_dat)
  }
  dat <- list(OM_dat = OM_dat, EM_dat = EM_dat)
  #' create a function that creates a combined column to the list_item of interest
  #'
  #' @param dat_list An SS data file as a list read in using r4ss
  #' @param list_item List item in dat_list to extract and return a modified
  #'  version of this value
  #' @param colnames Column names in list_item
  #' @noRd
  combine_cols <- function(dat_list, list_item, colnames) {
    tmp <- dat_list[[list_item]]
    combo <- NULL
    for(n in colnames) {
      combo <- paste0(combo, tmp[, n], "_")
    }
    tmp$combo <- combo
    tmp
  }
  CPUEs <- lapply(dat, function(x){
    tmp <- combine_cols(x, "CPUE", c("year", "seas", "index"))
 }) 
  # match 1 way: match each EM obs with an OM obs. extract only these OM obs.
   matches <- which(CPUEs[[1]][, "combo"] %in% CPUEs[[2]][, "combo"])
   # extract only the rows of interest and get rid of the "combo" column
   new_dat$CPUE <- CPUEs[[1]][matches, -ncol(CPUEs[[1]])]
  # add in lcomps
   if(OM_dat$use_lencomp == 1) {
     lcomps <- lapply(dat, function(x) {
       tmp <- combine_cols(x, "lencomp",
                           c("Yr", "Seas", "FltSvy", "Gender", "Part"))
     })
     matches_l <- which(lcomps[[1]][, "combo"] %in% lcomps[[2]][,"combo"])
     new_dat$lencomp <- lcomps[[1]][matches_l, -ncol(lcomps[[1]])]
   }
  # add in age comps
  acomps <- lapply(dat, function(x) {
    tmp <- combine_cols(x, "agecomp", 
             c("Yr", "Seas", "FltSvy", "Gender", "Part", "Lbin_lo", "Lbin_hi"))
  })
  matches_a <- which(acomps[[1]][, "combo"] %in% acomps[[2]][,"combo"])
  new_dat$agecomp <- acomps[[1]][matches_a, -ncol(acomps[[1]])]
  #return
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
#' @param nyrs_proj The number of years of catch to be returned. Only used if
#'  change_fcast = TRUE.
#' @param set_use_par Should input values be read from the .par file? If TRUE,
#' will change setting in the starter file; otherwise, will use the setting
#' already in the starter file, which may or may not read from the .par file.
#' @param change_fcast Should number of years be changed in forecast? Defaults
#'  to TRUE.
#' @param seas Season for catch to be output. Defaults to 1.
#' @param catch_se Catch standard error for catch to be output. Defaults to 
#'  0.01. 
#' @template verbose
#' @export
#' @importFrom r4ss SS_readforecast SS_writeforecast SS_readstarter SS_writestarter SS_read_summary
#' @return The future catch in a dataframe, based on the EM run and the harvest
#' strategy outlined within the SS forecasting module.
run_EM <- function(EM_dir, 
                   hess = FALSE, 
                   check_converged = TRUE, 
                   nyrs_proj,
                   set_use_par = FALSE,
                   change_fcast = FALSE,
                   seas = 1,
                   catch_se = 0.01,
                   verbose = FALSE) {
  EM_dir <- normalizePath(EM_dir)
  # checks
  check_dir(EM_dir)
  # set up to run the EM
  if(set_use_par == TRUE) {
    start <- SS_readstarter(file.path(EM_dir, "starter.ss"), verbose = verbose)
    start$init_values_src <- 1
    SS_writestarter(start, dir = EM_dir, overwrite = TRUE, verbose = verbose,
                    warn = FALSE)
  }
  if(change_fcast == TRUE) {
    # make sure enough yrs can be forecasted.
    fcast <- SS_readforecast(file.path(EM_dir, "forecast.ss"),
                                    readAll = TRUE,
                                    verbose = verbose)
    fcast$Nforecastyrs <- nyrs_proj
    SS_writeforecast(fcast, dir = EM_dir, writeAll = TRUE, overwrite = TRUE,
                     verbose = verbose)
  }
  if (hess == TRUE) {
    options <- ""
  } else {
   options  <- "-nohess"
  }
  run_ss_model(EM_dir, options)
  if(check_converged == TRUE) {
    # TODO: add additional checks for convergence?
    warn <- readLines(file.path(EM_dir, "warning.sso"))
    grad_warn <- grep("^Final gradient\\:\\s+\\d*\\.\\d*\\sis larger than final_conv\\:", warn)
    if(length(grad_warn) > 0) {
      warning("Estimation model did not converge this iteration.")
      #TODO: decide if this should be a stop() message instead? 
    }
  }
  # get projected catch values
  sum <- SS_read_summary(file.path(EM_dir, "ss_summary.sso"))
  # get the catch and year values.
  all_fore_catch <- sum$derived_quants[
    grep("^ForeCatch_\\d+$", rownames(sum$derived_quants)), ]
  yrs <- strsplit(rownames(all_fore_catch), "_", fixed = TRUE) 
  yrs <- as.integer(unlist(lapply(yrs, function(x) x[2])))
  # For now, assume in order and want to use all values. May want to add check?
  #TODO: modify for use with multiple fleets, areas, etc.
  catch <- data.frame(
             year = yrs,
             seas = seas,
             fleet = 1, 
             catch = all_fore_catch[, "Value"],
             catch_se = catch_se)
  # check the df produced and return
  check_catch_df(catch)
  catch
}

#' Add new data to an existing EM dataset
#' 
#' This should be used for the feedback loops when an EM is used.
#' @param OM_data An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @param EM_datafile Datafile name run in previous iterations with the EM.
#'  Assumed to exist in EM_dir.
#' @param dat_str A optional list including which years and fleets should be 
#'  added from the OM into the EM for different types of data. If NULL, the data
#'  structure will try to be infered from the pattern found for each of the 
#'  datatypes within EM_datafile.
#' @param EM_dir Absolute or relative path to the Estimation model directory.
#' @param do_checks Should checks on the data be performed? Defaults to TRUE.
#' @param new_datafile_name An optional name of a file to write the new datafile
#'  to. If NULL, a new datafile will not be written.
#' @template verbose
#' @return A new SS datafile containing the data in EM_datafile with new data 
#' from OM_data appended
#' @importFrom r4ss SS_readdat SS_writedat
#' @importFrom stats na.omit
add_new_dat <- function(OM_data, 
                        EM_datafile,
                        dat_str = NULL,
                        EM_dir, 
                        do_checks = TRUE,
                        new_datafile_name = NULL,
                        verbose = FALSE) {

  if(do_checks) {
    # TODO: do input checks: check OM_data is valid r4ss list, check data. only do if
    # do_checks = TRUE?
    if(OM_data$type != "Stock_Synthesis_data_file") {
      r4ss_obj_err("OM_data", "data list")
    }
    if(!is.null(dat_str)) check_dat_str(dat_str)
  }
  # Read in EM_datafile
  EM_data <- SS_readdat(file.path(EM_dir, EM_datafile), verbose = verbose)
  # add the data from OM_data into EM_data
  if(is.null(dat_str)) {
    stop("Option to determine sampling from EM_datafile not yet developed. ",
         "Please specify sampling using dat_str.")
    # see if there is a consistent pattern in sampling design in EM_datafile
    # if so, use this pattern to extract data from OM_data
    # stop on error (or generate warning and add all data??) if cannot determine
     # a specific pattern
  } else {
    # checks in relation to OM_data: check that years, fleets, etc. ar valid
    
    # extract data from OM_data based on valid data structure

   extracted_dat <- 
     mapply(
      function(df, df_name, OM_data) {
        OM_df <- OM_data[[df_name]]
        new_dat <- merge(df, OM_df, all.x = TRUE, all.y = FALSE)
        # warn if there were matches not found for OM_df, but remove to continue
        if(any(is.na(new_dat))) {
          warning("Some values specified in dat_str (list component ", df_name,
                  ") were not found in OM_data, so they will not be added to ",
                  "the EM_dat.")
          new_dat <- na.omit(new_dat)
        }
        new_dat
      }, 
      df = dat_str, df_name = names(dat_str), 
      MoreArgs = list(OM_data = OM_data), 
      SIMPLIFY = FALSE, USE.NAMES = TRUE)
    # insert this data into the EM_datafile
   new_EM_data <- EM_data
   for(n in names(extracted_dat)) {
    new_EM_data[[n]] <- rbind(new_EM_data[[n]], extracted_dat[[n]])
   }
  # write the new datafile if new_datafile_name isn't NULL
    if(!is.null(new_datafile_name)) {
      SS_writedat(new_EM_data, 
                  file.path(EM_dir, new_datafile_name),
                  overwrite = TRUE,
                  verbose = verbose)
    }
  }
   new_EM_data
}

