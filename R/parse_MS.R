# Parse management strageties.


#' Parse management strategy options
#' 
#' This function matches each management strategy with its correct method. And
#' checks for errors.
#' @template MS
#' @param EM_name Should be NULL unless \code{MS = "EM"}. Name of a valid Stock
#'   Synthesis stock assessment model to use as an EM. If the value of EM_name 
#'   is NULL and  \code{MS = "EM"}, then SSMSE will look for the estimation model
#'   in the path specified in EM_dir. valid inputs for EM_name are: \code{"cod"}
#'   or \code{NULL}.
#' @param EM_dir Relative or absolute path to the estimation model, if using a 
#'   model outside of the SSMSE package. Note that this value should be NULL if
#'   \code{MS} has a value other than \code{"EM"}.
#' @param init_loop Logical. If this is the first initialization loop of the 
#'   MSE, \code{init_loop} should be TRUE. If it is in further loops, it should
#'   be FALSE.
#' @param out_dir The directory to which to write output. IF NULL, will default
#'   to the working directory.
#' @param OM_data An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @template verbose
#' @param nyrs_assess The number of years between assessments. E.g., if an
#'   assessment is conducted every 3 years, put 3 here. A single integer value.
#' @param dat_yrs Which years should be added to the new model? Ignored if
#'  init_loop is TRUE.
#' @param dat_str A optional list including which years and fleets should be 
#'  added from the OM into the EM for different types of data. If NULL, the data
#'  structure will try to be infered from the pattern found for each of the 
#'  datatypes within EM_datafile. Ignored if init_loop is TRUE.
#' @importFrom r4ss SS_readstarter SS_writestarter SS_writedat
#' @importFrom SSutils copy_SS_inputs
parse_MS <- function(MS, EM_name = NULL, EM_dir = NULL, init_loop = TRUE, 
                     out_dir, OM_data, verbose = FALSE, nyrs_assess, dat_yrs,
                     dat_str) {
  # input checks ----
  if(init_loop) {
    valid_MS <- c("EM", "no_catch", "last_yr_catch")
    if (!MS %in% valid_MS) {
      stop("MS was input as ", MS, ", which is not valid. Valid options: ",
           valid_MS)
    }
    if(MS == "EM") {
      if(is.null(EM_name) & is.null(EM_dir)){
        stop("Management Strategy (MS) is EM (estimation model, but both EM_name", 
             " and EM_dir are null. Please specify either EM_name or EM_dir, or ",
             "change the management strategy.")
      }
      if(!is.null(EM_name) & !is.null(EM_dir)) {
        stop("Management Strategy (MS) is EM (estimation model, but both EM_name", 
             " and EM_dir are specified. Please specify either EM_name or EM_dir", 
             "or change the management strategy.")
      }
      if(!is.null(EM_name)){
        pkg_dirs <- list.dirs(system.file("extdata", "models", package = "SSMSE"))
        pkg_mods <- list.dirs(system.file("extdata", "models", package = "SSMSE"), full.names = FALSE)
        pkg_mods <- pkg_mods[pkg_mods != ""]
        orig_EM_dir <- pkg_dirs[grep(EM_name, pkg_dirs, fixed = TRUE)]
        if(length(orig_EM_dir) == 0) {
          stop("Currently, EM_name can only be one of the following: ", pkg_mods)
        }
      }
      if(!is.null(EM_dir)) check_dir(EM_dir) # make sure contains a valid model
    }
  }
  # parsing management strategies ----
  out_dir <- normalizePath(out_dir)
  if(MS == "EM") {
    # create folder 
    if(!is.null(EM_name)) {
      EM_dir <- file.path(out_dir, paste0(EM_name, "_EM"))
    }
    if(init_loop) {
      copy_SS_inputs(dir.old = orig_EM_dir, dir.new = EM_dir, overwrite = TRUE)
    }
    check_dir(EM_dir)
    if(init_loop) {
    # copy over raw data file from the OM
    SS_writedat(OM_data, 
                      file.path(EM_dir, "init_dat.ss"), 
                      overwrite = TRUE, 
                      verbose = verbose)
    # change the name of data file.
    start <- SS_readstarter(file.path(EM_dir, "starter.ss"), 
                                  verbose = verbose)
    start$datfile <- "init_dat.ss"
    SS_writestarter(start, file.path(EM_dir), verbose = verbose,
                    overwrite = TRUE)
    # make sure the data file has the correct formatting (use existing data 
    #file in the EM directory to make sure)??
    change_data(OM_datafile = "init_dat.ss",
                EM_dir = EM_dir,
                do_checks = TRUE,
                verbose = verbose)
    } else {
      if(!is.null(dat_str)) {
        dat_str_sub <- lapply(dat_str,
                              function(df, y) df[df[,1] %in% y, ],
                              y = dat_yrs)
      } else {
        dat_str_sub <- NULL
      }
      new_EM_data <- add_new_dat(OM_data = OM_data,
                                 EM_datafile = "init_dat.ss",
                                 dat_str = dat_str_sub,
                                 EM_dir = EM_dir,
                                 do_checks = TRUE,
                                 new_datafile_name = "init_dat.ss",
                                 verbose = verbose)
    }
    # given all checks are good, run the EM
    # check convergence (figure out way to error if need convergence)
    # get the future catch using the management strategy used in the SS model.
    new_catch_df <- run_EM(EM_dir = EM_dir, verbose = verbose,
                           check_converged = TRUE, nyrs_proj = nyrs_assess, 
                           change_fcast = TRUE)
  }
  if(MS == "last_yr_catch") {
    #TODO: extend this approach in the case of multiple fishery fleets.
    # get last year catch
    new_catch <- rep(OM_data$catch$catch[nrow(OM_data$catch)], 
                     length.out = nyrs_assess)
    new_catch_df <- data.frame(year = (OM_data$endyr+1):(OM_data$endyr+nyrs_assess), 
                               # assume always useing the same fleet and season for now
                               seas = OM_data$catch$seas[nrow(OM_data$catch)],
                               fleet = OM_data$catch$fleet[nrow(OM_data$catch)],
                               catch = new_catch,
                               catch_se = OM_data$catch$catch_se[nrow(OM_data$catch)])
    
  }
  if(MS == "no_catch") {
    new_catch_df <- data.frame(year = (OM_data$endyr+1):(OM_data$endyr+nyrs_assess), 
                               # assume always useing the same fleet and season for now
                               seas = OM_data$catch$seas[nrow(OM_data$catch)],
                               fleet = OM_data$catch$fleet[nrow(OM_data$catch)],
                               catch = 0,
                               catch_se = OM_data$catch$catch_se[nrow(OM_data$catch)])
    
  }
# check output before returning
check_catch_df(new_catch_df)
new_catch_df
}