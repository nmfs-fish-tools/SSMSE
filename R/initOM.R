# Functions associated with initialization of the OM.

#' create the OM from a stock assessment model
#' 
#' This function copies over the stock assessment model and manipulates it as
#' needed so that it can be used as an operating model.
#' @author Kathryn Doering
#' @param OM_dir The full path to the OM directory
#' @param SA_dir The full path to the EM directory
#' @param overwrite Overwrite existing files with matching names?
#' @param add_dummy_dat Add dummy data to  indices and comps for each year so that
#'   expected values and sampling is obtained for years other than those that 
#'   already  have data? Defaults to FALSE.
#' @param verbose Want verbose output? Defaults to FALSE.
#' @return OM_dir, because this function is used mainly for its side effects.
#' @importFrom SSutils copy_SS_inputs 
#' @importFrom r4ss SS_readdat SS_writedat
create_OM <-  function(OM_dir, SA_dir,overwrite = FALSE, add_dummy_dat = FALSE,
                       verbose = FALSE) {
  if(add_dummy_dat) {
    # TODO: develop this option. This will be necessary if we want to allow for 
    # use with EMs that have different years of sampled data than the OM.
    stop("Dummy data for indices and comps for all years cannot yet be added ",
         "to the operating model.")
  }
  # copy over SA model to OM.
  if(verbose) message("Copying over the model in ", SA_dir, " to ", OM_dir, ".")
  SSutils::copy_SS_inputs(dir.old = SA_dir, 
                 dir.new = OM_dir,
                 overwrite = overwrite,
                 use_ss_new = TRUE, # will rename the ss new files, also.
                 copy_par = TRUE,
                 verbose = verbose)
  # read in files needed to manipulate.
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"), 
                                verbose = verbose)
  # validate using model as an OM? may want to make a seperate function that can
  # validate if the model can be used as an OM or not.
    
  # manipulate to make an OM (set maxphase = 0, anything else?)
  start$last_estimation_phase <- 0
  r4ss::SS_writestarter(start, 
                        dir = OM_dir, 
                        overwrite = overwrite, 
                        verbose = verbose)
  # This function is used mainly for its side effects, so just return the first
  # argument invisibily.
  invisible(OM_dir)
}

#' Initial run of the OM
#' 
#' This function is used to initialize the OM and get either expected values 
#' or bootstrap.
#' @author Kathryn Doering
#' @param OM_dir The full path to the OM directory
#' @param boot Return the bootstrap dataset? If TRUE, function returns the 
#'   number bootstrapped dataset specified in \code{nboot}. If FALSE, it returns
#'   the expected values.
#' @param nboot The number bootstrapped data set. This value is only used if 
#'   \code{boot = TRUE}. Note that this numbering does NOT correspond with the
#'   numbering in section of r4ss::SS_readdat. E.g., specifying section = 3 in 
#'   SS_readdat is equivalent to specifying nboot = 1.
#' @param verbose Want verbose output? Defaults to FALSE.
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
run_init_OM <- function(OM_dir, boot = TRUE, nboot = 1, verbose = FALSE) {
  # make sure OM generates the correct number of data sets.
  if (boot) {
    max_section <- nboot + 2
  } else {
    max_section <- 2
  }
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"), 
                                verbose = FALSE)
  start$N_bootstraps <- max_section
  r4ss::SS_writestarter(start, dir = OM_dir, verbose = FALSE, overwrite = TRUE)
  # get exe location and run the model
  # TODO: eventually, may want to wrap around the ss3sim functions in runSS so
  # this is done in a platform independent way.
  # run_ss3model(dir = OM_dir, type = "om")
  bin <- get_bin()
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(OM_dir)
  if(verbose) message("Running OM.")
  system(bin, invisible = TRUE, ignore.stdout = FALSE, 
         show.output.on.console = FALSE)
  # stop on error if OM did not run.
  if(!file.exists(file.path(OM_dir, "data.ss_new"))) {
    stop("OM did not run correctly, as a data.ss_new file was not created.", 
         "Please check that the OM in ", OM_dir, " is valid.")
  } else {
    if(verbose) message("OM ran in dir ", getwd())
  }
  # return the desired data set (expected values or bootstrap)

  dat <- r4ss::SS_readdat("data.ss_new", 
                          section = max_section, 
                          verbose = verbose)
  return(dat)
}

#' Get the sampling scheme in a data file.
#' 
#' Determine what the default sampling scheme is for a given data file.
#' Produces a list object with the sampling scheme, which can be modified, if
#' desired.
#' 
# get the initial sampling values 
get_init_samp_scheme <- function(dat, 
                                 dat_types = c("CPUE","lencomp", "agecomp")) {
  #TODO: write this. Can be used for EM and OM.
}
