# Functions associated with initialization of the OM.

#' Create the OM from a stock assessment model
#' 
#' This function copies over the stock assessment model and manipulates it as
#' needed so that it can be used as an operating model.
#' @author Kathryn Doering
#' @param OM_dir The full path to the OM directory
#' @param SA_dir The full path to the EM directory
#' @param overwrite Overwrite existing files with matching names?
#' @param add_dummy_dat Add dummy data to indices and comps for each year so 
#'   that expected values and sampling is obtained for years other than those 
#'   that already have data? Defaults to FALSE.
#' @template verbose
#' @return OM_dir, because this function is used mainly for its side effects.
#' @importFrom SSutils copy_SS_inputs 
#' @importFrom r4ss SS_readdat SS_writedat
create_OM <- function(OM_dir, 
                      SA_dir,
                      overwrite     = FALSE,
                      add_dummy_dat = FALSE,
                      verbose       = FALSE) {
  if(add_dummy_dat) {
    stop("Dummy data for indices and comps for all years cannot yet be added ",
         "to the operating model.")
    # TODO: develop this option. This will be necessary if we want to allow for 
    # use with EMs that have different years of sampled data than the OM.
  }
  # copy over SA model to OM.
  if(verbose) message("Copying over model in ", SA_dir, " to ", OM_dir, ".")
  SSutils::copy_SS_inputs(dir.old = SA_dir, 
                 dir.new = OM_dir,
                 overwrite = overwrite,
                 use_ss_new = TRUE, # will rename the ss new files, also.
                 copy_par = TRUE,
                 verbose = verbose)
  # validate using model as an OM? may want to make a seperate function that can
  # validate if the model can be used as an OM or not.
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
#' @param init_run Is this the initial iteration of the OM? Defaults to FALSE.
#' @template verbose
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
                                verbose = verbose)
  start$N_bootstraps <- max_section
  r4ss::SS_writestarter(start, dir = OM_dir, verbose = verbose, overwrite = TRUE)
  }
  # run SS and get the data set
  run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess")

  dat <- r4ss::SS_readdat(file.path(OM_dir,"data.ss_new"), 
                          section = max_section, 
                          verbose = verbose)
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
