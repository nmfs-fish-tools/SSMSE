# Parse management strategies.

#' Parse management strategy options
#'
#' This function matches each management strategy with its correct method. And
#' checks for errors.
#' @template MS
#' @template EM_out_dir
#' @param EM_init_dir Initialization director that retains the reference files for interim assessments
#' @param init_loop Logical. If this is the first initialization loop of the
#'   MSE, \code{init_loop} should be TRUE. If it is in further loops, it should
#'   be FALSE.
#' @param OM_dat An valid SS3 data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @template OM_out_dir
#' @template verbose
#' @param nyrs_assess The number of years between assessments (e.g., if an
#'   assessment is conducted every 3 years, put 3 here). A single integer value.
#' @param dat_yrs Which years should be added to the new model? Ignored if
#'  init_loop is TRUE.
#' @template future_om_list
#' @template sample_struct
#' @param interim_struct An optional including how many years to average over,
#'  fleet weights, the scaling rate (Beta) of catch relative to the index change for each fleet,
#'  and the reference year for each fleet (either a fixed year or <=0 relative to end_yr, fixed year
#'  will stay constant during simulation while relative year will progress with simulation).
#' @template seed
#' @author Kathryn Doering & Nathan Vaughan
#' @export
#' @importFrom r4ss SS_readstarter SS_writestarter SS_writedat

parse_MS <- function(MS, EM_out_dir = NULL, EM_init_dir = NULL,
                     init_loop = TRUE, OM_dat, OM_out_dir = NULL,
                     verbose = FALSE, nyrs_assess, dat_yrs, future_om_list = NULL,
                     sample_struct = NULL, interim_struct = NULL, seed = NULL) {
  if (verbose) {
    message("Parsing the management strategy.")
  }
  # input checks ----
  if (!exists(MS)) {
    stop(
      "Invalid management strategy ", MS, ". Please check your function",
      " name and make sure it is available in the global ",
      "environment, if not built into SSMSE."
    )
  }

  if (!is.null(EM_out_dir)) check_dir(EM_out_dir) # make sure contains a valid model
  if (is.null(seed)) {
    seed <- stats::runif(1, 1, 9999999)
  }
  # parsing management strategies ----
  # note: only the below list of parameters can currently be passed to the
  # management strategy
  pars_list <- list(
    EM_out_dir = EM_out_dir,
    EM_init_dir = EM_init_dir,
    init_loop = init_loop,
    OM_dat = OM_dat,
    OM_out_dir = OM_out_dir,
    verbose = verbose,
    nyrs_assess = nyrs_assess,
    dat_yrs = dat_yrs,
    sample_struct = sample_struct,
    interim_struct = interim_struct,
    seed = seed
  )
  new_catch_list <- do.call(MS, args = pars_list)
  # to do: need better checks on function name? Maybe be more explicit on
  # which environment the function is in?
  # check output before returning
  check_catch_df(new_catch_list[["catch"]])
  if (isTRUE(!is.null(new_catch_list[["discards"]]))) {
    warning("Discards are not added into the OM for SSMSE currently.")
  }
  new_catch_list
}
