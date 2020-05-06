
#' Get results in a list for 1 scenario
#' 
#' Get results in a list for 1 iteration, using ss3sim::get_results_iter
#' 
#' @param dir Path to the directory for 1 scenario, either relative or absolute.
#'  Defaults to the working directory.
#' @return A list of 3 data frames called scalar, ts, and
#'  dq (for derived quantities). These lists contain information for
#'  multiple model runs (estimation models and operating models) for 1
#'  iteration.Also writes 3 .csv files with the contents of this list of
#'  dataframes to dir.
#' @seealso \code{\link[ss3sim]{get_results_scenario}}
#' @importFrom ss3sim get_results_scenario
#' @export
SSMSE_summary_scen <- function(dir = getwd()) {
  res <- ss3sim::get_results_scenario(scenario = basename(dir), 
                                      directory = dirname(dir))
}

#' Get results in a list for 1 iteration
#' 
#' Get results in a list for 1 iteration, using ss3sim::get_results_iter
#' 
#' @param dir Path to the directory for 1 iteration of 1 scenario.
#' @return A list of 3 data frames called scalar, timeseries, and
#'  derived (for derived quantities). These lists contain information for
#'  multiple model runs (estimation models and operating models) for 1
#'  iteration.
#' @seealso \code{\link[ss3sim]{get_results_iter}}
#' @importFrom ss3sim get_results_iter
#' @export
SSMSE_summary_iter <- function(dir) {
  res <- ss3sim::get_results_iter(dir_1_iter = dir)
}