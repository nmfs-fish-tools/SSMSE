#' Get results in a list for 1 scenario
#' 
#' Get results in a list for 1 iteration, using ss3sim::get_results_iter
#' 
#' @param dir Path to the directory containing the scenarios, either relative 
#'  or absolute. Defaults to the working directory.
#' @param scenarios A character vector of scenarios in dir from which to extract
#'  summaries. If left as NULL, the summaries will be extracted from all folders
#'  in dir.
#' @param run_parallel Option to use parallel processing. Defaults to FALSE.
#'  Note that running in parallel will only be faster if most of the scenarios
#'  do not yet have scenario-level summary files .
#' @param n_cores How many cores to use if running in parallel. If is NULL, 
#'  defaults to n_cores available - 1 (also capped at one less than the number
#'  of cores available - 1) 
#' @return A list of 3 data frames called scalar, ts, and
#'  dq (for derived quantities). These lists contain information for
#'  multiple model runs (estimation models and operating models) for 1
#'  iteration.Also writes 3 .csv files with the contents of this list of
#'  dataframes to dir and 3.csv files with scenario specific results in each of
#'  the scenario foldurs..
#' @seealso \code{\link[ss3sim]{get_results_all}}
#' @importFrom ss3sim get_results_all
#' @export
SSMSE_summary_all <- function(dir = getwd(), scenarios = NULL, 
                              run_parallel = FALSE, n_cores = NULL) {
  dir <- normalizePath(dir)
  if(is.null(scenarios)) {
    scenarios <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  }
  if (run_parallel) {
    exists_scen_files <- lapply(scenarios, function(scen, dir) {
      exists_file_list <- lapply(c("dq", "scalar", "ts"),
        function(file, dir, scen) {
        exists_file <- file.exists(
          file.path(dir, scen, paste0("results_", file, "_", scen, ".csv")))
      }, dir = dir, scen = scen)
      if(all(unlist(exists_file_list) == TRUE)) {
        return <- TRUE
      } else {
        return <- FALSE
      }
    }, dir = dir)
    if(all(unlist(exists_scen_files) == TRUE)) {
      warning("run_parallel is TRUE, but will take much longer than running in",
              " serial because the scenario files already exist. Skipping ", 
              "parallel processing. If you wish to create new scenario files",
              " please delete them and rerun SSMSE_summary_all.")
      use_parallel <- FALSE
    } else {
      use_parallel <- TRUE
    }
    if(use_parallel) {
      # setup and run parallel
      if (is.null(n_cores)) n_cores <- detectCores() - 1
      if (!is.null(n_cores)) n_cores <- min(max(n_cores, 1), (detectCores() - 1))
      cl <- parallel::makeCluster(n_cores)
      on.exit(stopCluster(cl))
      result <- parLapply(cl = cl, X = scenarios, 
                          fun = get_results_scenario, 
                          directory = dir, 
                          overwrite_files = TRUE)
    }
  }
  ret <- ss3sim::get_results_all(directory = dir, user_scenarios = scenarios, 
                                 overwrite_files = FALSE, 
                                 filename_prefix = "SSMSE")
  ret
}

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