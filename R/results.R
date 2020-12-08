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
#'  do not yet have scenario-level summary files and the suggested package
#'  parallel needs to be available for running in parallel to occur.
#' @param n_cores How many cores to use if running in parallel. If is NULL,
#'  defaults to n_cores available - 1 (also capped at one less than the number
#'  of cores available - 1)
#' @param overwrite Should existing summary files be overwritten if present
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
                              run_parallel = FALSE, n_cores = NULL,
                              overwrite = FALSE) {
  dir <- normalizePath(dir)
  if (is.null(scenarios)) {
    scenarios <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  }
  if (run_parallel) {
    exists_scen_files <- lapply(scenarios, function(scen, dir) {
      exists_file_list <- lapply(c("dq", "scalar", "ts"),
        function(file, dir, scen) {
          exists_file <- file.exists(
            file.path(dir, scen, paste0("results_", file, "_", scen, ".csv"))
          )
        },
        dir = dir, scen = scen
      )
      if (all(unlist(exists_file_list) == TRUE)) {
        return <- TRUE
      } else {
        return <- FALSE
      }
    }, dir = dir)
    if (all(unlist(exists_scen_files) == TRUE)) {
      warning(
        "run_parallel is TRUE, but will take much longer than running in",
        " serial because the scenario files already exist. Skipping ",
        "parallel processing. If you wish to create new scenario files",
        " please delete them and rerun SSMSE_summary_all."
      )
      use_parallel <- FALSE
    } else {
      use_parallel <- TRUE
    }
    if (!requireNamespace("parallel", quietly = TRUE)) {
      # this is unlikely to be used, as  parallel is installed with base R.
      warning(
        "run_parallel is TRUE, but the package parallel is not available.",
        " Running in serial."
      )
      use_parallel <- FALSE
    }
    if (use_parallel) {
      # setup and run parallel
      if (is.null(n_cores)) n_cores <- parallel::detectCores() - 1
      if (!is.null(n_cores)) {
        n_cores <- min(max(n_cores, 1), (parallel::detectCores() - 1))
      }
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl))
      result <- parallel::parLapply(
        cl = cl, X = scenarios,
        fun = get_results_scenario,
        directory = dir,
        overwrite_files = TRUE
      )
    }
  }
  ret <- ss3sim::get_results_all(
    directory = dir, user_scenarios = scenarios,
    overwrite_files = overwrite,
    filename_prefix = "SSMSE"
  )
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
  res <- ss3sim::get_results_scenario(
    scenario = basename(dir),
    directory = dirname(dir)
  )
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


#' Plot index data, expected values, and sampled data for 1 scenario
#'
#' To see how sampling lines up with data and expected values for the index of
#' abundance
#' @param dir Path to the directory containing 1 scenario
#' @export
#' @return A list containing 2 components: 1) the ggplot object and 2) the
#'  dataframe used to make the ggplot object
plot_index_sampling <- function(dir = getwd()) {
  # get the iterations
  iters <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  scenario <- basename(dir)
  # get the OM data values
  om_name <- grep("OM", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(om_name)
  tmp_dat_OM <- r4ss::SS_readdat(file.path(
    dir, as.character(iters[1]),
    om_name, "data.ss_new"
  ),
  verbose = FALSE, section = 1
  )
  tmp_dat_OM[["CPUE"]][["iteration"]] <- paste0(om_name, "_observed_values")
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- paste0(om_name, "_observed_values")
  index_dat <- tmp_dat_OM[["CPUE"]]
  # get the OM expected values
  tmp_dat_OM <- r4ss::SS_readdat(file.path(
    dir, as.character(iters[1]),
    om_name, "data.ss_new"
  ),
  verbose = FALSE, section = 2
  )
  tmp_dat_OM[["CPUE"]][["iteration"]] <- paste0(om_name, "_exp_values")
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- paste0(om_name, "_exp_values")
  index_dat <- rbind(index_dat, tmp_dat_OM[["CPUE"]])
  # get the EM init values
  # em name is the same across iterations
  em_name <- grep("EM_init$", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(em_name)
  for (i in iters) {
    tmp_dat_EM <- r4ss::SS_readdat(file.path(
      dir, as.character(i),
      em_name, "data.ss_new"
    ),
    verbose = FALSE, section = 1
    )
    tmp_dat_EM[["CPUE"]][["iteration"]] <- i
    tmp_dat_EM[["CPUE"]][["scenario"]] <- scenario
    tmp_dat_EM[["CPUE"]][["model_run"]] <- em_name
    index_dat <- rbind(index_dat, tmp_dat_EM[["CPUE"]])
  }
  # get rid of negative index values
  index_dat <- index_dat[index_dat[["index"]] > 0, ]

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    # this is unlikely to be used, as  parallel is installed with base R.
    warning(
      "The package parallel is not available.",
      " Returning early with just the dataframe and not the plot."
    )
    return(list(index_dat = index_dat, index_plot = NA))
  }
  index_plot <- ggplot2::ggplot(
    index_dat,
    ggplot2::aes(x = .data[["year"]], y = .data[["obs"]])
  ) +
    ggplot2::geom_line(ggplot2::aes(
      linetype = .data[["iteration"]],
      color = .data[["model_run"]]
    )) +
    ggplot2::scale_linetype_manual(
      values = rep("solid", length(unique(index_dat[["iteration"]])))
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["index"]])) + # by fleet
    ggplot2::guides(linetype = FALSE) +
    ggplot2::theme_classic()

  index_list <- list(index_dat = index_dat, index_plot = index_plot)
}
