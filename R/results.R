#' Get results in a list for 1 scenario
#'
#' Get results in a list for 1 iteration, using ss3sim::get_results_iter
#'
#' @param dir Path to the directory containing the scenarios, either relative
#'  or absolute. Defaults to the working directory.
#' @param scenarios A character vector of scenarios in dir from which to extract
#'  summaries. If left as NULL, the summaries will be extracted from all folders
#'  in dir.
#' @template parallel
#' @template overwrite
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
SSMSE_summary_iter <- function(dir) {
  res <- ss3sim::get_results_iter(dir_1_iter = dir)
}

#' Plot index data, expected values, and sampled data for 1 scenario
#'
#' Creates a plot that can be used to see how sampling lines up with
#' data and expected values for the index of abundance
#' @param dir Path to the directory containing 1 scenario. Defaults to
#'  the current working directory.
#' @export
#' @author Kathryn Doering
#' @return A list containing 2 components: 1) the ggplot object and 2) the
#'  dataframe used to make the ggplot object
plot_index_sampling <- function(dir = getwd()) {
  # get the iterations
  iters <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  scenario <- basename(dir)
  # get the OM data values, which are the same across iterations.
  om_name <- grep("OM", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(om_name)

  data_file <- list.files(file.path(dir,as.character(iters[1]), om_name), pattern = "data.ss_new|data_echo.ss_new")
  if (data_file == "data.ss_new") {
    tmp_dat_OM <- r4ss::SS_readdat(
      file.path(
        dir, as.character(iters[1]),
        om_name, data_file
      ),
      verbose = FALSE, section = 1
    )
  } else {
    tmp_dat_OM <- r4ss::SS_readdat(
      file.path(
        dir, as.character(iters[1]),
        om_name, data_file
      ),
      verbose = FALSE
    ) 
  }
  tmp_dat_OM[["CPUE"]][["iteration"]] <- as.character(iters[1])
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- "historical_values"
  index_dat <- tmp_dat_OM[["CPUE"]]
  # get the OM expected values
  if (data_file == "data.ss_new") {
    tmp_dat_OM <- r4ss::SS_readdat(
      file.path(
        dir, as.character(iters[1]),
        om_name, data_file
      ),
    verbose = FALSE, section = 2
    )
  } else {
    tmp_dat_OM <- r4ss::SS_readdat(
      file.path(
        dir, as.character(iters[1]),
        om_name, "data_expval.ss"
      ),
      verbose = FALSE
    )
  }
  tmp_dat_OM[["CPUE"]][["iteration"]] <- as.character(iters[1])
  tmp_dat_OM[["CPUE"]][["scenario"]] <- scenario
  tmp_dat_OM[["CPUE"]][["model_run"]] <- "OM_expected_values"
  index_dat <- rbind(index_dat, tmp_dat_OM[["CPUE"]])
  # get the EM init values
  # em name is the same across iterations
  em_name <- grep("EM_init$", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(em_name)
  for (i in iters) {
    if(data_file == "data.ss_new"){
      tmp_dat_EM <- r4ss::SS_readdat(
        file.path(
          dir, as.character(i),
          em_name, data_file
        ),
        verbose = FALSE, section = 1
      )
    } else {
      tmp_dat_EM <- r4ss::SS_readdat(
        file.path(
          dir, as.character(i),
          em_name, data_file
        ),
        verbose = FALSE
      )
    }
  
    tmp_dat_EM[["CPUE"]][["iteration"]] <- i
    tmp_dat_EM[["CPUE"]][["scenario"]] <- scenario
    tmp_dat_EM[["CPUE"]][["model_run"]] <- "sampled_dataset"
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
    ggplot2::guides(linetype = "none") +
    ggplot2::theme_classic()

  index_list <- list(index_dat = index_dat, index_plot = index_plot)
}

#' Plot comp data, expected values, and sampled data for 1 scenario
#'
#' Creates a plot that can be used to see how sampling lines up with
#' data and expected values for the index of abundance
#' @param dir Path to the directory containing 1 scenario. Defaults to
#'  the current working directory.
#' @param comp_type Type of composition data, age or length. Defaults to age.
#' @export
#' @import ggplot2
#' @importFrom magrittr %>%
#' @author Kathryn Doering
#' @return A list containing 2 components: 1) the ggplot object and 2) the
#'  dataframe used to make the ggplot object
plot_comp_sampling <- function(dir = getwd(), comp_type = c("agecomp", "lencomp")) {
  comp_type <- match.arg(as.character(comp_type), choices = c("agecomp", "lencomp"))
  # get the iterations
  iters <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  scenario <- basename(dir)
  # get the OM data values
  om_name <- grep("OM", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(om_name)
  # non-NULL compfile input provided and file exists
  out_OM <- r4ss::SS_output(
    file.path(
      dir, as.character(iters[1]),
      om_name
    ),
    verbose = FALSE,
    printstats = FALSE,
    hidewarn = TRUE
  )

  if (comp_type == "agecomp") comp_dbase <- out_OM[["agedbase"]]
  if (comp_type == "lencomp") comp_dbase <- out_OM[["lendbase"]]
  if (isTRUE(nrow(comp_dbase) == 0)) {
    stop(
      "The comp database from the operating model has no rows, so must not ",
      "have been any historical data in the OM."
    )
  }
  comp_dbase <- utils::type.convert(comp_dbase)
  comp_dbase[["iteration"]] <- 1
  comp_dbase[["scenario"]] <- scenario
  comp_dbase[["model_run"]] <- "om"
  # Get the EM data ----
  # get the EM init values
  # em name is the same across iterations
  em_name <- grep("EM_init$", list.dirs(file.path(dir, iters[1]),
    recursive = FALSE,
    full.names = FALSE
  ), value = TRUE)
  assertive.types::assert_is_a_string(em_name)
  for (i in iters) {
    tmp_out_EM <- r4ss::SS_output(
      file.path(
        dir, as.character(i),
        em_name
      ),
      verbose = FALSE,
      printstats = FALSE,
      hidewarn = TRUE
    )
    if (comp_type == "agecomp") tmp_comp_dbase <- tmp_out_EM[["agedbase"]]
    if (comp_type == "lencomp") tmp_comp_dbase <- tmp_out_EM[["lendbase"]]

    tmp_comp_dbase[["iteration"]] <- i
    tmp_comp_dbase[["scenario"]] <- scenario
    tmp_comp_dbase[["model_run"]] <- "em"
    comp_dbase <- rbind(comp_dbase, tmp_comp_dbase)
  }
  # get expected and observations in the same column
  comp_dbase <- tidyr::gather(comp_dbase, "type_obs", "obs_value", 17:18) %>%
    dplyr::filter(.data[["model_run"]] == "om" |
      (.data[["model_run"]] == "em" & .data[["type_obs"]] == "Obs"))
  comp_dbase[["model_type_obs"]] <- paste0(
    comp_dbase[["model_run"]], "_",
    comp_dbase[["type_obs"]]
  )
  comp_dbase <- tidyr::spread(comp_dbase, .data[["model_type_obs"]], .data[["obs_value"]]) %>%
    dplyr::mutate(Yr_lab = paste0("Yr: ", .data[["Yr"]])) %>%
    dplyr::mutate(Seas_lab = paste0("Seas: ", .data[["Seas"]])) %>%
    dplyr::mutate(Sex_lab = paste0("Sex: ", .data[["Sex"]]))
  # Make the plot ----
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning(
      "The package ggplot2 is not available.",
      " Returning early with just the dataframe and not the plot."
    )
    return(list(comp_dat = comp_dbase, plot = NA))
  }
  if (comp_type == "agecomp") xlab_val <- "Age"
  if (comp_type == "lencomp") xlab_val <- "Size Bins"
  # need a loop for multiple fleets (I think we just want a plot per fleet)
  # Need to add in better labels for sex
  comp_plot <- vector(mode = "list", length = length(unique(comp_dbase[["Fleet"]])))
  for (f in unique(comp_dbase[["Fleet"]])) {
    ind <- which(unique(comp_dbase[["Fleet"]]) == f)
    tmp_dbase_subset <- comp_dbase[comp_dbase[["Fleet"]] == f, ]
    comp_plot[[ind]] <- ggplot(tmp_dbase_subset, aes(x = .data[["Bin"]], y = .data[["om_Exp"]])) +
      geom_area(fill = "grey") +
      geom_point(aes(y = .data[["om_Obs"]]), color = "red", size = 2) +
      geom_point(aes(y = .data[["em_Obs"]], shape = .data[["iteration"]]), color = "black") +
      facet_wrap(vars(.data[["Yr_lab"]], .data[["Seas_lab"]], .data[["Sex_lab"]])) +
      scale_shape_manual(
        values =
          rep(15, length(unique(comp_dbase[["iteration"]])))
      ) +
      ylab("Proportion") +
      xlab(xlab_val) +
      ggtitle(paste0("Fleet ", f)) +
      theme_classic()
  }

  comp_list <- list(comp_dat = comp_dbase, plot = comp_plot)
}

#' get basic data to calculate performance metrics
#' @param dir Path to the directory containing the scenarios, either relative
#'  or absolute. Defaults to the working directory.
#' @param use_SSMSE_summary_all If it exists, should the summmary files generated
#'  by SSMSE_summary_all be used? Defaults to TRUE.
#' @param quantities Quantites from the operating model to add
get_performance_metrics <- function(dir = getwd(),
                                    use_SSMSE_summary_all = TRUE,
                                    quantities = c("catch", "SpawnBio")) {
  quantities <- match.arg(quantities,
    choices = c("catch", "SpawnBio"),
    several.ok = TRUE
  )
  perf_metrics_df <- NULL
  if ("catch" %in% quantities) {
    scens <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
    catch <- lapply(scens, function(x) {
      iters <- list.dirs(x, full.names = TRUE, recursive = FALSE)
      tmp_catch_df <- NULL
      for (i in iters) {
        tmp_mods <- list.dirs(i, full.names = TRUE, recursive = FALSE)
        tmp_mods_basename <- list.dirs(i, full.names = FALSE, recursive = FALSE)
        om_mod <- grep("OM$", tmp_mods_basename, ignore.case = TRUE)
        if (length(om_mod) != 1) {
          stop(
            "The regular expression 'OM$' (not case sensitive) did not match 1 model in the ",
            "directory ", i, "; it matched ", length(om_mod), " models.",
            "Please make sure only the OM will match this expression to use",
            "get_performance_metrics."
          )
        }
        om_mod_path <- tmp_mods[om_mod]
        om_mod_dat <- list.files(om_mod_path, pattern = "data.ss_new|data_echo.ss_new")
        if(om_mod_dat == "data.ss_new"){
          dat <- r4ss::SS_readdat(file.path(om_mod_path, om_mod_dat),
            section = 1, verbose = FALSE
        )
        } else {
          dat <- r4ss::SS_readdat(file.path(om_mod_path, om_mod_dat),
           verbose = FALSE
        )
        }
        tmp_catch <- dat[["catch"]]
        tmp_catch <- tmp_catch[, c("year", "fleet", "catch")]
        colnames(tmp_catch) <- c("year", "fleet", "value")
        tmp_catch[["quantity"]] <- "catch"
        tmp_catch[["model_run"]] <- tmp_mods_basename[om_mod]
        tmp_catch[["model_type"]] <- "OM"
        tmp_catch[["iteration"]] <- basename(i)
        tmp_catch_df <- rbind(tmp_catch_df, tmp_catch)
      }
      tmp_catch_df[["scenario"]] <- basename(x)
      tmp_catch_df
    })
    catch_df <- do.call("rbind", catch)
    perf_metrics_df <- rbind(perf_metrics_df, catch_df)
  }
  if ("SpawnBio" %in% quantities) {
    if (use_SSMSE_summary_all == TRUE) {
      ts_df <- utils::read.csv(file.path(dir, "SSMSE_ts.csv"))
      keep_rows <- grep("OM$", ts_df[["model_run"]], ignore.case = TRUE)
      ts_df <- ts_df[keep_rows, ]
      ts_df[["fleet"]] <- NA
      ts_df[["quantity"]] <- "SpawnBio"
      ts_df[["model_type"]] <- "OM"
      ts_df <- ts_df[, c(
        "year", "fleet", "SpawnBio", "quantity", "model_run",
        "model_type", "iteration", "scenario"
      )]
      colnames(ts_df) <- c(
        "year", "fleet", "value", "quantity", "model_run",
        "model_type", "iteration", "scenario"
      )
      perf_metrics_df <- rbind(perf_metrics_df, ts_df)
    } else {
      warning("use_SSMSE_summary_all needs to be TRUE to read in SSB")
    }
  }
  perf_metrics_df
}
