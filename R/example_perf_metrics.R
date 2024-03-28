#' Example Performance Metric: Calculate total catch over a range of years
#'
#' Example performance metric that calculates total catch over a range of years
#'  in a Stock Synthesis data file. This function aggregates catch across
#'  fleets, so may not be appropriate for models with multiple fleets.
#' @param datfile Path to the Stock Synthesis data file containing catch
#' @param yrs A vector containing a range of years. Years are as defined in the
#'  Stock Synthesis data file.
#' @returns The total catch, a number.
#' @export
#' @examples \dontrun{
#' total_catch <- get_total_catch(datfile = "ss3model/dat.ss", yrs = 25:100)
#' total_catch
#' }
get_total_catch <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_sum <- sum(dat[["catch"]][dat[["catch"]][["year"]] %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate average catch over a range of years
#'
#' Example performance metric that calculates average catch over a range of
#'  years in a Stock Synthesis data file. This function aggregates across
#'  fleets, so may not be appropriate for models with multiple fleets.
#' @inheritParams get_total_catch
#' @returns The average catch, a number.
#' @export
#' @examples
#' \dontrun{
#' avg_catch <- function(datfile = "ss3model/dat.ss", yrs = 30:75) {
#'   avg_catch
#' }
#' }
get_avg_catch <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_avg <- mean(dat[["catch"]][dat[["catch"]][["year"]] %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate Standard Deviation of Catch
#'
#' Example performance metric that calculates the standard deviation of catch
#'  over a range of years in a Stock Synthesis data file. This function
#'  aggregates across fleets, so may not be appropriate for models with multiple
#'  fleets.
#' @inheritParams get_total_catch
#' @returns The catch standard deviation, a number.
#' @export
#' @examples
#' \dontrun{
#' catch_sd <- get_catch_sd(datfile = "mod/dat.ss", yrs = c(20:50, 75:100))
#' catch_sd
#' }
get_catch_sd <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_var <- stats::sd(dat[["catch"]][dat[["catch"]][["year"]] %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate the coefficient of variation of catch
#'
#' Example performance metric that calculates the coefficient of variation (CV)
#'  of catch over a range of years in a Stock Synthesis data file. This function
#'  aggregates across fleets, so may not be appropriate for models with multiple
#'  fleets.
#' @inheritParams get_total_catch
#' @returns The catch coefficient of variation, a number.
#' @export
#' @examples
#' \dontrun{
#' catch_cv <- get_catch_cv(datfile = "mod/dat.ss", yrs = c(20:50, 75:100))
#' catch_cv
#' }
get_catch_cv <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch <- dat[["catch"]][dat[["catch"]][["year"]] %in% yrs, "catch"]
  catch_var <- stats::sd(catch) / mean(catch)
}

#' Example Performance Metric: calculate the average SSB over a range of years
#'  for each iteration
#'
#' Example performance metric that calculates the average Spawning Stock Biomass
#'  SSB (units as in the simulations) over a range of years for each iteration of
#'  each scenario in the SSMSE simulation run.
#' @param summary Summary returned from running `SSMSE_summary_all()`
#' @param min_yr The first year to include in the average
#' @param max_yr The last year to include in the average
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' avg_ssb <- get_SSB_avg(run_SSMSE_summary, min_yr = 10, max_yr = 105)
#' avg_ssb
#' }
#' @returns A tibble containing the average SSB by iteration and scenario.
get_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary[["ts"]][["model_run"]])
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  SSB_yr <- summary[["ts"]] %>%
    dplyr::filter(year >= min_yr & year <= max_yr) %>%
    dplyr::filter(model_run %in% OM_vals) %>%
    dplyr::select(iteration, scenario, year, SpawnBio) %>%
    dplyr::group_by(iteration, scenario) %>%
    dplyr::summarize(avg_SSB = mean(SpawnBio), .groups = "keep") %>%
    dplyr::ungroup()
  SSB_yr
}

#' Example Performance Metric: Calculate the avg relative SSB (SSB/SSB unfished)
#' over a range of years for each iteration
#'
#' Example performance metric that calculates the average Spawning Stock Biomass
#'  SSB (units as in the simulations) relative to the  unfished SSB over a range
#'  of years for each iteration of each scenario in the SSMSE simulation run.
#' @inheritParams get_SSB_avg
#' @export
#' @examples
#' \dontrun{
#' rel_avg_ssb <- get_rel_SSB_avg(run_SSMSE_summary, min_yr = 10, max_yr = 105)
#' rel_avg_ssb
#' }
#' @importFrom magrittr %>%
#' @returns A tibble containing the relative avg SSB per year by iteration and
#'  scenario.
get_rel_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary[["ts"]][["model_run"]])
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  B_unfished <- summary[["scalar"]] %>%
    dplyr::filter(model_run %in% OM_vals) %>%
    dplyr::select(iteration, scenario, SSB_Unfished)
  SSB_yr <- summary[["ts"]] %>%
    dplyr::filter(year >= min_yr & year <= max_yr) %>%
    dplyr::select(iteration, scenario, year, SpawnBio)
  SSB_yr <- dplyr::left_join(SSB_yr, B_unfished) %>%
    dplyr::mutate(Rel_SSB = SpawnBio / SSB_Unfished) %>%
    dplyr::group_by(iteration, scenario) %>%
    dplyr::summarize(avg_SSB = mean(Rel_SSB), .groups = "keep") %>%
    dplyr::ungroup()
  SSB_yr
}


#' Flag potential convergence issues in SS3 model runs
#'
#' Does basic checks for convergance of estimation model runs from `run_SSMSE()`
#'  simulations. This function 1) warns if there are parameters on bounds; 2)
#'  warns if the SSB in the EM is 2x as large or half as small as the OM. Note
#'  these warnings may not mean that the models have not converged, but can flag
#'  potential issues that can be investigated further
#' @param summary Summary returned from running `SSMSE_summary_all()`
#' @param min_yr The first year of SSB checked
#' @param max_yr The last year of SSB checked
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' check_convergance(SSMSE_summary, min_yr = 101, max_yr = 120)
#' }
#' @returns A tibble containing the SSB values in the EM relative to the OM by
#'  model run of each iteration of each scenario.
check_convergence <- function(summary, min_yr, max_yr) {
  if (any(!is.na(summary[["scalar"]][["params_on_bound"]]))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary[["ts"]][["model_type"]] <- ifelse(grepl("_EM_", summary[["ts"]][["model_run"]]), "EM", "OM")
  calc_SSB <- summary[["ts"]] %>%
    dplyr::filter(year >= min_yr & year <= max_yr) %>%
    dplyr::select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>%
    dplyr::filter(model_type == "OM") %>%
    dplyr::rename(SpawnBio_OM = SpawnBio) %>%
    dplyr::select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>%
    dplyr::filter(model_type == "EM") %>%
    dplyr::rename(SpawnBio_EM = SpawnBio) %>%
    dplyr::select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- dplyr::full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>%
    dplyr::mutate(SSB_ratio = SpawnBio_EM / SpawnBio_OM)
  filter_SSB <- bind_vals %>%
    dplyr::filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if (nrow(filter_SSB) > 0) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}
