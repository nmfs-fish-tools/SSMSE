#' Example Performance Metric: Calculate total catch over a range of years
#'
#' @param datfile Path to the Stock Synthesis data file containing catch
#' @param yrs A vector containing a range of years.
#' @return The total catch, a number.
#' @export
#' @examples \dontrun{
#'   total_catch <- get_total_catch(datfile = "ss3model/dat.ss", yrs = 25:100)
#'   total_catch
#' }
get_total_catch<- function(datfile, yrs) {
   dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
   catch_sum <- sum(dat$catch[dat$catch$year %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate average catch over a range of years
#' This is an example performance metric that will
#'  calculate average catch over a range of years.
#' Note that all fleets will be grouped together.
#' @inheritParams get_total_catch
#' @return The average catch, a number.
#' @export
#' @example \dontrun{
#' avg_catch <- function(datfile = "ss3model/dat.ss", yrs = 30:75)
#' avg_catch
#' }
get_avg_catch<- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_avg <- mean(dat$catch[dat$catch$year %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate Standard Deviation of Catch
#' 
#' Calculates the standard deviation of the catch across a range of years
#' Does not take into account multiple fleets
#' @inheritParams get_total_catch
#' @return The catch standard deviation, a number.
#' @export
#' @example \dontrun{
#'   catch_sd <- get_catch_sd(datfile = "mod/dat.ss", yrs = c(20:50, 75:100))
#'   catch_sd
#' }
get_catch_sd <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_var <- sd(dat$catch[dat$catch$year %in% yrs, "catch"])
}

#' Example Performance Metric: Calculate the coefficient of variation of catch
#' 
#' Calculates the coefficient of variation of catch over a range of years. Note that
#' this does not account for multiple fleets.
#' @inheritParams get_total_catch
#' @return The catch coefficient of variation, a number.
#' @export
get_catch_cv <- function(datfile, yrs) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch <- dat$catch[dat$catch$year %in% yrs, "catch"]
  catch_var <- sd(catch)/mean(catch)
}

#' Example Performance Metric: calculate the average SSB over a range of years
#' 
#' Calculates the average SSB for each iteration of each scenario in 
#' a run of SSMSE
#'  
#' @param summary Summary returned from running `run_SSMSE()`
#' @param min_year The first year to include in the average
#' @param max_year The last year to include in the average
#' @export
#' @return A tibble containing the Average SSB by iteration and scenario.
get_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  SSB_yr <- summary$ts %>% 
          filter(year >= min_yr & year <= max_yr) %>% 
          filter(model_run %in% OM_vals) %>% 
          select(iteration, scenario, year, SpawnBio) %>% 
          group_by(iteration, scenario) %>% 
          summarize(avg_SSB = mean(SpawnBio), .groups = "keep") %>% 
          ungroup()
  SSB_yr
}

#' Example Performance Metric: calculate the avg relative SSB (SSB/SSB unfished) over a range of years
#'
#' @inheritParams get_SSB_avg
#' @export
#' @return A tibble containing the avg r 
get_rel_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  B_unfished <- summary$scalar %>% 
                 filter(model_run %in% OM_vals) %>% 
                 select(iteration, scenario,SSB_Unfished)
  SSB_yr <- summary$ts %>% 
              filter(year >= min_yr & year <= max_yr) %>%
              select(iteration, scenario, year, SpawnBio)
  SSB_yr <- left_join(SSB_yr, B_unfished) %>%
              mutate(Rel_SSB = SpawnBio/SSB_Unfished) %>%
              group_by(iteration, scenario) %>%
              summarize(avg_SSB = mean(Rel_SSB), .groups = "keep") %>%
              ungroup()
  SSB_yr
}


# need to make general
check_convergence <- function(summary, min_yr = 101, max_yr = 120, n_EMs = 5) {
  require(dplyr) # note: not the best way to do this
  if(any(!is.na(summary$scalar$params_on_bound))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary$ts$model_type <- ifelse(grepl("_EM_", summary$ts$model_run), "EM", "OM")
  calc_SSB <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>% 
    select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>% 
              filter(model_type == "OM") %>% 
              rename(SpawnBio_OM = SpawnBio ) %>% 
              select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>% 
               filter(model_type == "EM") %>% 
               rename(SpawnBio_EM = SpawnBio) %>% 
               select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>% 
                  mutate(SSB_ratio = SpawnBio_EM/SpawnBio_OM)
  filter_SSB <- bind_vals %>% 
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if(nrow(filter_SSB) > 0 ) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}