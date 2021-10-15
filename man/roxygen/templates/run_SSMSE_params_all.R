# Template for parameters common to run_SSMSE, run_SSMSE_scen, and run_SSMSE_iter
#' @param custom_MS_source A file location with the source code for any custom MS
#'   models to be used in the simulation. SSMSE will source this file which should
#'   contain a function/s whose name/s match each custom MS models included in MS_vec.
#' @param run_EM_last_yr Should the MS be implemented to get future catch if the
#'  laste year is an assessment year? TRUE/FALSE option, so the same for all 
#'  scenarios and iterations. Defaults to FALSE.
