#' Helper function to create future om list objects
#' 
#' The future_om_list objects specify changes to make in the future to the
#' OM as the OM is extended forward in time. In particular, this function helps 
#' users create these objects. For now, just returns examples based on cod 
#' model that comes with SSMSE.
#' 
#' @param example_type Type of example future_om_list object to create. Options are
#' "model_change" or "custom". Defaults to "model_change".
#' @param list_length The length of the example list to create. Defaults to 1.
#' For now, just replicates the same list.
#' @example example_future_om_list <- create_om_list(example_type = "custom",
#'                                                   list_length = 2)
create_future_om_list <- function(example_type = c("model_change", "custom"),
                                  list_length = 1) {
  example_type <- match.arg(example_type,
                            choices = c("model_change", "custom"),
                            several.ok = FALSE)
  
  future_om_list <- vector(mode = "list", length = 1)
  future_om_list <- lapply(future_om_list, 
                           function (x) x <- vector(mode = "list", length = 4))
  names(future_om_list[[1]]) <- c("pars", "scen", "pattern", "input")
  
  if(example_type == "model_change") {
    # add values for selectivity curve param. step change occuring in year 103
    future_om_list[[1]][["pars"]] <- "SizeSel_P_3_Fishery(1)" # had to figure this out from reading in the par file.
    future_om_list[[1]][["scen"]] <- c("replicate", "scen2")
    future_om_list[[1]][["pattern"]] <- "model_change" # defaults to normal (with SD 0, mean at last yr of mod val?)
    future_om_list[[1]][["input"]] <- data.frame(first_yr_averaging = NA, # NA b/c not using historical values
                                                 last_yr_averaging = NA, # NA b/c not using historical values
                                                 last_yr_orig_val = 102,
                                                 first_yr_final_val = 103, 
                                                 ts_param = "mean",
                                                 method = "absolute", 
                                                 value = 4.5)
  }
  if(example_type == "custom") {
    future_om_list[[1]][["pars"]] <- "impl_error" 
    future_om_list[[1]][["scen"]] <- c("randomize", "all")
    future_om_list[[1]][["pattern"]] <- "custom"
    future_om_list[[1]][["input"]] <- data.frame(
      parameter = "impl_error",
      scenario = rep(c("scen1","scen2", "scen3"), times = rep(6*5, times = 3)), 
      iteration = rep(1:5, times = 3*6),
      year = rep(rep(101:106, times = rep(5, times = 6)), times = 3),
      value = c(rep(1.05, times = 6*5), rep(1.10, times = 6*5*2)))
  }
  future_om_list <- rep(future_om_list, times =  list_length)
}

#' Check the validity of a future OM list
#' 
#' @param future_om_list The future_om_list object to check
check_future_om_list <- function(future_om_list) {
  warning("Checks not yet added")
  invisible(future_om_list)
}