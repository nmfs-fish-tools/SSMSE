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

#' Check the validity of a future OM list and standardize values
#' 
#' Checks that a future OM list is valid. If any values are implicit, 
#' then add these values
#' 
#' @param future_om_list The future_om_list object to check
check_future_om_list <- function(future_om_list) {
  # warning provided until future_om_list is used within SSMSE
  if(isTRUE(!is.null(future_om_list))) {
    warning("future_om_list is not yet being used by SSMSE.")
  }
  list_nums <- seq_along(future_om_list)
  # general strucure checks
  future_om_list_mod <- mapply(FUN = function(x, elem_num) {
    # check names are correct
    if(!all(names(x) == c("pars", "scen", "pattern", "input"))) {
      stop("Names of list elements not correct for future_om_list[[", elem_num,
           "]]. Please check that each", 
           "first level list components has 4 lists below it named (in order)",
           "pars, scen, pattern and input.")
    }
    # check structure within each list element is correct.
    assertive.properties::assert_is_atomic(x[["pars"]])
    assertive.types::assert_is_character(x[["pars"]])

    assertive.properties::assert_is_atomic(x[["scen"]])
    assertive.types::assert_is_character(x[["scen"]])
    x[["scen"]][1] <- match.arg(x[["scen"]][1], choices = c("randomize", "replicate"))

    assertive.properties::assert_is_atomic(x[["pattern"]])
    assertive.types::assert_is_character(x[["pattern"]])
    x[["pattern"]][1] <- match.arg(x[["pattern"]][1], 
                                   choices = c("model_change", "custom"))
    if(x[["pattern"]][1] == "model_change") {
      if(length(x[["pattern"]]) == 1) {
        x[["pattern"]] <- c(x[["pattern"]], "normal")
      }
      assertive.properties::assert_is_of_length(x[["pattern"]], 2)
      # TODO: specify more choices for pattern?
      x[["pattern"]][2] <- match.arg(x[["pattern"]][2],
                                     choices = c("normal", "lognormal"))
    }
    if(x[["pattern"]][1] == "custom") {
      assertive.types::assert_is_a_string(x[["pattern"]])
    }
    assertive.types::assert_is_data.frame(x[["input"]])
    if(x[["pattern"]][1] == "model_change") {
      expected_names <- c("first_yr_averaging", "last_yr_averaging",
        "last_yr_orig_val", "first_yr_final_val", "ts_param", "method", "value")
    }
    if(x[["pattern"]][1] == "custom") {
      expected_names <- c("parameter", "scenario", "iteration", "year", "value")
    }
    if(isTRUE(!all(names(x[["input"]]) == expected_names)) 
       | isTRUE(length(names(x[["input"]] != length(expected_names))))) {
      stop("Because pattern future_om_list[[", elem_num, "]] is model_change", 
           "names(future_om_list[['input']]) should be ", expected_names, ".", 
           "Current names are: ", 
           paste0(names(x[["input"]]), collapse = ", "))
    }
    invisible(x)
  }, future_om_list, list_nums, SIMPLIFY = FALSE)
  

  
  # more specific structure checks
  mapply(FUN = function(x, elem_num) {
    # TODO: add checks that pars are either rec_devs, impl_error, all, or 
    # valid names from the om model?
    # TODO: add checks that scenario names are valid or is "all"
    # TODO: add checks that the correct values are present in the dataframes.
    # for example, for custom, there need to be a certain number of inputs
    # based on the scenarios selected, number of iterations.
    
  }, future_om_list_mod, list_nums, SIMPLIFY = FALSE)

  invisible(future_om_list_mod)
}