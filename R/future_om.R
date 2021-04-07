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
#' @export
#' @examples
#' example_future_om_list <- 
#'   create_future_om_list(example_type = "custom", list_length = 2)
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
      par = "impl_error",
      scen = rep(c("scen1","scen2", "scen3"), times = rep(6*5, times = 3)), 
      iter = rep(1:5, times = 3*6),
      yr = rep(rep(101:106, times = rep(5, times = 6)), times = 3),
      value = c(rep(1.05, times = 6*5), rep(1.10, times = 6*5*2)))
  }
  future_om_list <- rep(future_om_list, times =  list_length)
}

#' Check the general structure of a future OM list and standardize values
#' 
#' Checks that a future OM list is valid. If any values are implicit, 
#' then add these values. Does not check against arguments in the scenario,
#' just the generic structure
#' @param future_om_list The future_om_list object to check
#' @return The future_om_list with implicit arguments made explicit
check_future_om_list_str <- function(future_om_list) {
  # warning provided until future_om_list is used within SSMSE
  # if(isTRUE(!is.null(future_om_list))) {
  #   warning("future_om_list is not yet being used by SSMSE.")
  # }
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
      expected_names <- c("par", "scen", "iter", "yr", "value")
    }
    if(isTRUE(!all(names(x[["input"]]) == expected_names)) 
       | isTRUE(length(names(x[["input"]] != length(expected_names))))) {
      stop("Because pattern future_om_list[[", elem_num, "]] is model_change, ", 
           "names(future_om_list[[", elem_num, "]][['input']]) should be \n",
           paste0(expected_names, collapse = ", "), ".\n", 
           "Current names are: ", 
           paste0(names(x[["input"]]), collapse = ", "))
    }
    invisible(x)
  }, future_om_list, list_nums, SIMPLIFY = FALSE)

  invisible(future_om_list_mod)
}

#' Check structure of a future OM list against the scen_list and standardize output
#' 
#' Checks that a future OM list is valid when compared with the scen_list inputs
#' @param future_om_list The future_om_list object to check.
#' @param scen_list The list object of scenarios specifying inputs, typically 
#'  passed to or created by run_SSMSE.
#' @return The future_om_list with implicit arguments made explicit
check_future_om_list_vals <- function(future_om_list, scen_list) {
  list_nums <- seq_along(future_om_list)
# more specific structure checks
 future_om_list <-  mapply(FUN = function(x, elem_num, scen_list) {
    # TODO: add checks that pars are either rec_devs, impl_error, all, or 
    # valid names from the om model?
    # This is a tricky check, because the names may differ based on the scenario
    # Perhaps will just need to do this check later, on the fly?
    # checks that scenario names are valid or is "all"
    len_scen_vec <- length(x[["scen"]])
    tmp_scen <- x[["scen"]][-1]
    if(isTRUE(tmp_scen[1] != "all")) {
      if(isTRUE(!all(tmp_scen %in% names(scen_list)))) {
        stop("Scenario names in future_om_list[[", elem_num, "]] do not match ", 
             "the scenario names in scen_list.\nfuture_om_list[[", elem_num, "]]", 
             "scenario names: ", paste0(tmp_scen, collapse = ", "), "\nscen_list", 
             "names: ", paste0(names(scen_list), collapse = ", "))
      }
    }
    #  Can't think of a necessary check for when pattern = "model_change"
    if (x[["pattern"]][1] == "custom") {
      # number of vals will depend on the number of scenarios and number of
      # iterations for each scenario, 
      if(isTRUE(tmp_scen == "all")) tmp_scen <- names(scen_list)
      n_iter_vec <- unlist(lapply(scen_list, function(y) y[["iter"]]))
      nyrs_vec <- unlist(lapply(scen_list, function(y) y[["nyrs"]]))
      names(n_iter_vec) <- names(scen_list)
      names(nyrs_vec) <- names(scen_list)
      included_scen_iters <- n_iter_vec[which(names(scen_list) %in% tmp_scen)]
      included_nyrs <- nyrs_vec[which(names(scen_list) %in% tmp_scen)]
      if(all(x[["input"]][,"scen"] == "all")) {
        if(length(unique(n_iter_vec)) != 1) {
          stop("Cannot use 'scen = all' for custom if the number of", 
               " iterations differ across scenarios. Please specify values for ", 
               "each scenario, year, and iteration instead.")
        }
        if(length(unique(nyrs_vec)) != 1) {
          stop("Cannot use 'scen = all' for custom if the number of ", 
               "years extended forward differ across scenarios. Please specify ",
               "values for each scenario, year, and iteration instead.")
        }
        n_iters_expect <- unique(n_iter_vec)
        n_years_expect <- unique(nyrs_vec)
        total_rows_expect <- 1 * n_iters_expect * n_years_expect
        if(NROW(x[["input"]]) != total_rows_expect) {
          stop("Number of rows in future_om_list[[", elem_num, "]][['input']] ", 
               "is not correct. Expecting ", total_rows_expect, ", but there ", 
               "are ", NROW(x[['input']]), " rows. Please specify a row for ", 
                "each yr and iter.")
        }
      } else {
        if(nrow(x[["input"]]) != sum(included_scen_iters * included_nyrs)) {
          stop("Number of rows in future_om_list[[", elem_num, "]][['input']] ", 
               "is not correct. Expecting ", 
               sum(included_scen_iters * included_nyrs), ", but there ", 
               "are ", NROW(x[['input']]), " rows. Please specify a row for ", 
               "each year and iter for the included scenarios specified ", 
               " in future_om_list[[", elem_num, "]][['scen']].")
        }
      }
    }
    #replace all scenario designation with the scenario names, in the order that
    # they appear in the scenario list.
    if(isTRUE(x[["scen"]][2] == "all")) {
      x[["scen"]] <- x[["scen"]][1]
      x[["scen"]] <- c(x[["scen"]], names(scen_list))
    }
    x
  }, future_om_list, list_nums, MoreArgs = list(scen_list = scen_list),
  SIMPLIFY = FALSE)
  invisible(future_om_list)
}