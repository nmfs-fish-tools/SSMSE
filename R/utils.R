# utility functions for the packages. Construct objects, move files, etc.

#' Create scen_list object to use in run_SSMSE function.
#'
#' Function to create parameter \code{scen_list} in
#' \code{\link{run_SSMSE}}, but also could be used by users to construct their
#' list prior to using \code{\link{run_SSMSE}}. Note that there is no error
#' checking in this function, so getting output does not insure that this output
#' can be used as input to run_SSMSE.
#' @template run_SSMSE_scen_list
#' @author Kathryn Doering
#' @export
#' @examples
#' scen_list <- create_scen_list(
#'   scen_name_vec = c("scen 1", "scen_2"),
#'   out_dir_scen_vec = file.path("path", "to", "dir"),
#'   iter_vec = list(1:2, 5:7),
#'   OM_name_vec = "cod",
#'   OM_in_dir_vec = NULL,
#'   EM_name_vec = "cod",
#'   EM_in_dir_vec = NULL,
#'   MS_vec = "EM",
#'   use_SS_boot_vec = TRUE,
#'   nyrs_vec = 6,
#'   nyrs_assess_vec = 3,
#'   sample_struct_list = NULL
#' )
create_scen_list <- function(scen_name_vec,
                             out_dir_scen_vec = NULL,
                             iter_vec = NULL,
                             OM_name_vec = NULL,
                             OM_in_dir_vec = NULL,
                             EM_name_vec = NULL,
                             EM_in_dir_vec = NULL,
                             MS_vec = NULL,
                             use_SS_boot_vec = NULL,
                             nyrs_vec = NULL,
                             nyrs_assess_vec = NULL,
                             sample_struct_list = NULL,
                             sample_struct_hist_list = NULL,
                             sample_catch_vec = NULL,
                             interim_struct_list = NULL) {
  # note that input checking
  scen_name_vec <- as.character(scen_name_vec)
  # construct list. Note that it may not be usable at this stage, but there
  # will be another function to check it.
  scen_list <- lapply(scen_name_vec, function(x) NULL)
  names(scen_list) <- scen_name_vec

  # Note that the below is written as a function with documentation because it
  # was fairly long and the meaning of the variables may not be immediately
  # clear.

  # function to get the value for a scenario, which depends on if it is a list,
  # vector, or null. also includes error checking.
  #
  # @param num_scen Which number scenario in the list is this?
  # @param var The variable we want to pull output from. Should be a vector or
  # list, but has error checking in case it is not.
  # @param var_name The name of the variable, as a string.
  # @param len_scen_name_vec The total number of scenarios being investigated
  get_scen_list_val <- function(var, var_name, num_scen, len_scen_name_vec) {
    if (is.null(var)) {
      return_val <- NULL
    } else {
      if (length(grep("list", var_name)) == 1) {
        assertive.types::assert_is_list(var)
        if (length(var) == 1) {
          return_val <- var[[1]]
        } else if (length(var) == len_scen_name_vec) {
          return_val <- var[[num_scen]]
        } else {
          stop(
            var_name, " has length ", length(var), ", but should either have ",
            "length of 1 or length equal to the number of scenarios (i.e., ",
            len_scen_name_vec, ")."
          )
        }
      } else if (length(grep("vec", var_name)) == 1) {
        if (!is.vector(var)) {
          stop(
            var_name, " should be a vector, but is not. It currently looks ",
            "like ", var, ". Please make it a vector."
          )
        }
        if (length(var) == 1) {
          return_val <- var[1]
        } else if (length(var) == len_scen_name_vec) {
          return_val <- var[num_scen]
        } else {
          stop(
            var_name, " has length ", length(var), "but should either have ",
            "length 1 or equal to the number of scenarios , (i.e., ",
            len_scen_name_vec, "."
          )
        }
      } else {
        stop(
          "var_name should have 'vec' as part of its name to indicate it is ",
          "a vector or 'list' to indicate it is a list, but did not have ",
          "either terms. Please add this to the var name depending on its ",
          "structure."
        )
      }
      # use short circuiting && to avioid testing the second statement if legth
      # isn't 1. want to change NAs to NULLs to be consistent with what the
      # function expects.
      if (length(return_val) == 1 && is.na(return_val)) {
        return_val <- NULL
      }
    }
    return_val
  }
  # use the function to get values and put them in for each list component.
  all_vars <- names(formals(SSMSE::create_scen_list))[-1]
  # get names without _list or _vec
  all_vars_new_names <- unlist(strsplit(all_vars, split = "_list$|_vec$"))
  scen_list <- vector(mode = "list", length = length(scen_name_vec))
  for (i in seq_along(scen_name_vec)) {
    tmp_vals <- lapply(all_vars,
      function(x, var_name, num_scen, len_scen_name_vec) {
        get_scen_list_val(get(x),
          var_name = x,
          num_scen,
          len_scen_name_vec
        )
      },
      num_scen = i, len_scen_name_vec = length(scen_name_vec)
    )
    scen_list[[i]] <- tmp_vals
    # add names
    names(scen_list[[i]]) <- all_vars_new_names
  }
  names(scen_list) <- scen_name_vec
  # return list
  scen_list
}

#' clean the initial model files
#'
#' @template OM_out_dir
#' @template EM_out_dir
#' @template MS
#' @template overwrite
#' @importFrom r4ss SS_readstarter SS_readdat SS_writedat
clean_init_mod_files <- function(OM_out_dir, EM_out_dir = NULL, MS = "EM",
                                 overwrite = FALSE) {
  # read in files
  OM_start <- SS_readstarter(file.path(OM_out_dir, "starter.ss"),
    verbose = FALSE
  )
  OM_dat <- SS_readdat(file.path(OM_out_dir, OM_start[["datfile"]]),
    verbose = FALSE,
    section = 1
  )
  OM_ctl <- SS_readctl(file.path(OM_out_dir, OM_start[["ctlfile"]]),
    datlist = OM_dat,
    verbose = FALSE
  )
  if (!is.null(EM_out_dir)) {
    EM_start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"),
      verbose = FALSE
    )

    # turn on sd reporting for all em model years by default
    EM_start[["minyr_sdreport"]] <- -1
    EM_start[["maxyr_sdreport"]] <- -2

    EM_dat <- SS_readdat(file.path(EM_out_dir, EM_start[["datfile"]]), verbose = FALSE)

    EM_ctl <- SS_readctl(file.path(EM_out_dir, EM_start[["ctlfile"]]), datlist = EM_dat, verbose = FALSE)
  } else {
    EM_start <- NULL
    EM_dat <- NULL
    EM_ctl <- NULL
  }
  # model start and end yr should be the same for both
  styr <- OM_dat[["styr"]]
  endyr <- OM_dat[["endyr"]]




  # get years in range function
  get_yrs_in_range <- function(list_name, dat, styr, endyr) {
    df <- dat[[list_name]]
    if (!is.null(df)) {
      # find the year col name
      yr_name <- grep("^[yY]e?a?[r]$", colnames(df), value = TRUE)
      if (length(yr_name) != 1) {
        stop(
          "Problem in clean_init_mod_files function: looking for year col",
          " in the data file matched multiple values. Please contact the ",
          "developers for assistance."
        )
      }
      # subset to only values in model range
      df_new <- df[(df[, yr_name] >= styr & df[, yr_name] <= endyr), ]
    } else {
      df_new <- df
    }
    # return the new dataframe
    df_new
  }

  # remove any year observations not in the range of the model yrs
  clean_dat <- lapply(list(OM_dat = OM_dat, EM_dat = EM_dat),
    function(dat, styr, endyr) {
      list_names <- c(
        "CPUE", "lencomp", "agecomp", "meanbodywt",
        "MeanSize_at_Age_obs"
      )
      new_dfs <- lapply(list_names, get_yrs_in_range,
        dat = dat, styr = styr,
        endyr = endyr
      )

      for (i in seq_along(list_names)) {
        dat[[list_names[i]]] <- new_dfs[[i]]
      }
      dat
    },
    styr = styr, endyr = endyr
  )

  if (!is.null(OM_ctl)) {
    if (!is.null(OM_ctl[["Variance_adjustment_list"]])) {
      if (length(OM_ctl[["Variance_adjustment_list"]][, 1]) > 0) {
        warning(
          "Original OM model files have variance adjustment factors specified.",
          "This may have unintended effects such as causeing sample sizes to differ from those specified.",
          "If you didn't do this intentionally we suggest turning of variance adjustments."
        )
      }
    }
    if (!is.null(OM_ctl[["Q_options"]])) {
      if (sum(OM_ctl[["Q_options"]][["extra_se"]]) > 0) {
        warning(
          "Original OM model files have extra SE added to catchability specified.",
          "This may have unintended effects such as causeing sample sizes to differ from those specified.",
          "If you didn't do this intentionally we suggest turning of variance adjustments."
        )
      }
    }
  }
  if (!is.null(EM_ctl)) {
    if (!is.null(EM_ctl[["Variance_adjustment_list"]])) {
      if (length(EM_ctl[["Variance_adjustment_list"]][, 1]) > 0) {
        warning(
          "Original EM model files have variance adjustment factors specified.",
          "This may have unintended effects such as causeing sample sizes to differ from those specified.",
          "If you didn't do this intentionally we suggest turning of variance adjustments."
        )
      }
    }
    if (!is.null(EM_ctl[["Q_options"]])) {
      if (sum(EM_ctl[["Q_options"]][["extra_se"]]) > 0) {
        warning(
          "Original EM model files have extra SE added to catchability specified.",
          "This may have unintended effects such as causeing sample sizes to differ from those specified.",
          "If you didn't do this intentionally we suggest turning of variance adjustments."
        )
      }
    }
  }
  if (!is.null(EM_dat)) {
    if (EM_start[["init_values_src"]] == 1) {
      warning(
        "Original EM model files read initial values from ss.par, but ",
        "SSMSE can only read initial values from control. Changing EM to",
        "read initial values from control file."
      )
      EM_start[["init_values_src"]] <- 0
    }
  }

  if (overwrite) {
    SS_writedat(
      datlist = clean_dat[["OM_dat"]],
      outfile = file.path(OM_out_dir, OM_start[["datfile"]]),
      verbose = FALSE,
      overwrite = TRUE
    )
    if (!is.null(EM_dat)) {
      SS_writedat(
        datlist = clean_dat[["EM_dat"]],
        outfile = file.path(EM_out_dir, EM_start[["datfile"]]),
        verbose = FALSE,
        overwrite = TRUE
      )
      r4ss::SS_writestarter(EM_start,
        dir = EM_out_dir,
        overwrite = TRUE,
        verbose = FALSE
      )
    }
  }
  clean_dat[["EM_start"]] <- EM_start
  clean_dat
}

#' return a value from a data frame
#'
#' Return a single value from a column of a dataframe using the method specified
#' @param data A dataframe which has a column that matches (at least partially)
#'  colname
#' @param method How should the value to be returned be selected? Current
#'  options include "most_common_value", where the most common input uncertainty
#'  value will be returned and "only_value" where all input values must be the
#'  same in data; if they are, this value will be returned. Otherwise, an error
#'  will be generated.
#' @param colname Column name as a string in \code{data}. Note that partial
#'  matching and regular expressions can be used.
#' @param group Column name as a string in \code{data} used to group the data
#'  before calculating the input value to use. Defaults to NULL.
#' @return A value of the same type as \code{data[, colname]} if \code{group}
#'  is NULL, or a data.frame if \code{group} is specified.
#' @author Kathryn Doering
#' @details Note that this function was created intially to return a value to
#'  use as the input uncertainty, but it should be generalizable to pulling a
#'  value from a column in any data frame using the method specified.
#' @examples
#' dfr <- data.frame(
#'   "year" = 1:5,
#'   "value" = c(2, 2, 2, 3, 3),
#'   "se_log" = 0.2
#' )
#' SSMSE:::get_input_value(
#'   data = dfr, method = "most_common_value", colname = "se_log",
#'   group = "value"
#' )
#' SSMSE:::get_input_value(data = dfr, method = "most_common_value", colname = "value")
#' SSMSE:::get_input_value(data = dfr, method = "only_value", colname = "se_log")
#' # generates an error:
#' # SSMSE:::get_input_value(data = dfr, method = "only_value", colname = "value")
get_input_value <- function(data,
                            method = "most_common_value",
                            colname,
                            group = NULL) {
  # input checks
  assertive.types::assert_is_data.frame(data)
  assertive.properties::assert_has_colnames(data)
  assertive.types::assert_is_a_string(method)
  assertive.types::assert_is_a_string(colname)
  if (!is.null(group)) assertive.types::assert_is_a_string(group)
  method_values <- c("most_common_value", "only_value")
  if (!method %in% method_values) {
    stop(
      "method possible values are: ", paste0(method_values, collapse = ", "),
      "; method was specified as ", method
    )
  }
  selected_col <- grep(colname, colnames(data))
  selected_colname <- colnames(data)[selected_col]

  if (length(selected_col) == 0) {
    stop("column ", colname, " not found in data.")
  }
  if (length(selected_col) > 1) {
    stop(
      "The value specified for colname ", colname, " selected more than 1",
      " column in data (columns matched: ",
      paste0(selected_colname, collapse = ", "),
      "). Note that partial matching and regular expressions",
      " are used to find the column(s) that match with colname."
    )
  }
  if (!is.null(group)) {
    group_orig <- group
    group_col <- grep(group, colnames(data))
    group <- colnames(data)[group_col]
    if (length(group) == 0) {
      stop("column ", group_orig, " not found in data.")
    }
    if (length(group) > 1) {
      stop(
        "The value specified for colname ", group_orig, " selected more than 1",
        " column in data (columns matched: ",
        paste0(group, collapse = ", "),
        "). Note that partial matching and regular expressions",
        " are used to find the column(s) that match with colname."
      )
    }
    if (group == selected_colname) {
      stop(
        "group and colname cannot be the same. Both selected column",
        group
      )
    }
  }
  # get the value
  if (method == "most_common_value") {
    if (is.null(group)) {
      ux <- unique(data[, selected_col])
      val <- ux[which.max(tabulate(match(data[, selected_col], ux)))]
      assertive.properties::assert_is_of_length(val, 1)
    } else {
      val <- stats::aggregate(data[, selected_colname],
        by = list("group" = data[, group]),
        # find the most common value (mode)
        FUN = function(x) {
          unique(x)[which.max(tabulate(match(x, unique(x))))]
        }, drop = FALSE
      )
      assertive.types::assert_is_data.frame(val) # sanity check
      colnames(val) <- c(group, selected_colname)
    }
  }
  if (method == "only_value") {
    if (is.null(group)) {
      val <- unique(data[, selected_col])
      # check return value
      if (length(val) > 1) {
        stop(
          "Multiple unique values were found in data with colname ",
          selected_colname,
          ". Because method is only_value, this function only works if all ",
          "values in the column are the same."
        )
      }
      if (length(val) == 0) {
        stop("No value found in ", selected_colname, ".")
      }
      # sanity check for developers
      assertive.properties::assert_is_of_length(val, 1)
    } else {
      n_vals <- stats::aggregate(data[, selected_colname],
        by = list("group" = data[, group]),
        # find the most common value (mode)
        FUN = function(x) {
          length(unique(x))
        }, drop = FALSE
      )
      if (any(unlist(n_vals[, "x"]) > 1)) {
        stop(
          "Multiple unique values were found in data with colname ",
          selected_colname,
          "after grouping by ", group, ". Because method is only_value, ",
          "this function only works if all values in the column within the ",
          "same grouping are the same."
        )
      }
      # TODO: need check if any are length 0??? or check outside function?
      # get the value
      val <- stats::aggregate(data[, selected_colname],
        by = list("group" = data[, group]),
        # find the most common value (mode)
        FUN = function(x) {
          return <- unique(x)
          if (length(return) > 1) {
            stop("Problem calculating.")
          }
          return
        }, drop = FALSE
      )
      assertive.types::assert_is_data.frame(val) # sanity check
      colnames(val) <- c(group, selected_colname)
    }
  }
  val
}

#' create the OM directory
#'
#' Create an OM directory within the out_dir specified (named by the value of
#' niter)
#' @param out_dir  The directory to which to write output. IF NULL, will default
#'  to the working directory.
#' @param niter The number iteration
#' @template OM_name
#' @template OM_EM_in_dir
#' @param EM_name Name of the EM model
#' @return A list with 2 named components each of length 1 characters. The
#'  components are: OM_dir, where OM will be run, and OM_in_dir, where the model
#'  files will be copied from.
create_out_dirs <- function(out_dir, niter, OM_name, OM_in_dir,
                            EM_name = NULL, EM_in_dir = NULL) {
  # checks
  if (!is.null(out_dir)) assertive.types::assert_is_a_string(out_dir)
  if (!is.null(OM_name)) assertive.types::assert_is_a_string(OM_name)
  if (is.null(OM_name) & is.null(OM_in_dir)) {
    stop(
      "OM_name and OM_in_dir are both NULL. Please specify an OM_name, ",
      "OM_in_dir, or both."
    )
  }
  if (!is.null(EM_name)) assertive.types::assert_is_a_string(EM_name)
  if (!is.null(EM_in_dir)) assertive.types::assert_is_a_string(EM_in_dir)
  # create out_dir, named by the value of niter
  if (is.null(out_dir)) out_dir <- getwd()
  out_dir <- file.path(out_dir, as.character(niter))
  if (dir.exists(out_dir)) {
    warning(out_dir, " already exists, so skipping the iteration.")
    return(NULL)
  }
  dir.create(out_dir, showWarnings = FALSE)
  # specify the OM_in_dir if only specified OM by name.
  pkg_dirs <- list.dirs(system.file("extdata", "models", package = "SSMSE"))
  pkg_dirs <- pkg_dirs[-grep("models$", pkg_dirs)] # git rid of model directory.

  if (!is.null(OM_name) & is.null(OM_in_dir)) {
    OM_in_dir <- pkg_dirs[grep(OM_name, pkg_dirs)]
    if (length(OM_in_dir) != 1) {
      stop(
        "OM_name ", OM_name, " matched ", length(OM_in_dir), " models in ",
        "SSMSE external package data, but should match 1. Please ",
        "change OM_name to match (or partially match unambiguously) with 1 ",
        "model in the models folder of the SSMSE external package data. ",
        "Model options are: ", paste0(basename(pkg_dirs), collapse = ", ")
      )
    }
  }
  # figure out the OM_name and create the directory within the one just created
  if (!is.null(OM_name)) {
    OM_folder_name <- paste0(OM_name, "_OM")
  } else {
    OM_folder_name <- paste0(basename(OM_in_dir), "_OM")
  }
  OM_out_dir <- file.path(out_dir, OM_folder_name)
  dir.create(OM_out_dir, showWarnings = FALSE)
  # Add the EM dir, if necessary
  if (is.null(EM_name) & !is.null(EM_in_dir)) EM_name <- basename(EM_in_dir)
  if (!is.null(EM_name) & is.null(EM_in_dir)) {
    EM_in_dir <- pkg_dirs[grep(EM_name, pkg_dirs)]
    if (length(EM_in_dir) != 1) {
      stop(
        "EM_name ", EM_name, " matched ", length(EM_in_dir), " models in ",
        "SSMSE external package data, but should match 1. Please ",
        "change EM_name to match (or partially match unambiguously) with 1 ",
        "model in the models folder of the SSMSE external package data. ",
        "Model options are: ", paste0(basename(pkg_dirs), collapse = ", ")
      )
    }
  }
  if (!is.null(EM_name) | !is.null(EM_in_dir)) {
    EM_out_dir <- file.path(out_dir, paste0(EM_name, "_EM_init"))
    dir.create(EM_out_dir, showWarnings = FALSE)
  }
  if (is.null(EM_name) & is.null(EM_in_dir)) {
    EM_out_dir <- NULL
    EM_in_dir <- NULL
  }
  OM_mod_loc <- list(
    OM_in_dir = OM_in_dir, OM_out_dir = OM_out_dir,
    EM_in_dir = EM_in_dir, EM_out_dir = EM_out_dir
  )
}

#' Locate the OM model files
#'
#' @template OM_name
#' @param OM_in_dir Relative or absolute path to the operating model, if using a
#'   model outside of the SSMSE package. Should be a string.
#' @return A list with on comonent, OM_in_dir, which contains the model location
locate_in_dirs <- function(OM_name = NULL, OM_in_dir = NULL) {
  # checks
  if (!is.null(OM_name)) assertive.types::assert_is_a_string(OM_name)
  if (!is.null(OM_in_dir)) assertive.types::assert_is_a_string(OM_in_dir)
  if (is.null(OM_name) & is.null(OM_in_dir)) {
    stop(
      "OM_name and OM_in_dir are both NULL. Please specify an OM_name, ",
      "OM_in_dir, or both."
    )
  }
  # specify the OM_in_dir if only specified OM by name.
  pkg_dirs <- list.dirs(system.file("extdata", "models", package = "SSMSE"))
  pkg_dirs <- pkg_dirs[-grep("models$", pkg_dirs)] # git rid of model directory.

  if (!is.null(OM_name) & is.null(OM_in_dir)) {
    OM_in_dir <- pkg_dirs[grep(OM_name, pkg_dirs)]
    if (length(OM_in_dir) != 1) {
      stop(
        "OM_name ", OM_name, " matched ", length(OM_in_dir), " models in ",
        "SSMSE external package data, but should match 1. Please ",
        "change OM_name to match (or partially match unambiguously) with 1 ",
        "model in the models folder of the SSMSE external package data. ",
        "Model options are: ", paste0(basename(pkg_dirs), collapse = ", ")
      )
    }
  }
  OM_mod_loc <- list(OM_in_dir = OM_in_dir)
}

#' Copy OM and EM model files
#'
#' Copy OM and EM model files from input to output location.
#' @template OM_EM_in_dir
#' @template OM_out_dir
#' @template EM_out_dir
#' @template verbose
#' @return TRUE, if copying is successful
#'
copy_model_files <- function(OM_in_dir = NULL,
                             OM_out_dir = NULL,
                             EM_in_dir = NULL,
                             EM_out_dir = NULL,
                             verbose = FALSE) {
  # checks
  if (!is.null(OM_in_dir)) {
    dat_file <- list.files(OM_in_dir, pattern = "data.ss_new|data_echo.ss_new")
    if (!all(c(
      "control.ss_new", dat_file, "starter.ss_new",
      "forecast.ss_new", "ss.par"
    ) %in% list.files(OM_in_dir))) {
      stop(
        ".ss_new files not found in the original OM directory ",
        OM_in_dir, ". Please run the model to make the .ss_new files available."
      )
    }
  }
  # copy over OM ----
  if (!is.null(OM_in_dir) & !is.null(OM_out_dir)) {
    if (verbose == TRUE) {
      message(
        "Copying over .ss_new model files in ", OM_in_dir,
        " to ", OM_out_dir, "."
      )
    }
    success_OM <- r4ss::copy_SS_inputs(
      dir.old = OM_in_dir,
      dir.new = OM_out_dir,
      overwrite = FALSE,
      use_ss_new = TRUE, # will rename the ss new files, also.
      copy_par = TRUE,
      verbose = FALSE
    )
    if (success_OM == FALSE) {
      stop(
        "Problem copying SS OM .ss_new files from ", OM_in_dir, " to ",
        OM_out_dir, "."
      )
    }
  } else {
    success_OM <- TRUE
  }
  # copy over EM ----
  if (!is.null(EM_in_dir) & !is.null(EM_out_dir)) {
    if (verbose) {
      message(
        "Copying over input model files in ", EM_in_dir, " to ",
        EM_out_dir, "."
      )
    }
    success_EM <- r4ss::copy_SS_inputs(
      dir.old = EM_in_dir,
      dir.new = EM_out_dir,
      overwrite = FALSE,
      copy_par = TRUE,
      verbose = FALSE
    )
    if (success_EM == FALSE) {
      stop(
        "Problem copying SS EM files from ", EM_in_dir, "to",
        EM_out_dir, "."
      )
    }
  } else {
    success_EM <- TRUE
  }
  invisible(c(success_OM = success_OM, success_EM = success_EM))
}

#' function that creates a combined column to the list_item of interest
#'
#' @param dat_list An SS data file as a list read in using r4ss
#' @param list_item List item in dat_list to extract and return a modified
#'  version of this value
#' @param colnames Column names in list_item
combine_cols <- function(dat_list, list_item, colnames) {
  tmp <- dat_list[[list_item]]
  combo <- NULL
  for (n in colnames) {
    combo <- paste0(combo, tmp[, n], "_")
  }
  tmp[["combo"]] <- combo
  tmp
}

#' Set the initial global, scenario, and iteration seeds
#'
#' @template seed_input
#' @param iter_vec The number of iterations per scenario. A vector of integers
#'  in the same order as scen_name_vec.
#' @export
#' @returns A list of length 3 with 1) the global seed value; 2) the scenario seed values; and 3) the iteration seed values.
#' @examples
#' seeds <- set_MSE_seeds(seed = seq(10, 80, by = 10), iter_vec = c(2, 3))
set_MSE_seeds <- function(seed = NULL, iter_vec) {
  if (is.null(seed)) {
    seed <- list()
    seed[["global"]] <- floor(stats::runif(1, 1000000, 9999999))
    set.seed(seed = seed[["global"]])
    seed[["scenario"]] <- floor(stats::runif(length(iter_vec), 1000000, 9999999))
    seed[["iter"]] <- list()
    for (i in 1:length(iter_vec)) {
      set.seed(seed = seed[["scenario"]][i])
      seed[["iter"]][[i]] <- floor(stats::runif(iter_vec[i], 1000000, 9999999))
    }
  } else if (!is.list(seed)) {
    if (length(seed) == 1) {
      input.seed <- seed
      seed <- list()
      seed[["global"]] <- input.seed[1]
      set.seed(seed = seed[["global"]])
      seed[["scenario"]] <- floor(stats::runif(length(iter_vec), 1000000, 9999999))
      seed[["iter"]] <- list()
      for (i in 1:length(iter_vec)) {
        set.seed(seed = seed[["scenario"]][i])
        seed[["iter"]][[i]] <- floor(stats::runif(iter_vec[i], 1000000, 9999999))
      }
    } else if (length(seed) == (length(iter_vec) + 1)) {
      input.seed <- seed
      seed <- vector(mode = "list", length = 3)
      names(seed) <- c("global", "scenario", "iter")
      seed[["global"]] <- input.seed[1]
      input.seed <- input.seed[-1]
      seed[["scenario"]] <- input.seed
      seed[["iter"]] <- list()
      for (i in 1:length(iter_vec)) {
        set.seed(seed = seed[["scenario"]][i])
        seed[["iter"]][[i]] <- floor(stats::runif(iter_vec[i], 1000000, 9999999))
      }
    } else if (length(iter_vec) == length(iter_vec)) {
      if (length(seed) == (iter_vec[1] + length(iter_vec) + 1)) {
        if (length(unique(iter_vec) == 1)) {
          input.seed <- seed
          seed <- vector(mode = "list", length = 3)
          names(seed) <- c("global", "scenario", "iter")
          seed[["global"]] <- input.seed[1]
          input.seed <- input.seed[-1]
          seed[["scenario"]] <- input.seed[1:length(iter_vec)]
          input.seed <- input.seed[-c(1:length(iter_vec))]
          seed[["iter"]] <- list()
          for (i in 1:length(iter_vec)) {
            seed[["iter"]][[i]] <- input.seed
          }
        } else {
          stop("Error: All scenarios must have same number of iterations to use a single input of seeds")
        }
      } else if (length(seed) == (sum(iter_vec) + length(iter_vec) + 1)) {
        input.seed <- seed
        seed <- vector(mode = "list", length = 3)
        names(seed) <- c("global", "scenario", "iter")
        seed[["global"]] <- input.seed[1]
        input.seed <- input.seed[-1]
        seed[["scenario"]] <- input.seed[1:length(iter_vec)]
        input.seed <- input.seed[-c(1:length(iter_vec))]
        seed[["iter"]] <- list()
        for (i in 1:length(iter_vec)) {
          seed[["iter"]][[i]] <- input.seed[1:iter_vec[i]]
          input.seed <- input.seed[-c(1:iter_vec[i])]
        }
      } else {
        stop("The length of your seed vector doesn't match either
          (1, 1+n_scenarios, 1+n_scenarios+n_iterations_single_scenario,
          or 1+n_scenarios+n_iterations_all_scenarios)")
      }
    } else if (length(iter_vec) == 1) {
      if (length(seed) == (iter_vec[1] + length(iter_vec) + 1)) {
        input.seed <- seed
        seed <- vector(mode = "list", length = 3)
        names(seed) <- c("global", "scenario", "iter")
        seed[["global"]] <- input.seed[1]
        input.seed <- input.seed[-1]
        seed[["scenario"]] <- input.seed[1:length(iter_vec)]
        input.seed <- input.seed[-c(1:length(iter_vec))]
        seed[["iter"]] <- list()
        for (i in 1:length(iter_vec)) {
          seed[["iter"]][[i]] <- input.seed
        }
      } else if (length(seed) == ((iter_vec[1] * length(iter_vec)) + length(iter_vec) + 1)) {
        input.seed <- seed
        seed <- vector(mode = "list", length = 3)
        names(seed) <- c("global", "scenario", "iter")
        seed[["global"]] <- input.seed[1]
        input.seed <- input.seed[-1]
        seed[["scenario"]] <- input.seed[1:length(iter_vec)]
        input.seed <- input.seed[-c(1:length(iter_vec))]
        seed[["iter"]] <- list()
        for (i in 1:length(iter_vec)) {
          seed[["iter"]][[i]] <- input.seed[1:iter_vec[i]]
          input.seed <- input.seed[-c(1:iter_vec[i])]
        }
      } else {
        stop("The length of your seed vector doesn't match either
          (1, 1+n_scenarios, 1+n_scenarios+n_iterations_single_scenario,
          or 1+n_scenarios+n_iterations_all_scenarios)")
      }
    }
  } else {
    input.seed <- seed
    seed <- vector(mode = "list", length = 3)
    names(seed) <- c("global", "scenario", "iter")
    if (length(input.seed[["global"]]) == 1) {
      seed[["global"]] <- input.seed[["global"]]
    } else if (length(input.seed[[1]]) == 1) {
      seed[["global"]] <- input.seed[[1]]
    } else {
      stop("seed entered as list but no seed[['global']] and first element more that 1 value")
    }

    if (length(input.seed[["scenario"]]) == length(iter_vec)) {
      seed[["scenario"]] <- input.seed[["scenario"]]
    } else if (length(input.seed[[2]]) == length(iter_vec)) {
      seed[["scenario"]] <- input.seed[[2]]
    } else {
      stop("seed entered as list but no seed[['scenario']] and second element not the same
             length as the number of scenarios")
    }

    if (length(input.seed[["iter"]]) == length(iter_vec) & is.list(input.seed[["iter"]])) {
      seed[["iter"]] <- input.seed[["iter"]]
      for (i in 1:length(seed[["iter"]])) {
        if (length(iter_vec) == 1) {
          loc <- 1
        } else {
          loc <- i
        }
        if (length(seed[["iter"]][[i]]) != iter_vec[loc]) {
          stop("wrong number of seeds for iterations")
        }
      }
    } else if (length(input.seed[["iter"]]) == length(iter_vec)) {
      seed[["iter"]] <- input.seed[["iter"]]
      for (i in 1:length(seed[["iter"]])) {
        if (length(iter_vec) == 1) {
          loc <- 1
        } else {
          loc <- i
        }
        if (length(seed[["iter"]][[i]]) != iter_vec[loc]) {
          stop("wrong number of seeds for iterations")
        }
      }
    } else {
      stop("seed entered as list but no seed[['iter']] and third element not the same
             length as the number of scenarios")
    }
  }
  return(seed)
}

# Create global variables to decrease warnings in check()
utils::globalVariables(c(
  "Rel_SSB",
  "SSB_Unfished",
  "SSB_ratio",
  "SpawnBio",
  "SpawnBio_EM",
  "SpawnBio_OM",
  "iteration",
  "model_run",
  "model_type",
  "scenario",
  "sd",
  "year"
))
