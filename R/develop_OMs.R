#' Develop different operating models
#'
#' This is a utility to help a user create new operating models starting from the
#' same model. For now, it is only possible to adjust 1 parameter value
#' @template OM_name
#' @template OM_in_dir
#' @param out_dir Path where the new models will be written. Defaults to the
#'  current working directory.
#' @param par_name Name of the parameter to modify
#' @param par_vals Vector of parameter values to modify in the OM. Assume these
#'  will be fixed so phase will be set as negative.
#' @param refit_OMs Should the models be refit to data? Defaults to TRUE
#' @param hess Should the hessian be estimated if reffiting the OMs? defaults to
#'  FALSE
#' @export
develop_OMs <- function(OM_name = NULL, OM_in_dir = NULL, out_dir = getwd(), par_name, par_vals,
                        refit_OMs = TRUE, hess = FALSE) {
  # check input
  if (!is.null(OM_name)) assertive.types::assert_is_a_string(OM_name)
  if (!is.null(OM_in_dir)) assertive.types::assert_is_a_string(OM_in_dir)
  assertive.types::assert_is_a_string(out_dir)
  assertive.types::assert_is_a_string(par_name)
  assertive.properties::assert_is_atomic(par_vals)
  assertive.types::assert_is_a_bool(refit_OMs)
  if (is.null(OM_name) & is.null(OM_in_dir)) {
    stop(
      "OM_name and OM_in_dir are both NULL. Please specify at least an OM_name or ",
      "OM_in_dir."
    )
  }
  # create the out_dir folder if it doesn't exist
  if (isTRUE(!file.exists(out_dir))) {
    dir.create(out_dir)
  }
  # get the path to the OM if it is in the external package data.
  # specify the OM_in_dir if only specified OM by name.
  if (!is.null(OM_name) & is.null(OM_in_dir)) {
    pkg_dirs <- list.dirs(system.file("extdata", "models", package = "SSMSE"))
    pkg_dirs <- pkg_dirs[-grep("models$", pkg_dirs)] # git rid of model directory.
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
  # read in the starterfile, b/c should be the same across iterations
  start <- r4ss::SS_readstarter(
    file = file.path(OM_in_dir, "starter.ss"),
    verbose = FALSE
  )
  if (refit_OMs == FALSE) {
    # warning("Parameter devs will all be 0 in the OM if the model is not refit.",
    #         " If parameter devs are desired, use refit_OMs = TRUE.")
    # read in parfile to save the recdevs.
    parfile <- r4ss::SS_readpar_3.30(
      parfile = file.path(OM_in_dir, "ss.par"),
      datsource = file.path(OM_in_dir, start[["datfile"]]),
      ctlsource = file.path(OM_in_dir, start[["ctlfile"]]),
      verbose = FALSE
    )
    # note: may want to save  forecast recdevs also?
  }
  if (hess) {
    opts <- ""
  } else {
    opts <- "-nohess"
  }
  OM_out_name <- ifelse(!is.null(OM_name), OM_name, basename(OM_in_dir))
  for (i in par_vals) {
    # copy to a new location and rename to make a new model
    file.copy(from = OM_in_dir, to = out_dir, recursive = TRUE)
    tmp_mod_path <- file.path(
      out_dir,
      paste0(OM_out_name, "_", par_name, "_", as.character(i))
    )
    file.rename(
      from = file.path(out_dir, basename(OM_in_dir)),
      to = tmp_mod_path
    )
    r4ss::SS_changepars(
      dir = tmp_mod_path, ctlfile = "control.ss_new",
      newctlfile = "control_modified.ss", strings = par_name,
      newvals = i, verbose = FALSE
    )
    # remove files with old values
    file.remove(file.path(tmp_mod_path, "control.ss_new"))
    file.remove(file.path(tmp_mod_path, start[["ctlfile"]]))
    file.remove(file.path(tmp_mod_path, "ss.par"))
    file.rename(
      from = file.path(tmp_mod_path, "control_modified.ss"),
      to = file.path(tmp_mod_path, start[["ctlfile"]])
    )
    if (refit_OMs == TRUE) {
      run_ss_model(
        dir = tmp_mod_path,
        admb_options = opts,
        verbose = FALSE
      )
      if (!file.exists(file.path(tmp_mod_path, "control.ss_new"))) {
        warning("Problem refitting model in ", tmp_mod_path)
      }
      if (file.exists(file.path(tmp_mod_path,"data.ss_new")) && file.exists(file.path(tmp_mod_path,"data_echo.ss_new"))) {
        file.remove(file.path(tmp_mod_path,"data.ss_new"))
      }
    } else {
      # run with no estimation
      run_ss_model(
        dir = tmp_mod_path,
        admb_options = "-maxfn 0 -phase 50 -nohess",
        verbose = FALSE
      )
      if (!file.exists(file.path(tmp_mod_path, "control.ss_new"))) {
        warning("Problem running model without estimation in ", tmp_mod_path)
      }
      if (file.exists(file.path(tmp_mod_path,"data.ss_new")) && file.exists(file.path(tmp_mod_path,"data_echo.ss_new"))) {
        file.remove(file.path(tmp_mod_path,"data.ss_new"))
      }
      # add back original recdevs into the model (b/c not specified through the ctl file)
      new_parfile <- r4ss::SS_readpar_3.30(
        parfile = file.path(tmp_mod_path, "ss.par"),
        datsource = file.path(tmp_mod_path, start[["datfile"]]),
        ctlsource = file.path(tmp_mod_path, start[["ctlfile"]]), verbose = FALSE
      )
      if (!is.null(new_parfile[["recdev1"]])) {
        recdev_name <- "recdev1"
      } else {
        recdev_name <- "recdev2"
      }
      new_parfile[[recdev_name]][, "recdev"] <- parfile[[recdev_name]][, "recdev"]

      # add back original F estimates for F method 2 assessments otherwise they all default to 0.05
      if (!is.null(new_parfile[["F_rate"]])) {
        new_parfile[["F_rate"]][, "F"] <- parfile[["F_rate"]][, "F"]
      }

      r4ss::SS_writepar_3.30(new_parfile,
        outfile = file.path(tmp_mod_path, "ss.par"),
        verbose = FALSE, overwrite = TRUE
      )
    }
  }
  # TODO add checks that model converged?
  invisible(OM_in_dir)
}
