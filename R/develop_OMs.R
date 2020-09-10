#' Develop different operating models
#' 
#' THis is a utility to help a user create new operating models starting from the
#' same model. For now, it is only possible to adjust 1 parameter value
#' @param OM_dir Path to the original operating model
#' @param out_dir Path where the new models will be written. Defaults to the
#'  current working directory.
#' @param par_name Name of the parameter to modify
#' @param par_vals Vector of parameter values to modify in the OM. Assume these
#'  will be fixed so phase will be set as negative.
#' @param refit_OMs Should the models be refit to data? Defaults to TRUE
#' @export
develop_OMs <- function(OM_dir, out_dir = getwd(), par_name, par_vals,
                        refit_OMs = TRUE, hess = TRUE) {
  # check input
  assertive.types::assert_is_a_string(OM_dir)
  assertive.types::assert_is_a_string(out_dir)
  assertive.types::assert_is_a_string(par_name)
  assertive.properties::assert_is_atomic(par_vals)
  assertive.types::assert_is_a_bool(refit_OMs)
  # read in the starterfile, b/c should be the same across iterations
  start <- r4ss::SS_readstarter(file = file.path(OM_dir, "starter.ss"), 
                                verbose = FALSE)
  if(refit_OMs == FALSE) {
    warning("Parameter devs will all be 0 in the OM if the model is not refit.",
            " If parameter devs are desired, use refit_OMs = TRUE.")
    # read in parfile to save the recdevs.
    parfile <- r4ss::SS_readpar_3.30(
      parfile = file.path(OM_dir, "ss.par"),
      datsource = file.path(OM_dir, start$datfile), 
      ctlsource = file.path(OM_dir, "control.ss_new"), 
      verbose = FALSE)
  }
  if (hess) {
    opts <- ""
  } else {
    opts <- "-nohess"
  }
  for (i in par_vals) {
    tmp_new_dir <- file.path(out_dir, 
                             paste0(basename(OM_dir), "_", parname, "_", i))
    # copy to a new location and rename to make a new model
    file.copy(from = OM_dir, to = out_dir, recursive = TRUE)
    tmp_mod_path <- file.path(out_dir, 
                              paste0(basename(OM_dir), "_", parname, "_", i))
    file.rename(from = file.path(out_dir, basename(OM_dir)), 
                to = tmp_mod_path)
    r4ss::SS_changepars(dir = tmp_mod_path, ctlfile = "control.ss_new", 
                        newctlfile = "control_modified.ss", strings = par_name,
                        newvals = i)
    # remove files with old values
    file.remove(file.path(tmp_mod_path, "control.ss_new"))
    file.remove(file.path(tmp_mod_path, start$controlfile))
    file.remove(file.path(tmp_mod_path, "ss.par"))
    file.rename(from = file.path(tmp_mod_path, "control_modified.ss"),
                to = file.path(tmp_mod_path, start$controlfile))
  }
  if(refit_OMs == TRUE) {
    SSMSE:::run_ss_model(dir = tmp_mod_path, 
                         admb_options = opts,
                         verbose = FALSE)
  } else {
    # run with no estimation
    SSMSE:::run_ss_model(dir = tmp_mod_path, 
                         admb_options = "-maxfn 0 -phase 50 -nohess",
                         verbose = FALSE)
    # add back original recdevs into the model (b/c not specified through the ctl file)
    new_parfile <- r4ss::SS_readpar_3.30(
      parfile = file.path(tmp_mod_path, "ss.par"),
      datsource = file.path(tmp_mod_path, start$datfile), 
      ctlsource = file.path(tmp_mod_path, start$controlfile), verbose = FALSE)
    if(!is.null(new_parfile[["recdev1"]])) {
      recdev_name <- "recdev1"
    } else {
      recdev_name <- "recdev2"
    }
    new_parfile[[recdev_name]][, "recdev"] <- parfile[[recdev_name]][, "recdev"]
    r4ss::SS_writepar_3.30(new_parfile, 
                           outfile = file.path(tmp_mod_path, "ss.par"),
                           verbose = FALSE)
  }
  # TODO add checks that model ran and That model converged?
  invisible(OM_dir)
}