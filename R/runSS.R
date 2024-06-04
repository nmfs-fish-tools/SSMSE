# Utility functions for running SS, taken from ss3sim.
# https://github.com/ss3sim/ss3sim

#' Run an operating or estimation model
#'
#' This function takes care of calling SS3. Importantly, it parses whether the
#' user is on Unix or Windows and calls the binary correctly. This lower-level
#' function is meant to be called by higher level functions. Modified from
#' run_ss3model in \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#'
#' @param dir The full or relative path to the model directory
#' @param admb_options Any options to pass to SS3 command. Should be
#'  of the form '-option'. Note that no checks are done to ensure this is a
#'  valid ADMB command
#' @param ss3_bin Name of the SS3 executable. Defaults to "ss3"
#' @param ignore.stdout Passed to \code{system}. If \code{TRUE} then ADMB
#'   output is not printed on screen. This will be slightly faster. Set to
#'  \code{FALSE} to help with debugging.
#' @param admb_pause A length of time (in seconds) to pause after running the
#'   simulation model. This can be necessary on certain computers where file
#'   writing can be slightly delayed. For example, on computers where the files
#'   are written over a network connection. If the output files haven't
#'   finished writing before \R starts looking for the output then the
#'   simulation will crash with an error about missing files. The default
#'   value is set to \code{0.01} seconds, just to be safe.
#' @param show.output.on.console Logical: passed on to
#'   \code{\link[base]{system}}.
#' @param check_run Should it be checked that the model ran by deleting the
#'  data.ss_new file if one exists and then checking if one was created?
#'  Defaults to TRUE.
#' @param debug_par_run If set to TRUE, and the run fails, a new folder called
#'  error_check will be created, and the model will be run from control start
#'  values instead of ss.par. The 2 par files are then compared to help debug
#'  the issue with the model run. Defaults to FALSE.
#' @template verbose
#' @param ... Anything else to pass to \code{\link[base]{system}}.
#' @export
#' @author Sean C. Anderson, Kathryn Doering
run_ss_model <- function(dir,
                         admb_options = "",
                         ss3_bin = "ss3",
                         ignore.stdout = TRUE,
                         admb_pause = 0.05,
                         show.output.on.console = FALSE,
                         check_run = TRUE,
                         debug_par_run = FALSE,
                         verbose = FALSE,
                         ...) {
  # TODO: create Input checking: check form of admb options
  dir <- normalizePath(dir)
  check_dir(dir)
  wd_orig <- getwd()
  on.exit(setwd(wd_orig))
  os <- .Platform[["OS.type"]]
  bin <- get_bin(ss3_bin)
  if (check_run == TRUE) {
    # new data file named for ss3 v.3.30.18 and prior
    if (file.exists(file.path(dir,"data.ss_new")) && file.exists(file.path(dir,"data_echo.ss_new"))) {
        file.remove(file.path(dir,"data.ss_new"))
      }
    ss_new_path <- list.files(dir, pattern = "data.ss_new|data_echo.ss_new", full.names = TRUE)
    if (length(ss_new_path) > 0) {
      if (file.exists(ss_new_path)) {
            file.remove(ss_new_path)
          }
    }
  }
  if (verbose) message("Running SS3.")
  if (os == "unix") {
    system(
      paste0(
        "sudo chmod a+x ", bin, "; ", "cd ", dir, "; ", paste0(bin, " "),
        admb_options
      ),
      ignore.stdout = ignore.stdout, ...
    )
  } else {
    setwd(dir)
    system(paste0(paste0(bin, " "), admb_options),
      invisible = TRUE, ignore.stdout = ignore.stdout,
      show.output.on.console = show.output.on.console, ...
    )
  }
  if (check_run == TRUE) {
    if (!file.exists(file.path(dir, "data.ss_new")) && !file.exists(file.path(dir, "data_echo.ss_new"))) {
      if (debug_par_run) {
        test_no_par(
          orig_mod_dir = dir,
          new_mod_dir = file.path(dirname(dir), "OM_error_check")
        )
        # note that this will exit on error.
      } else {
        stop(
          "new data file (data.ss_new if using SS3 3.30.18 or data_echo.ss_new",
          "if using SS3 3.30.21) was not created during the model run, which",
          "suggests SS3 did not run correctly"
        )
      }
    } else {
      if (verbose) "new data file (data.ss_new if using SS3 3.30.18 or data_echo.ss_new if using SS3 3.30.21) created during model run."
    }
  }
  Sys.sleep(admb_pause)
  invisible(dir)
}

#' Get SS3 binary/executable location in package
#'
#' Get the binary/executable location in the package SSMSE. This function
#' is from \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#' @param bin_name Name of SS3 binary, defaults to "ss3"
#' @return The path to an SS3 binary. If using the GitHub version of the
#'   package, this will be an internal binary. Otherwise, this function
#'   will search for a version of the binary in your path. See the
#'   ss3sim vignette.
#' @export
#' @examples
#' \dontrun{
#' get_bin()
#' }
get_bin <- function(bin_name = "ss3") {
  # code inspiration from glmmADMB package:
  if (.Platform[["OS.type"]] == "windows") {
    platform <- "Windows64"
    bin_name <- paste0(bin_name, ".exe")
    bit <- R.version[["arch"]]
    if (grepl("3", bit)) {
      if (!grepl("86", bit)) {
        platform <- "Windows32"
        warning(
          "SS3 binary is not available for 32-bit ",
          .Platform[["OS.type"]], " within the package. ",
          "You must have an appropriate SS3 binary in your path. ",
          "See the ss3sim vignette."
        )
      }
    }
  } else {
    if (substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]] == "x86_64") {
      platform <- "MacOS"
    } else {
      if (substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]] == "aarch64") {
        platform <- "MacOS_arm64"
      } else {
        if (R.version[["os"]] == "linux-gnu") {
          platform <- "Linux64"
        } else {
          warning(
            "SS3 binary is not available for OS ", R.version[["os"]], " ", R.version[["arch"]],
            " within the package. You must have an appropriate SS3 binary in your ",
            "path. See the ss3sim vignette."
          )
        }
      }
    }
  }
  loc <- system.file("bin", package = "SSMSE")
  if (loc != "") { # we found binaries in the package
    bin <- file.path(loc, platform, bin_name)
    if (!file.exists(bin)) bin <- ""
  } else {
    bin <- ""
  }
  if (bin == "") { # resort to binaries in path
    bin <- Sys.which(bin_name)[[1]]
    if (bin == "") {
      stop(
        "The expected SS3 executable, ", bin_name, ", was not found in your",
        " path. See the ss3sim vignette and ?ss3sim::run_ss3model for ",
        "instructions."
      )
    }
  }
  if (grepl("[[:space:]]", bin)) {
    bin <- shQuote(bin)
  }
  bin
}
