# Utility functions for running SS, taken from ss3sim.
# https://github.com/ss3sim/ss3sim

#' Run an operating or estimation model for a specified set of scenario IDs
#'
#' This function takes care of calling SS3. Importantly, it parses whether the
#' user is on Unix or Windows and calls the binary correctly. This lower-level
#' function is meant to be called by higher level functions such as
#' \code{run_ss3sim}, \code{ss3sim_base}, or your own custom
#' function. From \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#' @details ss3sim requires you to place the SS executable in your
#' path. See the vignette \code{vignette("ss3sim-vignette")} for details on
#' this process. The executables themselves can be downloaded from:
#' \url{https://www.dropbox.com/sh/zg0sec6j20sfyyz/AACQiuk787qW882U2euVKoPna}
#'#'
#' @param scenarios Which scenarios to run. Controls which folder contains the
#'   model that SS3 should run on.
#' @param iterations Which iterations to run. Controls which folder contains
#'   the model that SS3 should run on.
#' @param type Are you running the operating or estimation models?
#' @param hess Calculate the Hessian on estimation model runs?
#' @param admb_options Any additional options to pass to the SS3 command.
#' @param ignore.stdout Passed to \code{system}. If \code{TRUE} then ADMB
#'   output is not printed on screen. This will be slightly faster. Set to
#'   \code{FALSE} to help with debugging.
#' @param admb_pause A length of time (in seconds) to pause after running the
#'   simulation model. This can be necessary on certain computers where file
#'   writing can be slightly delayed. For example, on computers where the files
#'   are written over a network connection. If the output files haven't
#'   finished writing before \R starts looking for the output then the
#'   simulation will crash with an error about missing files. The default
#'   value is set to \code{0.01} seconds, just to be safe.
#' @param show.output.on.console Logical: passed on to
#'   \code{\link[base]{system}}.
#' @param ... Anything else to pass to \code{\link[base]{system}}.
#' @author Sean C. Anderson
#' @export

run_ss3model <- function(scenarios, iterations, type = c("om", "em"),
                         admb_options = "", hess = FALSE, ignore.stdout =
                           TRUE, admb_pause = 0.05, 
                         show.output.on.console = FALSE, ...) {
  
  # Input checking:
  admb_options <- sanitize_admb_options(admb_options, "-nohess")
  admb_options <- sanitize_admb_options(admb_options, "-noest")
  
  os <- .Platform$OS.type
  ss_bin <- "ss"
  
  bin <- get_bin(ss_bin)
  
  ss_em_options <- ifelse(hess, "", "-nohess")
  
  for(sc in scenarios) {
    for(it in iterations) {
      message(paste0("Running ", toupper(type), " for scenario: ", sc,
                     "; iteration: ", it))
      if(os == "unix") {
        system(paste0("cd ", file.path(sc, it, type), ";", paste0(bin, " "),
                      ss_em_options, " ", admb_options), 
               ignore.stdout = ignore.stdout, ...)
        rename_ss3_files(path = file.path(sc, it, type), ss_bin = ss_bin,
                         extensions = c("par", "rep", "log", "bar"))
      } else {
        wd <- getwd()
        setwd(file.path(sc, it, type))
        system(paste0(paste0(bin, " "), ss_em_options, admb_options),
               invisible = TRUE, ignore.stdout = ignore.stdout,
               show.output.on.console = show.output.on.console, ...)
        rename_ss3_files(path = "", ss_bin = ss_bin,
                         extensions = c("par", "rep", "log", "bar"))
        setwd(wd)
      }
    }
  }
  Sys.sleep(admb_pause)
}

#' Rename SS3-version-specific files
#' 
#' From \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#' @param path The path to the folder with the files.
#' @param ss_bin A character value giving the SS binary name
#' @param extensions A character vector of file extensions to rename without
#'   periods preceding the values.
#' @author Sean C. Anderson
rename_ss3_files <- function(path, ss_bin, extensions) {
  for(i in seq_along(extensions)) {
    file.rename(from = file.path(path, paste0(ss_bin, ".", extensions[i])),
                to   = file.path(path, paste0("ss",  ".", extensions[i])))
  }
}

#' Check admb options to make sure there aren't flags there shouldn't
#' be
#'
#' @param x The admb options
#' @param exclude A character object (not a vector)
#' @author Sean C. Anderson
sanitize_admb_options <- function(x, exclude = "-nohess") {
  if(length(x) > 1) stop("x should be of length 1")
  if(length(exclude) > 1) stop("exclude should be of length 1")
  
  x_split <- strsplit(x, " ")[[1]]
  x_split_g <- grep(exclude, x_split)
  if(sum(x_split_g) > 0) {
    warning(paste("Removed admb_option", x_split[x_split_g]))
    x_split_clean <- x_split[-x_split_g]
  } else {
    x_split_clean <- x_split
  }
  paste(x_split_clean, collapse = " ")

}

#' Get SS3 binary/executable location in package
#' 
#' Get the binary/executable location in the package SSMSE. This function
#' is from \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#' @param bin_name Name of SS3 binary, defaults to "ss"
#' @return The path to an SS binary. If using the GitHub version of the
#'   package, this will be an internal binary. Otherwise, this function
#'   will search for a version of the binary in your path. See the
#'   ss3sim vignette.
#'
#' @export
#' @examples
#' \dontrun{
#' get_bin()
#' }
get_bin <- function(bin_name = "ss") {
  # code inspiration from glmmADMB package:
  if (.Platform$OS.type == "windows") {
    platform <- "Windows64"
    bin_name <- paste0(bin_name, ".exe")
    bit <- gsub("\\/", "", Sys.getenv("R_ARCH"))
    if (grepl("3", bit)) {
      if (!grepl("86", bit)) {
        platform <- "Windows32"
        warning("SS3 binary is not available for 32-bit ",
                .Platform$OS.type, " within the package. ",
                "You must have an appropriate SS3 binary in your path. ",
                "See the ss3sim vignette.")
      }}
  } else {
    if (substr(R.version$os, 1, 6) == "darwin") {
      platform <- "MacOS"
    } else {
      if (R.version$os == "linux-gnu") {
        platform <- "Linux64"
      } else {
        warning("SS3 binary is not available for OS ", R.version$os,
               " within the package. You must have an appropriate SS3 binary in your ",
                "path. See the ss3sim vignette.")
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
    if(bin == "") stop(paste0("The expected SS3 executable, ", bin_name,
                              ", was not found in your path. See the ss3sim vignette and ?run_ss3model",
                              " for instructions."))
  }
  if (grepl("[[:space:]]", bin)) {
    bin <- shQuote(bin)
  }
  bin
}

#' Get the parameter values for change_bin
#'
#' This function organizes arguments for other functions needed by 
#' \code{change_bin}. From \href{https://github.com/ss3sim/ss3sim}{ss3sim}.
#'
#' @param dat A list of sample arguments from all sampling functions
get_bin_info <- function(dat) {
  dat <- dat[!vapply(dat, is.null, FUN.VALUE = logical(1L))]
  dummy_dat_list <- do.call("rbind", 
                            lapply(seq_along(dat), function(type) {
                              do.call("rbind",
                                      lapply(seq_along(dat[[type]]$fleets), function(fleet) {
                                        data.frame(
                                          "fleet" = dat[[type]]$fleets[fleet],
                                          "year"  = dat[[type]]$years[[fleet]],
                                          "type"  = names(dat)[type],
                                          stringsAsFactors = FALSE)}
                                      ))
                            }))
  invisible(dummy_dat_list)
}
