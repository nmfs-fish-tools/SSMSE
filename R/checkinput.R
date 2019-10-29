# Functions to check input. Hopefully, these can be general and use common 
# checks across multiple functions.

#' Check the catch dataframe
#' 
#' @param df The catch datafarme to test
#' @author Kathryn Doering
check_catch_df <- function(df) {
  catch_colnames <- c("year", "seas", "fleet", "catch", "catch_se")
  input_colnames <- colnames(df)
  if(length(catch_colnames)!= length(input_colnames)) {
    stop("The catch data frame does not have the correct number of column ",
         "names. The column names should be: ", 
         paste0(catch_colnames, collapse = ", " ), ".")
  }
  if(any(catch_colnames != input_colnames)) {
    stop("The catch data frame does not have the correct column names in the ",
         "correct order. The column names should be: ", 
         paste0(catch_colnames, collapse = ", " ), ".")
  }
  invisible(df)
}

#' Check that the directory for an OM is valid
#' 
#' @param dir Input to check. Should be a directory name that should contain an
#'  SS model that can be used
#'  as an OM.
check_OM_dir <- function(dir) {
  # chack that the dir contains expected SS model files
  all_files <- list.files(dir)
  errors <- NULL
  if(!"starter.ss" %in% all_files) {
    errors <- c(errors, "starter.ss")
  }
  if(!"forecast.ss" %in% all_files){
    errors <- c(errors, "forecast.ss")
  }
  if(!is.null(errors)){
    stop("The file(s): ", paste(errors, collapse = ", "), " is/are missing from ",
        "the directory ", dir, ", which suggests that it is not a valid SS OM ",
        "directory. Please change to a directory containing a valid SS model.")
  }
  invisible(dir)
}