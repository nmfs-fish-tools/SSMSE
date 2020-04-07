# functions to calculate F.

#' Get the Fishing mortality from the timeseries Report.sso table
#' 
#' @param timeseries from SSoutput
#' @importFrom tidyr gather separate
#' @return a data frame with Fs by Yr, Era, Seas, and Fleet (long format)
get_F <- function(timeseries) {
  # find the F columns
  F_col_ind <- grep("^F:_\\d+$", colnames(timeseries))
  # Note that Area does not need to be included b/c fleets can only operate in
  # 1 area
  other_col_ind <- which(colnames(timeseries) %in% c("Yr", "Era", "Seas"))
  # Note: pivot_longer is a newer alternative, but it is not yet stable, so 
  # the "retired" function gather was used
  # may also be able to use stats::reshape or aggregate here.
  # create a long F data frame so columns are Yr, Era, Seas, Fleet, F
  F_df <- timeseries[, c(other_col_ind, F_col_ind)]
  # change from log to wide formate
  F_df <- tidyr::gather(F_df, key = "tmp_Fleet", value = "F", 
                 grep("^F:_\\d+$", colnames(F_df)))
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  F_df <- tidyr::separate(F_df, col = "tmp_Fleet", into = c(NA, "Fleet"), 
                          sep = ":_",convert = TRUE)
  
  # get the F_rate, if any, by finding only the values during the model period
  # and that are greater than 0 (need to make sure there is retained catch?)
  F_rate <- F_df[F_df$F > 0 & F_df$Era == "TIME", 
                 setdiff(colnames(F_df), c("Era"))]
  # the following should work, but this sanity check added to avoid assigning
  # the wrong column names. May not work if order of df col changes.
  if(all(colnames(F_rate) == c("Yr", "Seas", "Fleet", "F"))) {
    colnames(F_rate) <- c("year", "seas", "fleet", "F")
  } else {
    stop("Column names not in the correct order.")
  }
  # Make sure that the df is ordered correctly; by year, season, fleet
  F_rate <- F_rate[order(F_rate[,"year"], F_rate[,"seas"], F_rate[,"fleet"]), ]
  if(nrow(F_rate) == 0) {
    F_rate <- NULL
  }
  # form init_F
  init_F <- F_df[F_df$F > 0 & F_df$Era == "Init", c("Fleet", "F")]
  #order by fleet
  init_F <- init_F[order(init_F[,"Fleet"])]
  if(nrow(init_F) == 0) {
    init_F <- NULL
  } else {
    # feed back as a named vector sorted by fleet
    init_F_names <- init_F$Fleet
    init_F <- init_F[, "F", drop = TRUE]
    names(init_F) <- init_F_names
  }

  F_list <- list(F_df = F_df, F_rate = F_rate, init_F = init_F)
}

#' Get retained catch from the timeseries Report.sso table
#' 
#' @param timeseries from SSoutput
#' @param units_of_catch From datalist
#' @importFrom tidyr gather separate
#' @return a data frame with retained catch by Area, Yr, Era, Seas, Fleet, and 
#'  units (long format)
get_retained_catch <- function(timeseries, units_of_catch) {
  units_catch_string <- ifelse(units_of_catch == 1, "B", "N")
  retain_catch_colnames <- paste0("retain(", units_catch_string, "):_",
                                  seq_along(units_catch_string))
  # some may not be included in output if no catch.
  retain_catch_colnames <- retain_catch_colnames[
                            retain_catch_colnames %in% colnames(timeseries)]
  # switch from wide to long format.
  retain_catch_df <- timeseries[, c("Yr", "Era", "Seas", retain_catch_colnames)]
  retain_catch_df <- tidyr::gather(retain_catch_df, 
                                   key = "tmp_units_fleet", 
                                   value = "retained_catch", 
                            grep("^retain\\([BN]\\):_\\d+$", 
                                 colnames(retain_catch_df)))
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  retain_catch_df <- tidyr::separate(retain_catch_df, col = "tmp_units_fleet",
                          into = c("Units", "Fleet"), 
                          sep = ":_",convert = TRUE)
  #units are not as concies as they could be, but leave for now.
}