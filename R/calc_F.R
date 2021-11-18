# functions to calculate F.

#' Get the Fishing mortality from the timeseries Report.sso table
#'
#' @template timeseries
#' @param fleetnames A vector of fleet names, in the order they appear in the
#'  ss model.
#' @importFrom tidyr gather separate
#' @return a list containing: F_df, a long dataframe with F by Yr, Era, Seas,
#'  and fleet; F_rate, a data frame with F for the time frame of the model
#'  only by Yr, Seas, and Fleet, ordered as the ss.par file expects; init_F,
#'  a named vector of initial F values by Season and Fleet, ordered (and named)
#'  as SS expects; and F_rate_fcast, a dataframe of forecasted F by Yr, Seas,
#'  and fleet, ordered as SS would expect in F_rate.
get_F <- function(timeseries, fleetnames) {
  assertive.types::assert_is_data.frame(timeseries)
  # find the F columns
  F_col_ind <- grep("^F:_\\d+$", colnames(timeseries))
  # Note that Area does not need to be included b/c fleets can only operate in
  # add check that fleet name is the correct length
  nfleets <- length(F_col_ind)
  assertive.properties::assert_is_of_length(fleetnames, nfleets)
  # 1 area
  other_col_ind <- which(colnames(timeseries) %in% c("Yr", "Era", "Seas"))
  # Note: pivot_longer is a newer alternative, but it is not yet stable, so
  # the "retired" function gather was used
  # may also be able to use stats::reshape or aggregate here.
  # create a long F data frame so columns are Yr, Era, Seas, Fleet, F
  F_df <- timeseries[, c(other_col_ind, F_col_ind)]
  # change from log to wide formate
  F_df <- tidyr::gather(F_df,
    key = "tmp_Fleet", value = "F",
    grep("^F:_\\d+$", colnames(F_df))
  )
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  F_df <- tidyr::separate(F_df,
    col = "tmp_Fleet", into = c(NA, "Fleet"),
    sep = ":_", convert = TRUE
  )

  # get the F_rate, if any, by finding only the values during the model period
  # and that are greater than 0 (need to make sure there is retained catch?)
  F_rate <- F_df[
    F_df[["F"]] > 0 & F_df[["Era"]] == "TIME",
    setdiff(colnames(F_df), c("Era"))
  ]
  # the following should work, but this sanity check added to avoid assigning
  # the wrong column names. May not work if order of df col changes.
  if (all(colnames(F_rate) == c("Yr", "Seas", "Fleet", "F"))) {
    colnames(F_rate) <- c("year", "seas", "fleet", "F")
  } else {
    stop("Column names not in the correct order.")
  }
  # Make sure that the df is ordered correctly;
  # verified F rate order by running a multiseason and multifleet model and
  # looking at order of F_rate in the PARAMETERS section of the report file.
  F_rate <- F_rate[order(F_rate[, "fleet"], F_rate[, "year"], F_rate[, "seas"]), ]
  # add a name col that is the same as naming in the Report.sso
  F_rate[["name"]] <- paste0(
    "F_fleet_", F_rate[["fleet"]], "_YR_", F_rate[["year"]], "_s_",
    F_rate[["seas"]]
  )
  if (nrow(F_rate) == 0) {
    F_rate <- NULL
  }
  # form init_F
  # Report.sso PARAMETERS implies there can be 1 init F per fleet and season
  # (if there is initial catch for that fleet and season)
  init_F <- F_df[F_df[["F"]] > 0 & F_df[["Era"]] == "INIT", c("Seas", "Fleet", "F")]
  if (nrow(init_F) == 0) {
    init_F <- NULL
  } else {
    # feed back as a named vector sorted by fleet, then season. Names are the
    # same as in the PARAMETERS section of report.sso
    init_F <- init_F[order(init_F[, "Fleet"], init_F[, "Seas"]), ]
    fleetnames_df <- data.frame(
      Fleet = seq_along(fleetnames),
      fleetname = fleetnames
    )
    init_F <- merge(init_F, fleetnames_df)
    init_F_names <- paste0(
      "InitF_seas_", init_F[["Seas"]], "_flt_", init_F[["Fleet"]],
      init_F[["fleetname"]]
    )
    init_F <- init_F[, "F", drop = TRUE]
    names(init_F) <- init_F_names
  }
  # get the F_rate_fcast, if any, by finding only the values during the model period
  # and that are greater than 0 (need to make sure there is retained catch?)
  F_rate_fcast <- F_df[
    F_df[["F"]] > 0 & F_df[["Era"]] == "FORE",
    setdiff(colnames(F_df), c("Era"))
  ]
  # the following should work, but this sanity check added to avoid assigning
  # the wrong column names. May not work if order of df col changes.
  if (all(colnames(F_rate_fcast) == c("Yr", "Seas", "Fleet", "F"))) {
    colnames(F_rate_fcast) <- c("year", "seas", "fleet", "F")
  } else {
    stop("Column names not in the correct order.")
  }

  if (nrow(F_rate_fcast) == 0) {
    F_rate_fcast <- NULL
  } else {
    # Make sure that the df is ordered correctly;
    # verified F rate order by running a multiseason and multifleet model and
    # looking at order of F_rate in the PARAMETERS section of the report file.
    F_rate_fcast <- F_rate_fcast[order(
      F_rate_fcast[, "fleet"],
      F_rate_fcast[, "year"],
      F_rate_fcast[, "seas"]
    ), ]
    # add a name col that is the same as naming in the Report.sso
    F_rate_fcast[["name"]] <- paste0(
      "F_fleet_", F_rate_fcast[["fleet"]],
      "_YR_", F_rate_fcast[["year"]],
      "_s_", F_rate_fcast[["seas"]]
    )
  }

  F_list <- list(
    F_df = F_df, F_rate = F_rate, init_F = init_F,
    F_rate_fcast = F_rate_fcast
  )
}

#' Get retained catch from the timeseries Report.sso table
#'
#' @template timeseries
#' @param units_of_catch From datalist, the catch units. A named list where the
#' names are the fleets (to provide an extra check)
#' @importFrom tidyr gather separate
#' @return a data frame with retained catch by Yr, Era, Seas, Fleet, and
#'  units (long format)
get_retained_catch <- function(timeseries, units_of_catch) {
  # input checks
  assertive.types::assert_is_data.frame(timeseries)
  nfleets <- length(grep("^F:_\\d+$", colnames(timeseries)))
  assertive.properties::assert_is_of_length(units_of_catch, nfleets)
  fleet_names <- strsplit(grep("^F:_\\d+$", colnames(timeseries), value = TRUE),
    "_",
    fixed = TRUE
  )
  fleet_names <- unlist(lapply(fleet_names, function(x) x[2]))
  if (!is.null(names(units_of_catch))) {
    assertive.base::assert_all_are_true(fleet_names == names(units_of_catch))
  }

  # calc retained catch
  units_catch_string <- ifelse(units_of_catch == 1, "B", "N")
  retain_catch_colnames <- paste0(
    "retain(", units_catch_string, "):_",
    fleet_names
  )
  # some may not be included in output if no catch.
  retain_catch_colnames <- retain_catch_colnames[
    retain_catch_colnames %in% colnames(timeseries)
  ]

  # switch from wide to long format.
  retain_catch_df <- timeseries[, c("Yr", "Era", "Seas", retain_catch_colnames)]
  retain_catch_df <- tidyr::gather(retain_catch_df,
    key = "tmp_units_fleet",
    value = "retained_catch",
    grep(
      "^retain\\([BN]\\):_\\d+$",
      colnames(retain_catch_df)
    )
  )
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  retain_catch_df <- tidyr::separate(retain_catch_df,
    col = "tmp_units_fleet",
    into = c("Units", "Fleet"),
    sep = ":_", convert = TRUE
  )

  retain_catch_df <- retain_catch_df %>%
    dplyr::group_by(.data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]]) %>%
    dplyr::summarise(retained_catch = sum(.data[["retained_catch"]])) %>%
    dplyr::select(.data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]], .data[["retained_catch"]])
  retain_catch_df <- as.data.frame(retain_catch_df) # want as df and not tibble
  # units are not as concise as they could be, but leave for now.
  retain_catch_df
}

#' Get dead catch from the timeseries Report.sso table
#'
#' @template timeseries
#' @param units_of_catch From datalist, the catch units. A named list where the
#' names are the fleets (to provide an extra check)
#' @importFrom tidyr gather separate
#' @return a data frame with retained catch by Yr, Era, Seas, Fleet, and
#'  units (long format)
get_dead_catch <- function(timeseries, units_of_catch) {
  # input checks
  assertive.types::assert_is_data.frame(timeseries)
  nfleets <- length(grep("^F:_\\d+$", colnames(timeseries)))
  assertive.properties::assert_is_of_length(units_of_catch, nfleets)
  fleet_names <- strsplit(grep("^F:_\\d+$", colnames(timeseries), value = TRUE),
    "_",
    fixed = TRUE
  )
  fleet_names <- unlist(lapply(fleet_names, function(x) x[2]))
  if (!is.null(names(units_of_catch))) {
    assertive.base::assert_all_are_true(fleet_names == names(units_of_catch))
  }

  # calc dead catch
  units_catch_string <- ifelse(units_of_catch == 1, "B", "N")
  dead_catch_colnames <- paste0(
    "dead(", units_catch_string, "):_",
    fleet_names
  )
  # some may not be included in output if no catch.
  dead_catch_colnames <- dead_catch_colnames[
    dead_catch_colnames %in% colnames(timeseries)
  ]

  # switch from wide to long format.
  dead_catch_df <- timeseries[, c("Yr", "Era", "Seas", dead_catch_colnames)]
  dead_catch_df <- tidyr::gather(dead_catch_df,
    key = "tmp_units_fleet",
    value = "retained_catch",
    grep(
      "^dead\\([BN]\\):_\\d+$",
      colnames(dead_catch_df)
    )
  )
  # make the fleet column just the numerical values. could maybe to with
  # strsplit instead?
  dead_catch_df <- tidyr::separate(dead_catch_df,
    col = "tmp_units_fleet",
    into = c("Units", "Fleet"),
    sep = ":_", convert = TRUE
  )
  dead_catch_df <- dead_catch_df %>%
    dplyr::group_by(.data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]]) %>%
    dplyr::summarise(retained_catch = sum(.data[["retained_catch"]])) %>%
    dplyr::select(
      .data[["Yr"]], .data[["Era"]], .data[["Seas"]], .data[["Units"]], .data[["Fleet"]],
      .data[["retained_catch"]]
    )
  dead_catch_df <- as.data.frame(dead_catch_df)
  # units are not as concise as they could be, but leave for now.
}
