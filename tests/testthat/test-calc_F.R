context("Test functions in calc_F script")

nfleets <- 2
test_timeseries <- data.frame(
  Area = 1,
  Yr = rep(-1:4, 2),
  Seas = rep(c(1, 2), times = c(6, 6)),
  Era = c("VIRG", "INIT", "TIME", "TIME", "TIME", "FORE"),
  Bio_all = 3910080000,
  Bio_smry = 3908690000,
  SpawnBio = 3878510000,
  Recruit_0 = 133853000,
  `SpawnBio_GP:1` = 3878510000,
  `SmryBio_SX:1_GP:1` = 3908690000,
  `SmryNum_SX:1_GP:1` = 604568000,
  `sel(B):_1` = 0,
  `dead(B):_1` = 0,
  `retain(B):_1` = 100,
  `sel(N):_1` = 0,
  `dead(N):_1` = 1,
  `retain(N):_1` = 1,
  `obs_cat:_1` = 1,
  `F:_1` = 0.05,
  `sel(B):_2` = 0,
  `dead(B):_2` = 0,
  `retain(B):_2` = 200,
  `sel(N):_2` = 0,
  `dead(N):_2` = 0,
  `retain(N):_2` = 2,
  `obs_cat:_2` = 0,
  `F:_2` = c(0.01, 0, rep(0.02, 3), 0, rep(0.07, 5), 0),
  SSB_vir_LH = 1,
  ABC_buffer = NA, stringsAsFactors = FALSE,
  check.names = FALSE
)
test_fleetnames <- c("Fishery", "Survey")

test_that("get_F works with multi yrs, fleets, and seasons", {
  F_list <- get_F(timeseries = test_timeseries, fleetnames = test_fleetnames, fleetnames_all = test_fleetnames)

  expect_true(NROW(F_list[["F_df"]]) == nfleets * NROW(test_timeseries))
  expect_true(length(F_list) == 4) # b/c 4 components of output
  expect_equal(
    F_list[["F_rate"]][["name"]],
    c(
      "F_fleet_1_YR_1_s_1", "F_fleet_1_YR_1_s_2", "F_fleet_1_YR_2_s_1",
      "F_fleet_1_YR_2_s_2", "F_fleet_1_YR_3_s_1", "F_fleet_1_YR_3_s_2",
      "F_fleet_2_YR_1_s_1", "F_fleet_2_YR_1_s_2", "F_fleet_2_YR_2_s_1",
      "F_fleet_2_YR_2_s_2", "F_fleet_2_YR_3_s_1", "F_fleet_2_YR_3_s_2"
    )
  )
  expect_length(F_list[["init_F"]], 3)
  expect_equal(
    names(F_list[["init_F"]]),
    c(
      "InitF_seas_1_flt_1Fishery", "InitF_seas_2_flt_1Fishery",
      "InitF_seas_2_flt_2Survey"
    )
  )
  expect_equal(
    F_list[["F_rate_fcast"]][["name"]],
    c("F_fleet_1_YR_4_s_1", "F_fleet_1_YR_4_s_2")
  )
})

test_that("get_F works with multi yrs, fleets, and seasons but no init F", {
  no_init_F_timeseries <- test_timeseries
  no_init_F_timeseries[no_init_F_timeseries[["Era"]] == "INIT", c("F:_1", "F:_2")] <- 0
  F_list <- get_F(timeseries = no_init_F_timeseries, fleetnames = test_fleetnames, fleetnames_all = test_fleetnames)
  expect_true(NROW(F_list[["F_df"]]) == nfleets * NROW(no_init_F_timeseries))
  expect_true(length(F_list) == 4) # b/c 4 components of output
  expect_equal(
    F_list[["F_rate"]][["name"]],
    c(
      "F_fleet_1_YR_1_s_1", "F_fleet_1_YR_1_s_2", "F_fleet_1_YR_2_s_1",
      "F_fleet_1_YR_2_s_2", "F_fleet_1_YR_3_s_1", "F_fleet_1_YR_3_s_2",
      "F_fleet_2_YR_1_s_1", "F_fleet_2_YR_1_s_2", "F_fleet_2_YR_2_s_1",
      "F_fleet_2_YR_2_s_2", "F_fleet_2_YR_3_s_1", "F_fleet_2_YR_3_s_2"
    )
  )
  expect_null(F_list[["init_F"]])
  expect_equal(
    F_list[["F_rate_fcast"]][["name"]],
    c("F_fleet_1_YR_4_s_1", "F_fleet_1_YR_4_s_2")
  )
})

test_that("get_F works with multi yrs, fleets, and seasons but no F_rate_fcast", {
  no_fcast_F_timeseries <- test_timeseries
  no_fcast_F_timeseries[no_fcast_F_timeseries[["Era"]] == "FORE", c("F:_1", "F:_2")] <- 0
  F_list <- get_F(timeseries = no_fcast_F_timeseries, fleetnames = test_fleetnames, fleetnames_all = test_fleetnames)
  expect_true(NROW(F_list[["F_df"]]) == nfleets * NROW(no_fcast_F_timeseries))
  expect_true(length(F_list) == 4) # b/c 4 components of output
  expect_equal(
    F_list[["F_rate"]][["name"]],
    c(
      "F_fleet_1_YR_1_s_1", "F_fleet_1_YR_1_s_2", "F_fleet_1_YR_2_s_1",
      "F_fleet_1_YR_2_s_2", "F_fleet_1_YR_3_s_1", "F_fleet_1_YR_3_s_2",
      "F_fleet_2_YR_1_s_1", "F_fleet_2_YR_1_s_2", "F_fleet_2_YR_2_s_1",
      "F_fleet_2_YR_2_s_2", "F_fleet_2_YR_3_s_1", "F_fleet_2_YR_3_s_2"
    )
  )
  expect_length(F_list[["init_F"]], 3)
  expect_equal(
    names(F_list[["init_F"]]),
    c(
      "InitF_seas_1_flt_1Fishery", "InitF_seas_2_flt_1Fishery",
      "InitF_seas_2_flt_2Survey"
    )
  )
  expect_null(F_list[["F_rate_fcast"]][["name"]])
})

test_that("get_F can catch bad input", {
  fleetnames_wrong_len <- c(test_fleetnames, "extra_fleetname")
  expect_error(get_F(test_timeseries, fleetnames_wrong_len, fleetnames_wrong_len),
    "is_of_length : fleetnames has length 3, not 2.",
    fixed = TRUE
  )
  expect_error(get_F(as.matrix(test_timeseries), test_fleetname, test_fleetnames),
    "is_data.frame : timeseries is not of class 'data.frame'",
    fixed = TRUE
  )
})

units_of_catch <- c(1, 0)

test_that("get_retained_catch works", {
  ret_catch <- get_retained_catch(test_timeseries,
    units_of_catch = units_of_catch
  )
  expect_type(ret_catch, "list")
  expect_s3_class(ret_catch, "data.frame")
  expect_equal(unique(ret_catch[ret_catch[["Fleet"]] == 1, "Units"]), "retain(B)")
  expect_equal(unique(ret_catch[ret_catch[["Fleet"]] == 2, "Units"]), "retain(N)")
  expect_equivalent(ncol(ret_catch), 6)
  expect_equal(
    colnames(ret_catch),
    c("Yr", "Era", "Seas", "Units", "Fleet", "retained_catch")
  )
})


test_that("get_retained_catch catches bad input", {
  expect_error(
    get_retained_catch(test_timeseries,
      units_of_catch = c(units_of_catch, 3)
    ),
    "is_of_length : units_of_catch has length 3, not 2.",
    fixed = TRUE
  )
  expect_error(
    get_retained_catch(as.matrix(test_timeseries),
      units_of_catch = units_of_catch
    ),
    "is_data.frame : timeseries is not of class 'data.frame'",
    fixed = TRUE
  )
})
