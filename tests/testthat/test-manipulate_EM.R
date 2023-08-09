context("Check that functions in manipulate_EM script work")

# TODO: finish this test.

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-manipulate_EM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)

test_that("get_EM_dat works", {
  EM_dat <- r4ss::SS_readdat(file.path(cod_mod, "ss3.dat"), verbose = FALSE)
  OM_dat <- EM_dat # for simplicity, mock OM/EM dat.
  rm_ind <- c(1, 2) # index of obs to move
  # manipulate EM_dat so that it has less observations than OM
  EM_dat[["CPUE"]] <- EM_dat[["CPUE"]][-rm_ind, ] # remove the first 2 rows
  EM_dat[["lencomp"]] <- EM_dat[["lencomp"]][-rm_ind, ]
  EM_dat[["agecomp"]] <- EM_dat[["agecomp"]][-rm_ind, ]

  EM_dat[["CPUE"]][["obs"]] <- 999 # to make it easier to identify if the values change
  new_dat <- get_EM_dat(OM_dat, EM_dat, do_checks = FALSE)
  row_names <- c("year", "seas", "index")
  lapply(row_names, function(x) {
    expect_equal(EM_dat[["CPUE"]][x], new_dat[["CPUE"]][x])
  })
  expect_equal(OM_dat[["CPUE"]][["obs"]][-rm_ind], new_dat[["CPUE"]][["obs"]])
  # check length comps changed
  row_names <- c("Yr", "Seas", "FltSvy")
  lapply(row_names, function(x) {
    expect_equal(EM_dat[["lencomp"]][x], new_dat[["lencomp"]][x])
  })
  expect_equal(OM_dat[["lencomp"]][["obs"]][-rm_ind], new_dat[["lencomp"]][["obs"]])
  # chack age comps changed
  row_names <- c("Yr", "Seas", "FltSvy")
  lapply(row_names, function(x) {
    expect_equal(EM_dat[["agecomp"]][x], new_dat[["agecomp"]][x])
  })
  expect_equal(OM_dat[["agecomp"]][["obs"]][-rm_ind], new_dat[["agecomp"]][["obs"]])
})
# add tests for change_dat

test_that("run_EM works", {
  skip_on_cran()
  if (file.exists(file.path(temp_path, "cod", "control.ss_new"))) {
    file.remove(file.path(temp_path, "cod", "control.ss_new"))
  }
  df3 <- run_EM(
    EM_dir = file.path(temp_path, "cod"),
    set_use_par = TRUE
  )
  start <- r4ss::SS_readstarter(file.path(temp_path, "cod", "starter.ss"),
    verbose = FALSE
  )
  expect_equivalent(start[["init_values_src"]], 1)
  expect_true(file.exists(file.path(temp_path, "cod", "control.ss_new")))
})

test_that("run_EM exits on error when it should", {
  # specify a directory that is not valid
  expect_error(
    run_EM(EM_dir = temp_path),
    "Please change to a directory containing a valid SS model."
  )
})

test_that("add_new_dat works", {
  OM_dat <- r4ss::SS_readdat(file.path(cod_mod, "ss3.dat"), verbose = FALSE)
  EM_dat <- OM_dat # for simplicity, mock OM/EM dat.
  EM_dat_orig <- EM_dat # this will be used to create the expectations.
  # Note OM should go beyond EM now.

  # Modify the EM_dat to use in the function, because want to mock the OM having
  # data that the EM does not.
  EM_dat[["endyr"]] <- OM_dat[["endyr"]] - 3
  EM_dat[["catch"]] <- EM_dat[["catch"]][1:98, ]
  # just get rid of last val for the following, because all data are within the
  # range from start year to the new endyr:
  EM_dat[["CPUE"]] <- EM_dat[["CPUE"]][-nrow(EM_dat[["CPUE"]]), ]
  EM_dat[["lencomp"]] <- EM_dat[["lencomp"]][-nrow(EM_dat[["lencomp"]]), ]
  EM_dat[["agecomp"]] <- EM_dat[["agecomp"]][-nrow(EM_dat[["agecomp"]]), ]
  # write the file because the function requires it
  r4ss::SS_writedat(EM_dat, file.path(temp_path, "cod_EM_dat.ss"),
    overwrite = TRUE, verbose = FALSE
  )
  # define sampling (with r4ss names), beyond the last year of EM_dat currently
  # This is basically just what has been excluded from the EM:
  ca <- EM_dat_orig[["catch"]][99:101, 1:3] #
  CP <- EM_dat_orig[["CPUE"]][nrow(EM_dat_orig[["CPUE"]]), 1:3, drop = FALSE]
  LC <- EM_dat_orig[["lencomp"]][nrow(EM_dat_orig[["lencomp"]]), 1:6, drop = FALSE]
  AC <- EM_dat_orig[["agecomp"]][nrow(EM_dat_orig[["agecomp"]]), 1:9, drop = FALSE]
  sample_struct <- list(
    catch = ca,
    CPUE = CP,
    lencomp = LC,
    agecomp = AC
  )
  # this function should produce the sampling with the samples defined in sample
  # struct added in
  new_EM_dat <- add_new_dat(
    OM_dat = OM_dat,
    EM_datfile = "cod_EM_dat.ss",
    sample_struct = sample_struct,
    EM_dir = temp_path,
    nyrs_assess = 3,
    do_checks = TRUE,
    new_datfile_name = NULL,
    verbose = FALSE
  )
  lapply(
    c("catch", "CPUE", "lencomp", "agecomp"),
    function(x) {
      expect_equal(new_EM_dat[[x]], EM_dat_orig[[x]],
        tolerance = 0.9
      )
    }
  )
})


test_that("add_new_dat works when meansize at age obs", {
  skip_if(!file.exists(file.path(
    extdat_path, "models", "Simple_with_Discard",
    "data.ss"
  )))
  OM_dat <- r4ss::SS_readdat(file.path(
    extdat_path, "models",
    "Simple_with_Discard", "data.ss"
  ))
  OM_dat[["meanbodywt"]] <- rbind(OM_dat[["meanbodywt"]], OM_dat[["meanbodywt"]])
  OM_dat[["meanbodywt"]][["Year"]] <- c(1995, 1995, 2000, 2000)
  OM_dat[["MeanSize_at_Age_obs"]] <- rbind(OM_dat[["MeanSize_at_Age_obs"]], OM_dat[["MeanSize_at_Age_obs"]])
  OM_dat[["MeanSize_at_Age_obs"]][["Yr"]] <- c(
    1971, 1995, 1995, 1995, 1971, 1995,
    1997, 1998, 1998, 1998, 1999, 2000
  )
  EM_dat <- OM_dat # for simplicity, mock OM/EM dat.
  EM_dat_orig <- EM_dat # keep the original data
  EM_dat[["endyr"]] <- EM_dat[["endyr"]] - 5 # roll back 5 years

  # get rid of data after the new end yr in EM_dat
  EM_dat[["catch"]] <- EM_dat[["catch"]][1:27, ]
  EM_dat[["CPUE"]] <- EM_dat[["CPUE"]][2:17, ]
  EM_dat[["meanbodywt"]] <- EM_dat[["meanbodywt"]][1:2, ]
  EM_dat[["MeanSize_at_Age_obs"]] <- EM_dat[["MeanSize_at_Age_obs"]][1:6, ]

  # specify the sampling structure
  Ca <- EM_dat_orig[["catch"]][28:32, 1:3]
  CP <- EM_dat_orig[["CPUE"]][c(1, 18:23), 1:3]
  Mean <- EM_dat_orig[["meanbodywt"]][3:4, 1:5]
  SAA <- EM_dat_orig[["MeanSize_at_Age_obs"]][7:12, 1:7]

  sample_struct <- list(
    catch = Ca,
    CPUE = CP,
    meanbodywt = Mean,
    MeanSize_at_Age_obs = SAA
  )
  r4ss::SS_writedat(EM_dat, file.path(temp_path, "simple_EM_dat.ss"),
    overwrite = TRUE, verbose = FALSE
  )
  new_EM_dat <- add_new_dat(
    OM_dat = OM_dat,
    EM_datfile = "simple_EM_dat.ss",
    sample_struct = sample_struct,
    EM_dir = temp_path,
    nyrs_assess = 5,
    do_checks = TRUE,
    new_datfile_name = NULL,
    verbose = FALSE
  )
  lapply(
    c("catch", "CPUE", "lencomp", "agecomp"),
    function(x) {
      expect_equal(new_EM_dat[[x]], EM_dat_orig[[x]],
        tolerance = 0.9
      )
    }
  )
})

test_that("add_new_dat warns as expected", {
  OM_dat <- r4ss::SS_readdat(file.path(cod_mod, "ss3.dat"), verbose = FALSE)
  EM_dat <- OM_dat # for simplicity, mock OM/EM dat.
  # get rid of a few years of data
  catch <- EM_dat[["catch"]][(nrow(EM_dat[["catch"]]) - 2):nrow(EM_dat[["catch"]]), 1:3]
  catch[["year"]][3] <- 200
  CP <- EM_dat[["CPUE"]][nrow(EM_dat[["CPUE"]]), 1:3, drop = FALSE]
  EM_dat[["catch"]] <- EM_dat[["catch"]][-((nrow(EM_dat[["catch"]]) - 2):nrow(EM_dat[["catch"]])), ]
  EM_dat[["CPUE"]] <- EM_dat[["CPUE"]][-nrow(EM_dat[["CPUE"]]), ]
  # the data structure is the
  sample_struct <- list(
    catch = catch,
    CPUE = CP
  )
  r4ss::SS_writedat(EM_dat, file.path(temp_path, "cod_EM_dat.ss"),
    overwrite = TRUE, verbose = FALSE
  )
  expect_warning(new_EM_dat <- add_new_dat(
    OM_dat = OM_dat,
    EM_datfile = "cod_EM_dat.ss",
    sample_struct = sample_struct,
    EM_dir = temp_path,
    nyrs_assess = 0,
    do_checks = TRUE,
    new_datfile_name = NULL,
    verbose = FALSE
  ), "Some values specified")
})

fore <- r4ss::SS_readforecast(
  file.path(cod_mod, "forecast.ss"),
  verbose = FALSE
)
styr <- 1
endyr <- 100

test_that("change_yrs_fcast works without allocation", {
  new_fore <- change_yrs_fcast(fore,
    make_yrs_rel = TRUE,
    nyrs_fore = 15,
    mod_styr = styr,
    mod_endyr = endyr
  )
  expect_true(all(new_fore[["Bmark_years"]] <= 0))
  expect_true(all(new_fore[["Fcast_years"]] <= 0))
  expect_true(new_fore[["Nforecastyrs"]] == 15)
  nyrs_inc <- 3
  new_fore <- change_yrs_fcast(fore,
    make_yrs_rel = FALSE,
    nyrs_increment = nyrs_inc,
    mod_styr = 1,
    mod_endyr = 100
  )
  expect_true(all(new_fore[["Bmark_years"]] == fore[["Bmark_years"]]))
  expect_true(all(new_fore[["Fcast_years"]] == fore[["Fcast_years"]]))
  expect_true(new_fore[["FirstYear_for_caps_and_allocations"]] > (endyr + nyrs_inc))
  expect_true(new_fore[["FirstYear_for_caps_and_allocations"]] ==
    (fore[["FirstYear_for_caps_and_allocations"]] + nyrs_inc))
})
test_that("change_yrs_fcast works/errors with allocation as expected", {
  # mock a forecast file with allocation. assume has 3 fleets with catch.
  fore[["N_allocation_groups"]] <- 3
  fore[["fleet_assignment_to_allocation_group"]] <- data.frame(
    Fleet = 1:3,
    Group = c(1, 1, 2)
  )
  fore[["allocation_among_groups"]] <-
    data.frame(Year = endyr + 1, `Group 1` = 0.25, `Group 2` = 0.75)
  new_fore <- change_yrs_fcast(fore,
    make_yrs_rel = TRUE,
    nyrs_fore = 15,
    mod_styr = styr,
    mod_endyr = endyr
  )
  expect_true(all(new_fore[["Bmark_years"]] <= 0))
  expect_true(all(new_fore[["Fcast_years"]] <= 0))
  expect_true(new_fore[["Nforecastyrs"]] == 15)
  nyrs_inc <- 3
  endyr <- endyr + nyrs_inc
  new_fore <- change_yrs_fcast(fore,
    make_yrs_rel = FALSE,
    nyrs_increment = nyrs_inc,
    mod_styr = styr,
    mod_endyr = endyr
  )
  expect_true(all(new_fore[["Bmark_years"]] == fore[["Bmark_years"]]))
  expect_true(all(new_fore[["Fcast_years"]] == fore[["Fcast_years"]]))
  expect_true(new_fore[["FirstYear_for_caps_and_allocations"]] > endyr)
  expect_true(new_fore[["FirstYear_for_caps_and_allocations"]] ==
    (fore[["FirstYear_for_caps_and_allocations"]] + nyrs_inc))
  expect_true(new_fore[["allocation_among_groups"]][["Year"]] == endyr + 1)
  # change to have time varying allocation (which SSMSE cannot handle currently)
  fore[["allocation_among_groups"]] <- data.frame(
    Year = c(endyr - 3, endyr - 2),
    `Group 1` = c(0.25, 0.75),
    `Group 2` = c(0.75, 0.25)
  )
  expect_error(
    change_yrs_fcast(fore,
      make_yrs_rel = FALSE,
      nyrs_increment = nyrs_inc,
      mod_styr = styr,
      mod_endyr = endyr
    ),
    "Time-varying allocation in the forecasting file cannot yet be used",
    fixed = TRUE
  )
  fore[["ForeCatch"]] <- data.frame(
    Year = 101:102, Seas = 1, Fleet = 1:2,
    Catch = c(200, 300)
  )
  expect_warning(new_fore <- change_yrs_fcast(fore,
    make_yrs_rel = FALSE,
    nyrs_increment = NULL,
    mod_styr = styr,
    mod_endyr = endyr
  ))
  expect_true(is.null(new_fore[["ForeCatch"]]))
})

test_that("change_yrs_fcast errors as expected", {
  # pretend that model enyr only goes to 20, so some years are out of range.
  endyr <- 20
  expect_error(
    change_yrs_fcast(fore,
      make_yrs_rel = TRUE,
      nyrs_fore = 15,
      mod_styr = styr,
      mod_endyr = endyr
    ),
    "Year in fcast file out of range."
  )
})

# TODO: write this test
# test_that("add_new_dat fails as expected", {
# })
