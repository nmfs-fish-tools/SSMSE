context("test that functions in parse_MS work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-parse_MS")
dir.create(temp_path, showWarnings = FALSE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
cod_OM_mod <- file.path(extdat_path, "test_mod", "cod_initOM_for_tests")

# copy cod to the temp_path
dir.create(file.path(temp_path, "cod_OM"))
dir.create(file.path(temp_path, "cod_EM"))
file.copy(cod_OM_mod, file.path(temp_path, "cod_OM"), recursive = TRUE)
file.copy(cod_mod, file.path(temp_path, "cod_EM"), recursive = TRUE)
cod_OM_path <- file.path(temp_path, "cod_OM", "cod_initOM_for_tests")
cod_EM_path <- file.path(temp_path, "cod_EM", "cod")

OM_dat <- r4ss::SS_readdat(file.path(cod_OM_path, "data.ss"), verbose = FALSE)

test_that("get_EM_catch_df works with no discards", {
  # This is an function referenced within the parse)MS function, so only
  # created a simple test.
  catch_list <- get_EM_catch_df(cod_EM_path, dat = OM_dat)
  expect_true(is.data.frame(catch_list[["catch"]]))
  expect_true(all(catch_list[["catch"]][["year"]] %in% 101:103))
  expect_true(all(colnames(catch_list[["catch"]]) == colnames(OM_dat[["catch"]])))
  expect_true(is.null(catch_list[["discards"]]))
  expect_equivalent(catch_list[["catch"]], catch_list[["catch_bio"]])
  expect_true(all(catch_list[["catch_F"]][["year"]] %in% 101:103))
  # note: the following check would need to change if the report file does
  # F forecast could change if this model is rerun.
  expect_true(all(round(catch_list[["catch_F"]][["catch"]], digits = 3) ==
    round(0.100968, digits = 3)))
})


test_that("get_no_EM_catch_df works with no discards", {
  skip_on_cran() # because has to run SS
  # This is an function referenced within the parse_MS function, so only
  # created some simple tests.
  # mock additional fleets of catch data
  catch <- OM_dat[["catch"]]
  lyr_catch_list <- get_no_EM_catch_df(
    OM_dir = cod_OM_path, yrs = 101:105,
    MS = "last_yr_catch"
  )
  lyr_catch_df <- lyr_catch_list[["catch"]]
  expect_true(all(lyr_catch_df[["catch"]] != 0))
  expect_true(length(unique(lyr_catch_df[["fleet"]])) == length(unique(catch[["fleet"]])))

  no_catch_list <- get_no_EM_catch_df(
    OM_dir = cod_OM_path, yrs = 101:105,
    MS = "no_catch"
  )
  no_catch_list[["discards"]] <- NULL # get rid of discards list component
  lapply(no_catch_list, function(x) expect_true(all(x[["catch"]] == 0)))
  no_catch_df <- no_catch_list[["catch"]]
  expect_true(length(unique(no_catch_df[["fleet"]])) == length(unique(catch[["fleet"]])))
  expect_error(get_no_EM_catch_df(
    OM_dir = cod_OM_path, yrs = 101:105,
    MS = "bad_MS_input"
  ))
})

# TODO: add tests for using discards.
# copy cod to the temp_path
unlink(file.path(temp_path, "cod_OM"), recursive = TRUE)
unlink(file.path(temp_path, "cod_EM"), recursive = TRUE)
dir.create(file.path(temp_path, "cod_OM"))
dir.create(file.path(temp_path, "cod_EM"))
file.copy(cod_OM_mod, file.path(temp_path, "cod_OM"), recursive = TRUE)
file.copy(cod_mod, file.path(temp_path, "cod_EM"), recursive = TRUE)
cod_OM_path <- file.path(temp_path, "cod_OM", "cod_initOM_for_tests")
cod_EM_path <- file.path(temp_path, "cod_EM", "cod")

OM_dat <- r4ss::SS_readdat(file.path(cod_OM_path, "data.ss"), verbose = FALSE)
test_that("parse_MS works for no estimation model methods", {
  skip_on_cran()
  # no catch management strategy
  catch_list_1 <- parse_MS(
    MS = "no_catch",
    OM_dat = OM_dat,
    OM_out_dir = cod_OM_path,
    dat_yrs = 101:103,
    nyrs_assess = 3
  )
  catch_df_1 <- catch_list_1[["catch"]]
  expect_equivalent(catch_df_1[["catch"]], rep(0, times = 3))
  # last year management strategy
  catch_list_2 <- parse_MS(
    MS = "last_yr_catch",
    OM_dat = OM_dat,
    OM_out_dir = cod_OM_path,
    dat_yrs = 101:103,
    nyrs_assess = 3
  )
  catch_df_2 <- catch_list_2[["catch"]]
  lyr_catch <- OM_dat[["catch"]][["catch"]][nrow(OM_dat[["catch"]])]
  expect_true(all(catch_df_2[["catch"]] == lyr_catch))
})

test_that("parse_MS works as currently expected for estimation model methods", {
  skip_on_cran()
  # use cod as the EM
  catch_list <- parse_MS(
    MS = "EM",
    EM_out_dir = cod_EM_path,
    OM_dat = OM_dat,
    OM_out_dir = cod_OM_path,
    dat_yrs = 101:103,
    nyrs_assess = 3
  )
  catch_df_3 <- catch_list[["catch"]]
  expect_true(nrow(catch_df_3) == 3)
  expect_true(ncol(catch_df_3) == 5)
  expect_equivalent(
    colnames(catch_df_3),
    c("year", "seas", "fleet", "catch", "catch_se")
  )
  # TODO: add tests to make sure parse_MS works for future iterations
})

test_that("parse_MS catches errors when it should", {
  # MS is invalid
  expect_error(
    parse_MS(
      MS = "bad_option", EM_out_dir = NULL,
      OM_dat = "fake_2",
      verbose = FALSE, dat_yrs = 101:103,
      nyrs_assess = 3
    ),
    "Invalid management strategy",
    fixed = TRUE
  )
  # invalid EM_dir
  expect_error(
    parse_MS(
      MS = "EM", EM_out_dir = "other_fake_dir",
      OM_dat = "fake_2",
      verbose = FALSE, dat_yrs = 101:103,
      nyrs_assess = 3
    ),
    "Please change to a directory containing a valid SS3 model",
    fixed = TRUE
  )
  # TODO: need to catch invalid OM_dat? What about nyrs_assess?
})
