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
  expect_true(all(catch_list$catch$year %in% 101:103))
  expect_true(all(colnames(catch_list[["catch"]]) == colnames(OM_dat$catch)))
})


test_that("get_no_EM_catch_df works with no discards", {
  # This is an function referenced within the parse)MS function, so only
  # created some simple tests.
  # mock additional fleets of catch data
  catch <- OM_dat$catch
  new_catch <- data.frame(year = 100,
                          seas = 1,
                          fleet = c(2,3),
                          catch = c(200, 300),
                          catch_se = c(0.02, 0.03))
  catch <- rbind(catch, new_catch)
  lyr_catch_df <- get_no_EM_catch_df(OM_dir = cod_OM_path, yrs = 101:105,
                                  MS = "last_yr_catch")
  lyr_catch_df <- lyr_catch_df[["catch"]]
  expect_true(all(lyr_catch_df$catch != 0))
  expect_true(length(unique(lyr_catch_df$fleet)) == length(unique(catch$fleet)))
  expect_true(length(unique(lyr_catch_df$fleet)) == 3)
  no_catch_df <- get_no_EM_catch_df(catch = catch, yrs = 101:105,
                                 MS = "no_catch")
  no_catch_df <- no_catch_df[["catch"]]
  expect_true(all(no_catch_df$catch == 0))
  expect_true(length(unique(no_catch_df$fleet)) == length(unique(catch$fleet)))
  expect_error(get_no_EM_catch_df(catch = catch, yrs = 101:105,
                               MS = "bad_MS_input"))
})

#TODO: add tests for using discards.

test_that("parse_MS works for no estimation model methods", {
  # no catch managemetn strategy
  catch_list_1 <- parse_MS(MS = "no_catch",
                    OM_dat = OM_dat,
                    OM_out_dir = cod_OM_path,
                    nyrs_assess = 3)
  catch_df_1 <- catch_list_1[["catch"]]
  expect_equivalent(catch_df_1$catch, rep(0, times = 3))
  # last year management strategy
  catch_list_2 <- parse_MS(MS = "last_yr_catch",
                    OM_dat = OM_dat,
                    OM_out_dir = cod_OM_path,
                    nyrs_assess = 3)
  catch_df_2 <- catch_list_2[["catch"]]
  lyr_catch <- OM_dat$catch$catch[nrow(OM_dat$catch)]
  expect_true(all(catch_df_2$catch == lyr_catch))
})

test_that("parse_MS works as currently expected for estimation model methods", {
  skip_on_cran()
  # use cod as the EM
  catch_list <- parse_MS(MS = "EM",
                         EM_out_dir = cod_EM_path,
                         OM_dat = OM_dat,
                         OM_out_dir = cod_OM_path,
                         nyrs_assess = 3)
  catch_df_3 <- catch_list[["catch"]]
  expect_true(nrow(catch_df_3) == 3)
  expect_true(ncol(catch_df_3) == 5)
  expect_equivalent(colnames(catch_df_3),
                    c("year", "seas", "fleet", "catch", "catch_se"))
  #TODO: add tests to make sure parse_MS works for future iterations

})

test_that("parse_MS catches errors when it should", {
  # MS is invalid
  expect_error(parse_MS(MS = "bad_option", EM_out_dir = NULL,
           OM_dat = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "MS was input as", fixed = TRUE)
  # invalid EM_dir
  expect_error(parse_MS(MS = "EM", EM_out_dir = "other_fake_dir",
           OM_dat = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "Please change to a directory containing a valid SS model",
           fixed = TRUE)
  #TODO: need to catch invalid OM_dat? What about nyrs_assess?
})
