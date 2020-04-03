context("Make sure wrapper functions to run SSMSE work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-runSSMSE")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("run_SSMSE_iter runs with an EM", {
  skip_on_travis()
  skip_on_cran()
  catch_add_yrs <- 101:106
  add_yrs <- c(102,105)
  result <- run_SSMSE_iter(OM_name = "cod",
                 MS = "EM",
                 out_dir = temp_path,
                 EM_name = "cod",
                 nyrs = 6,
                 rec_dev_iter = rep(0, times = 3*2), # Nfleets times nyrs_assess
                 impl_error = rep(1, times = 3*2), # Nfleets times nyrs_assess
                 nyrs_assess = 3,
                 dat_str = list(
                   catch = data.frame(year = catch_add_yrs, seas = 1, fleet = 1),
                   CPUE = data.frame(year = add_yrs, seas = 7, index = 2),
                   lencomp = data.frame(Yr = add_yrs, Seas = 1 , FltSvy = 1, 
                                        Gender = 0, Part = 0),
                   agecomp = data.frame(Yr = add_yrs, Seas = 1 , FltSvy = 2,
                                        Gender = 0, Part = 0, Ageerr = 1,
                                        Lbin_lo = -1, Lbin_hi = -1)
                 )
  )
  expect_true(file.exists(file.path(temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(result)
  # some more specific values, specific to the scenario above.
  dat <- SS_readdat(file.path(temp_path, "1", "cod_EM", "data.ss_new"), verbose = FALSE)
  added_catch <- dat$catch[dat$catch$year %in% catch_add_yrs, ]
  old_catch <- dat$catch[dat$catch$year < min(catch_add_yrs), ]
  expect_true(all(added_catch$catch_se == 0.005))
  added_CPUE <- dat$CPUE[dat$CPUE$year >= min(catch_add_yrs), ]
  expect_true(all(add_yrs %in% unique(added_CPUE$year)))
  added_lencomp <- dat$lencomp[dat$lencomp$Yr %in% add_yrs, ]
  expect_true(all(add_yrs %in% unique(added_lencomp$Yr)))
  added_agecomp <- dat$agecomp[dat$agecomp$Yr %in% add_yrs, ]
  expect_true(all(add_yrs %in% unique(added_agecomp$Yr)))
})

test_that("run_SSMSE_iter runs with no EM", {
  skip_on_cran()
  new_temp_path <- file.path(temp_path, "no_EM")
  dir.create(new_temp_path)
  result <- run_SSMSE_iter(OM_name = "cod",
                           MS = "no_catch",
                           out_dir = new_temp_path,
                           nyrs = 6,
                           nyrs_assess = 3
                           )
  expect_true(file.exists(file.path(new_temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(result)
# add more specific tests
})

OM_path_cod <- file.path(extdat_path, "models","cod")
EM_path_cod <- file.path(extdat_path, "models","cod")
test_that("cod works when treated as a custom model", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  new_temp_path <- file.path(temp_path, "custom_cod")
  dir.create(new_temp_path)
  catch_add_yrs <- 101:106
  add_yrs <- c(102,105)
  result <- run_SSMSE_iter(OM_name = NULL,
                           OM_in_dir = OM_path_cod,
                           MS = "EM",
                           out_dir = new_temp_path,
                           EM_name = NULL,
                           EM_in_dir = EM_path_cod,
                           nyrs = 6,
                           nyrs_assess = 3,
                           dat_str = list(
                             catch = data.frame(year = catch_add_yrs, seas = 1, fleet = 1),
                             CPUE = data.frame(year = add_yrs, seas = 7, index = 2),
                             lencomp = data.frame(Yr = add_yrs, Seas = 1 , FltSvy = 1, 
                                                  Gender = 0, Part = 0),
                             agecomp = data.frame(Yr = add_yrs, Seas = 1 , FltSvy = 2,
                                                  Gender = 0, Part = 0, Ageerr = 1,
                                                  Lbin_lo = -1, Lbin_hi = -1)
                           )
  )
  expect_true(file.exists(file.path(new_temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(result)
})
