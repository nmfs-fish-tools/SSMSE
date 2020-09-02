context("Test functions in extendOM.R script")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-extendOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "test_mod", "cod_initOM_for_tests")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
dat <- r4ss::SS_readdat(file.path(temp_path, "cod_initOM_for_tests", "data.ss"),
                        verbose = FALSE)
file.rename(file.path(temp_path, "cod_initOM_for_tests"),
            file.path(temp_path, "cod_initOM1"))


# create a catch dataframe to add to add to the model
# just repeat the catch and se from the last year.
new_catch <- data.frame(
              year = (dat$endyr + 1):(dat$endyr + 3),
              seas = unique(dat$catch$seas)[1],
              fleet = unique(dat$catch$fleet)[1],
              catch = dat$catch$catch[nrow(dat$catch)],
              catch_se = dat$catch$catch_se[nrow(dat$catch)]
             )

new_yrs <- new_catch$year

# create a dataframe here.
extend_vals <- list(
                 CPUE = data.frame(year = c(101, 103, 105), seas = 7, index = 2, 
                                   se_log = c(0.1, 0.2, 0.3)),
                 lencomp = data.frame(Yr = 101:103, Seas = 1, FltSvy = 1, 
                                      Gender = 0, Part = 0, 
                                      Nsamp = c(25, 50, 100)),
                 agecomp = data.frame(Yr = 101:104, Seas = 1, FltSvy = 2,
                                      Gender = 0, Part = 0, Ageerr = 1, 
                                      Lbin_lo = -1, Lbin_hi = -1,
                                      Nsamp = c(25, 50, 100, 150))
)

test_that("extend_OM works with simple case", {
  skip_on_cran()
  # simple case: 1 fleet and season needs CPUE, lencomp, agecomp added
  return_dat <- extend_OM(catch = new_catch,
                            discards = NULL,
                            OM_dir = file.path(temp_path, "cod_initOM1"),
                            sample_struct = extend_vals,
                            nyrs_extend = 3,
                            rec_devs = rep(0, 4),
                            impl_error = rep(1, 4),
                            write_dat = FALSE,
                            verbose = FALSE)
  # check catch
  new_rows <- return_dat$catch[
                (nrow(return_dat$catch) - nrow(new_catch) + 1):nrow(return_dat$catch), ]
  lapply(colnames(new_catch), function(x) {
    expect_equal(new_rows[, x], new_catch[, x])
  })
  # check CPUE # wrap first exp. in abs() b/c fleet negative in OM as a switch.
  expect_equivalent(
    abs(return_dat$CPUE[101:102, c("year", "seas", "index", "se_log")]),
    extend_vals$CPUE[extend_vals$CPUE$year <= return_dat$endyr, ])
  # check lencomp
  expect_equivalent(
    abs(return_dat$lencomp[101:103, colnames(extend_vals$lencomp)]),
    extend_vals$lencomp[extend_vals$lencomp$Yr <= return_dat$endyr, ])
  # check agecomp
  expect_equivalent( # wrap both exp. in abs b/c of neg in fleet and in lbin/lbinhi
    abs(return_dat$agecomp[101:103, colnames(extend_vals$agecomp)]),
    abs(extend_vals$agecomp[extend_vals$agecomp$Yr <= return_dat$endyr, ]))
})

# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
dat <- r4ss::SS_readdat(file.path(temp_path, "cod_initOM_for_tests", "data.ss"),
                        verbose = FALSE)
file.rename(file.path(temp_path, "cod_initOM_for_tests"),
            file.path(temp_path, "cod_initOM2"))

test_that("extend_OM exits on error when it should", {
  skip_on_cran()
  # nyrs is too small (needs to be at least 3)
  expect_error(extend_OM(catch = new_catch,
                         OM_dir = file.path(temp_path, "cod_initOM2"),
                         nyrs = 1,
                         verbose = FALSE),
               "The maximum year input for catch")
  # Missing a column in the catch dataframe
  unlink(file.path(temp_path, "cod_initOM2"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch[, -1], OM_dir = file.path(temp_path, "cod_initOM2"),
                         nyrs = 3),
               "The catch data frame does not have the correct")
  # wrong column in the catch dataframe
  alt_new_catch <- new_catch
  colnames(alt_new_catch)[1] <- "wrongname"
  unlink(file.path(temp_path, "cod_initOM2"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(alt_new_catch, OM_dir = file.path(temp_path, "cod_initOM2"),
                         nyrs = 3),
               "The catch data frame does not have the correct")
  # path does not lead to model
  unlink(file.path(temp_path, "cod_initOM2"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch, OM_dir = temp_path, nyrs = 3),
               "Please change to a directory containing a valid SS model")
})

# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
dat <- r4ss::SS_readdat(file.path(temp_path, "cod_initOM_for_tests", "data.ss"),
                        verbose = FALSE)
file.rename(file.path(temp_path, "cod_initOM_for_tests"),
            file.path(temp_path, "cod_initOM3"))

test_that("check_future_catch works", {
  # doesn't flag anything
  return_catch <- check_future_catch(catch = new_catch,
                                  OM_dir = file.path(temp_path, "cod_initOM3"))
  expect_equal(return_catch, new_catch)
  summary <- r4ss::SS_read_summary(file.path(temp_path, "cod_initOM3", "ss_summary.sso"))
  summary <- summary$biomass
  large_catch <- new_catch
  large_catch_val <- summary["TotBio_100", "Value"] + 1000
  large_catch[2, "catch"] <- large_catch_val
  expect_error(check_future_catch(catch = large_catch,
                                  OM_dir = file.path(temp_path, "cod_initOM3")),
               "Some input values for future catch are higher")
  # catch has the wrong year
  wrong_yr_catch <- new_catch
  wrong_yr_catch[2, "year"] <- 5
  expect_error(check_future_catch(catch = wrong_yr_catch,
                                  OM_dir = file.path(temp_path, "cod_initOM3")),
               "The highest year for which TotBio")
  # function cannot be used for other catch_units besides "bio"
  expect_error(check_future_catch(catch = new_catch,
                                  OM_dir = file.path(temp_path, "cod_initOM3"),
                                  catch_units = "num"),
               "Function not yet implemented when catch is not in biomass")
  expect_error(check_future_catch(catch = new_catch,
                                  OM_dir = file.path(temp_path, "cod_initOM3"),
                                  catch_units = "wrong_value"),
               "Function not yet implemented when catch is not in biomass")
})
