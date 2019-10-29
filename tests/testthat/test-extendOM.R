context("Test functions in extendOM.R script")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-extendOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
dat <- r4ss::SS_readdat(file.path(temp_path, "cod", "ss3.dat"))
# create a catch dataframe to add to add to the model
# just repeat the catch and se from the last year.
new_catch <- data.frame(
              year  = (dat$endyr+1):(dat$endyr+3),
              seas  = unique(dat$catch$seas)[1],
              fleet = unique(dat$catch$fleet)[1],
              catch = dat$catch$catch[nrow(dat$catch)],
              catch_se = dat$catch$catch_se[nrow(dat$catch)]
             )
test_that("extend_OM works", {
  return_catch <- extend_OM(catch = new_catch, 
                            OM_dir = file.path(temp_path, "cod"), 
                            nyrs = 3,
                            verbose = FALSE)
  expect_equal(new_catch, return_catch)
  new_dat <- r4ss::SS_readdat(file.path(temp_path, "cod", "ss3.dat"))
  new_rows <- new_dat$catch[
                (nrow(new_dat$catch)-nrow(new_catch)+1):nrow(new_dat$catch), ]
  lapply(colnames(new_catch), function(x) {
    expect_equal(new_rows[, x], new_catch[, x])
    }
  )
  #In future, may want to test that dummy data adds correctly.
})

test_that("extend_OM exits on error when it should", {
  # nyrs is too small (needs to be at least 3)
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(catch = new_catch, 
                         OM_dir = file.path(temp_path, "cod"),
                         nyrs = 1, 
                         verbose = FALSE), 
               "The maximum year input for catch")
  # Missing a column in the catch dataframe
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch[, -1], OM_dir = file.path(temp_path, "cod"), 
                         nyrs = 3),
               "The catch data frame does not have the correct")
  # wrong column in the catch dataframe
  alt_new_catch <- new_catch
  colnames(alt_new_catch)[1] <- "wrongname"
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(alt_new_catch, OM_dir = file.path(temp_path, "cod"), 
                         nyrs = 3),
               "The catch data frame does not have the correct")
  #path does not lead to model
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch, OM_dir = temp_path, nyrs = 3),
               "Please change to a directory containing a valid SS model")
})

test_that("check_future_catch works", {
  # doesn't flag anything
  return_catch <- check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"))
  expect_equal(return_catch, new_catch)
  summary <- r4ss::SS_read_summary(file.path(temp_path, "cod", "ss_summary.sso"))
  summary <- summary$biomass
  large_catch <- new_catch
  large_catch_val <- summary["TotBio_101", "Value"] + 1000
  large_catch[2,"catch"] <- large_catch_val
  expect_error(check_future_catch(catch = large_catch, 
                                  OM_dir = file.path(temp_path, "cod")), 
               "Some input values for future catch are higher")
  # catch has the wrong year
  wrong_yr_catch <- new_catch
  wrong_yr_catch[2,"year"] <- 5
  expect_error(check_future_catch(catch = wrong_yr_catch, 
                                  OM_dir = file.path(temp_path, "cod")), 
               "The highest year for which TotBio")
  # function cannot be used for other catch_units besides "bio"
  expect_error(check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"),
                                  catch_units = "num"), 
               "Function not yet implemented when catch is not in biomass")
  expect_error(check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"),
                                  catch_units = "wrong_value"), 
               "Function not yet implemented when catch is not in biomass")
})