context("Check that functions in manipulate_EM script work")

#TODO: finish this test.

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-manipulate_EM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
#on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)

test_that("run_EM works", {
  skip_on_cran()
  df <- run_EM(EM_dir = file.path(temp_path, "cod"),  change_fcast = FALSE)
  fore <- r4ss::SS_readforecast(file.path(temp_path, "cod", "forecast.ss"))
  expect_equal(nrow(df), fore$Nforecastyrs)
  
  df2 <- run_EM(EM_dir = file.path(temp_path, "cod"), 
                change_fcast = TRUE, 
                nyrs_proj = 4)
  expect_equivalent(nrow(df2), 4)
  
  df3 <- run_EM(EM_dir = file.path(temp_path, "cod"),
                set_use_par = TRUE)
  expect_equivalent(nrow(df3), 4)
  start <- r4ss::SS_readstarter(file.path(temp_path, "cod", "starter.ss"), 
                                verbose = FALSE)
  expect_equivalent(start$init_values_src, 1)
})

test_that("run_EM exits on error when it should", {
  # specify a directory that is not valid
  expect_error(run_EM(EM_dir = temp_path), 
               "Please change to a directory containing a valid SS model.")
  # use change_fcast = TRUE with no value for nyrs_proj
  expect_error(run_EM(EM_dir = file.path(temp_path, "cod"),
                      change_fcast = TRUE), 
               "argument \"nyrs_proj\" is missing, with no default")
})