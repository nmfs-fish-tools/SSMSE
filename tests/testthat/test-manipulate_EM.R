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

test_that("get_EM_dat works", {
  EM_dat <- r4ss::SS_readdat(file.path(cod_mod, "ss3.dat"))
  OM_dat <- EM_dat #for simplicity, mock OM/EM dat.
  rm_ind <- c(1,2) #index of obs to move
  #manipulate EM_dat so that it has less observations than OM
  EM_dat$CPUE <- EM_dat$CPUE[-rm_ind, ] #remove the first 2 rows
  EM_dat$lencomp <- EM_dat$lencomp[-rm_ind, ]
  EM_dat$agecomp <- EM_dat$agecomp[-rm_ind, ]
  
  EM_dat$CPUE$obs <- 999 #to make it easier to identify if the values change4
  new_dat <- get_EM_dat(OM_dat, EM_dat,  do_checks = FALSE)
  row_names <- c("year", "seas", "index")
  lapply(row_names, function(x) {
    expect_equal(EM_dat$CPUE[x], new_dat$CPUE[x])
  })
  expect_equal(OM_dat$CPUE$obs[-rm_ind], new_dat$CPUE$obs)
  # check length comps changed
  row_names <- c("Yr", "Seas", "FltSvy")
  lapply(row_names, function(x) {
    expect_equal(EM_dat$lencomp[x], new_dat$lencomp[x])
  })
  expect_equal(OM_dat$lencomp$obs[-rm_ind], new_dat$lencomp$obs)
  # chack age comps changed
  row_names <- c("Yr", "Seas", "FltSvy")
  lapply(row_names, function(x) {
    expect_equal(EM_dat$agecomp[x], new_dat$agecomp[x])
  })
  expect_equal(OM_dat$agecomp$obs[-rm_ind], new_dat$agecomp$obs)
})
#add tests for change_dat
# 
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