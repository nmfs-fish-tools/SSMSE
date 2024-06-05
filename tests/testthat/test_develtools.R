context("Test functions in develtools.R")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-develtools")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
file.copy(file.path(extdat_path, "models", "cod"),
  temp_path,
  recursive = TRUE
)
orig_mod_dir <- file.path(temp_path, "cod")
# make sure originally is runing from par.
start_orig <- r4ss::SS_readstarter(file.path(orig_mod_dir, "starter.ss"),
  verbose = FALSE
)
start_orig[["init_values_src"]] <- 1 # read inits from ctl instead of par.
r4ss::SS_writestarter(start_orig,
  dir = file.path(orig_mod_dir), overwrite = TRUE,
  verbose = FALSE
)

# mock a bad par file by deleting a line
orig_par <- readLines(file.path(orig_mod_dir, "ss.par"))
writeLines(orig_par, file.path(temp_path, "good_ss.par"))
fcast_line <- grep("^# Fcast_impl_error:$|^# Fcast_recruitments:$", orig_par)
bad_par <- orig_par[-c(fcast_line, fcast_line + 1)]
writeLines(bad_par, file.path(orig_mod_dir, "ss.par"))
new_mod_dir <- file.path(temp_path, "new_mod_dir")

test_that("test_no_par works as expected", {
  skip_on_cran()
  expect_error(
    run_ss_model(orig_mod_dir, "-maxfn 0 -phase 50 -nohess",
      verbose = FALSE
    ),
    "New data file (data.ss_new if using SS3 v3.30.18 or data_echo.ss_new ",
    fixed = TRUE
    )
  expect_error(
    test_no_par(
      orig_mod_dir = orig_mod_dir,
      new_mod_dir = new_mod_dir
    ),
    "Problem with the ss.par file - different number of lines."
  )
  unlink(new_mod_dir, recursive = TRUE)
  # manipulate the fcast file to make some other issue
  fore <- r4ss::SS_readforecast(file.path(orig_mod_dir, "forecast.ss"),
    verbose = FALSE
  )
  fore[["benchmarks"]] <- 3
  r4ss::SS_writeforecast(fore,
    dir = orig_mod_dir,
    verbose = FALSE, overwrite = TRUE
  )
  expect_error(
    test_no_par(
      orig_mod_dir = orig_mod_dir,
      new_mod_dir = new_mod_dir
    ),
    "Problem with model - not ss.par related",
    fixed = TRUE
  )
})
