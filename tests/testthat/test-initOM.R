context("Test functions in R script initialize OM")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-initOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
file.copy(file.path(extdat_path, "models", "cod"),
          temp_path, recursive = TRUE)

test_that("create_OM can add in dummy data and modify model", {
  skip_on_cran() # because runs ss.
  new_dat <- create_OM(OM_out_dir = file.path(temp_path, "cod"),
                       overwrite = TRUE,
                       add_dummy_dat = TRUE,
                       verbose = FALSE,
                       nyrs_assess = 3,
                       rec_devs = rep(0, length = 6),
                       verify_OM = TRUE) # use the runtime check also.
  # note there are 2 NAs introduced by coercion warnings from SS_output that
  # are safe to ignore.
  dat_types <- c("CPUE", "lencomp", "agecomp")
  new_dat_yrs <- lapply(dat_types, function(type, datlist) {
                  unique_yrs <- unique(datlist[[type]][, 1])
                  ordered_yrs <- unique_yrs[order(unique_yrs)]
                  },
                  datlist = new_dat
                  )
  names(new_dat_yrs) <- dat_types
  # check that all years of data were added
  lapply(new_dat_yrs, function(new_yrs, expected_yrs) {
    expect_equivalent(new_yrs, expected_yrs)
    }, expected_yrs = new_dat$styr:new_dat$endyr)
  # following should be true b/c there is only 1 fleet and season
  expect_equal(length(new_dat$CPUE$year), length(new_dat$styr:new_dat$endyr))
  expect_equal(length(new_dat$lencomp$Yr), length(new_dat$styr:new_dat$endyr))
  expect_equal(length(new_dat$agecomp$Yr), length(new_dat$styr:new_dat$endyr))
  # check that no duplicated data
  expect_equal(new_dat$CPUE[, c("year", "seas", "index")],
               unique(new_dat$CPUE[, c("year", "seas", "index")])
               )
  cols_lencomp <- c("Yr", "Seas", "FltSvy", "Gender", "Part")
  expect_equal(new_dat$lencomp[, cols_lencomp],
               unique(new_dat$lencomp[, cols_lencomp])
  )
  cols_agecomp <- c("Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo",
                         "Lbin_hi")
  expect_equal(new_dat$agecomp[, cols_agecomp],
               unique(new_dat$agecomp[, cols_agecomp])
  )
  # check that a valid model is produced by seeing if it can run.
  start <- r4ss::SS_readstarter(file.path(temp_path, "cod", "starter.ss"))
  expect_true(start$init_values_src == 1) # b/c should be running from.par.
  file.remove(file.path(temp_path, "cod", "control.ss_new"))
  run_ss_model(file.path(temp_path, "cod"), "-maxfn 0 -phase 50 -nohess",
               verbose = FALSE)
  expect_true(file.exists(file.path(temp_path, "cod", "control.ss_new")))
})
