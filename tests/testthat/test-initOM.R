context("Test functions in R script initialize OM")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-initOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("create_OM works", {
  
  create_OM(OM_dir = "cod", 
            SA_dir = file.path(extdat_path, "models", "cod"), 
            overwrite = TRUE, 
            verbose = FALSE)
  expect_true(file.exists(file.path("cod", "starter.ss")))
  start <- r4ss::SS_readstarter(file.path("cod", "starter.ss"))
  expect_equivalent(start$last_estimation_phase, 0)
})