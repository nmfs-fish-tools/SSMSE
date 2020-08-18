context("Integration test for interim assessment workflow")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-interim")
dir.create(temp_path, showWarnings = FALSE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("run_SSMSE runs with interim assessment workflow", {
  skip_on_cran()
  nyrs <- 6
  assess_freq <- 10
  datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
  # use sample_struct to get the structure, then modify
  sample_struct <- create_sample_struct(dat = datfile, nyrs = nyrs)
  sample_struct$CPUE <- data.frame(Yr = 101:110, Seas = 7, FltSvy = 2, SE = 0.2)
  sample_struct$agecomp <- NULL
  sample_struct$lencomp <- NULL
  interim_struct_list <- list(MA_years = 3,
                              assess_freq = 10,
                              Beta = c(1, 1),
                              Index_SE = c(0.2, 0.2),
                              Index_weights = c(1, 1),
                              Ref_years = c(0, 0),
                              control = FALSE)
  result <- run_SSMSE(scen_name_vec = "base", # name of the scenario
                      out_dir_scen_vec = temp_path, # directory in which to run the scenario
                      iter_vec = 1, # run with 5 iterations each
                      OM_name_vec = "cod", 
                      EM_name_vec = "cod", # cod is included in package data
                      MS_vec = "Interim",       # The management strategy is specified in the EM
                      use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                      nyrs_vec = nyrs,        # Years to project OM forward
                      nyrs_assess_vec = 1, # Years between assessments
                      rec_dev_pattern = "none", # Don't use recruitment deviations
                      impl_error_pattern = "none", # Don't use implementation error
                      sample_struct_list = list(sample_struct), # How to sample data for running the EM.  
                      interim_struct_list = list(interim_struct_list),
                      seed = 12345) #Set a fixed integer seed that allows replication 
  expect_true(file.exists(file.path(temp_path, "base", "1", "cod_OM",
                                    "data.ss_new")))
  expect_true(file.exists(file.path(temp_path, "base", "1", "cod_EM_init",
                                    "data.ss_new")))
  expect_length(result, 1)
  # could add some better tests to make sure this works.
})
