context("Make sure wrapper functions to run SSMSE work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-runSSMSE")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("run_SSMSE_iter works", {
  skip_on_cran()
  result <- run_SSMSE_iter(OM_name = "cod",
                 MS = "EM",
                 out_dir = temp_path,
                 EM_name = "cod",
                 nyrs = 6,
                 nyrs_assess = 3,
                 dat_str = list(
                   catch = data.frame(year = 101:106, seas = 1, fleet = 1),
                   CPUE = data.frame(year = c(102, 105), seas = 7, index = 2)
                 )
  )
  expect_true(file.exists(file.path(temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(result)
  browser()
})