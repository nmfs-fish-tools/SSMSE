context("Make sure wrapper functions to run SSMSE work")

# # create a temporary location to avoid adding files to the repo.
# temp_path <- file.path(tempdir(), "test-runSSMSE")
# dir.create(temp_path, showWarnings = FALSE)
# wd <- getwd()
# on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)
# 
# extdat_path <- system.file("extdata", package = "SSMSE")
# 
# test_that("run_SSMSE_iter works", {
#   skip_on_cran()
#   new_catch <- run_SSMSE_iter(out_dir = temp_path)
#   expect_true(file.exists(file.path(temp_path, "1", "cod_OM", "data.ss_new")))
#   expect_equivalent(length(new_catch), 3)
# })