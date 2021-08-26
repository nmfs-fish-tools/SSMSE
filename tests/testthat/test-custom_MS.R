context("test that defining a custom Management Strategy will work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-custom_MS")
dir.create(temp_path, showWarnings = FALSE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)
# make this function in the global environment (so use <<- assignment)
run_test_custom_MP <<- function(OM_out_dir, OM_dat, nyrs_assess, MS, ...) {
  new_catch_list <- get_no_EM_catch_df(
    OM_dir = OM_out_dir,
    yrs = (OM_dat[["endyr"]] + 1):(OM_dat[["endyr"]] + nyrs_assess),
    MS = "no_catch"
  )
}
# cleanup
on.exit(rm(run_test_custom_MP, envir = globalenv()), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
cod_OM_mod <- file.path(extdat_path, "test_mod", "cod_initOM_for_tests")

# copy cod to the temp_path
dir.create(file.path(temp_path, "cod_OM"))
dir.create(file.path(temp_path, "cod_EM"))
file.copy(cod_OM_mod, file.path(temp_path, "cod_OM"), recursive = TRUE)
file.copy(cod_mod, file.path(temp_path, "cod_EM"), recursive = TRUE)
cod_OM_path <- file.path(temp_path, "cod_OM", "cod_initOM_for_tests")
cod_EM_path <- file.path(temp_path, "cod_EM", "cod")

OM_dat <- r4ss::SS_readdat(file.path(cod_OM_path, "data.ss"), verbose = FALSE)

test_that("parse_MS works with custom option", {
  catch_list <- tryCatch(parse_MS(
    MS = "run_test_custom_MP",
    EM_out_dir = cod_EM_path,
    OM_dat = OM_dat,
    OM_out_dir = cod_OM_path,
    dat_yrs = 2010,
    nyrs_assess = 3
  ), error = function(e) e)
  catch_df_3 <- catch_list[["catch"]]
  expect_true(nrow(catch_df_3) == 3)
  expect_true(ncol(catch_df_3) == 5)
  expect_equivalent(
    colnames(catch_df_3),
    c("year", "seas", "fleet", "catch", "catch_se")
  )
})
