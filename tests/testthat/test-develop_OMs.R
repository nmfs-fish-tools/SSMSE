context("Develop OMs")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-develop_OMs")
dir.create(temp_path, showWarnings = FALSE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)


test_that("develop_OMs works as expected", {
  skip_on_cran() # because runs with no est
  develop_OMs(
    OM_name = "cod",
    out_dir = temp_path,
    par_name = "SR_BH_steep",
    par_vals = c(0.4, 0.8),
    refit_OMs = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "cod_SR_BH_steep_0.4", "control.ss_new")))
  expect_true(file.exists(file.path(temp_path, "cod_SR_BH_steep_0.8", "control.ss_new")))

  dat_file <- list.files(file.path(temp_path, "cod_SR_BH_steep_0.4"), pattern = "data.ss_new|data_echo.ss_new")
  dat <- r4ss::SS_readdat(file.path(temp_path, "cod_SR_BH_steep_0.4", dat_file),
    verbose = FALSE
  )
  ctl <- r4ss::SS_readctl(
    file.path(temp_path, "cod_SR_BH_steep_0.4", "control.ss_new"),
    use_datlist = TRUE, datlist = dat, verbose = FALSE
  )
  expect_true(ctl[["SR_parms"]]["SR_BH_steep", "INIT"] == 0.4)
})

new_temp_path <- file.path(temp_path, "refit_OMs")
dir.create(new_temp_path)
extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")

test_that("develop_OMs works as expected with refiting and specifying directory", {
  skip_on_cran()
  develop_OMs(
    OM_in_dir = cod_mod,
    out_dir = new_temp_path,
    par_name = "SR_BH_steep",
    par_vals = 0.6,
    refit_OMs = TRUE
  )
  expect_true(file.exists(file.path(new_temp_path, "cod_SR_BH_steep_0.6", "control.ss_new")))
})

temp_path_mod_rename <- file.path(temp_path, "mod_rename")

test_that("develop_OMs works as expected when renaming the model", {
  skip_on_cran() # b/c runs without estimation
  develop_OMs(
    OM_name = "rename-mod-test",
    OM_in_dir = cod_mod,
    out_dir = temp_path_mod_rename,
    par_name = "SR_BH_steep",
    par_vals = c(0.4, 0.8),
    refit_OMs = FALSE
  )
  expect_true(file.exists(file.path(temp_path_mod_rename, "rename-mod-test_SR_BH_steep_0.4", "control.ss_new")))
  expect_true(file.exists(file.path(temp_path_mod_rename, "rename-mod-test_SR_BH_steep_0.8", "control.ss_new")))

  dat_file <- list.files(file.path(temp_path_mod_rename, "rename-mod-test_SR_BH_steep_0.4"), pattern = "data.ss_new|data_echo.ss_new")
  dat <- r4ss::SS_readdat(file.path(temp_path_mod_rename, "rename-mod-test_SR_BH_steep_0.4", dat_file),
    verbose = FALSE
  )
  ctl <- r4ss::SS_readctl(
    file.path(temp_path_mod_rename, "rename-mod-test_SR_BH_steep_0.4", "control.ss_new"),
    use_datlist = TRUE, datlist = dat, verbose = FALSE
  )
  expect_true(ctl$SR_parms["SR_BH_steep", "INIT"] == 0.4)
})
