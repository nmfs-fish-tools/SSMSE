context("test that functions in parse_MS work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-parse_MS")
dir.create(temp_path, showWarnings = FALSE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
cod_OM_path <- file.path(temp_path, "cod")

out_dir <- file.path(temp_path, "out_dir")
dir.create(out_dir)

OM_data <- r4ss::SS_readdat(file.path(cod_OM_path, "ss3.dat"), verbose = FALSE)

test_that("parse_MS works for no estimation model methods", {
  # no catch managemetn strategy
  catch_df_1 <- parse_MS(MS = "no_catch",
                    out_dir = out_dir,
                    OM_data = OM_data,
                    nyrs_assess = 3)
  expect_equivalent(catch_df_1$catch, rep(0, times = 3))
  # last year management strategy
  catch_df_2 <- parse_MS(MS = "last_yr_catch",
                    out_dir = out_dir,
                    OM_data = OM_data,
                    nyrs_assess = 3)
  lyr_catch <- OM_data$catch$catch[nrow(OM_data$catch)]
  expect_true(all(catch_df_2$catch == lyr_catch))
})

test_that("parse_MS works as currently expected for estimation model methods", {
  skip_on_cran()
  # use cod as the EM
  catch_df_3 <- parse_MS(MS = "EM", 
                         out_dir = out_dir, 
                         EM_name = "cod",
                         OM_data = OM_data, 
                         nyrs_assess = 3)
  expect_true(nrow(catch_df_3) == 3)
  expect_true(ncol(catch_df_3) == 5)
  expect_equivalent(colnames(catch_df_3),
                    c("year", "seas", "fleet", "catch", "catch_se"))
})

test_that("parse_MS catches errors when it should", {
  # MS is invalid
  expect_error(parse_MS(MS = "bad_option", EM_name = NULL, EM_dir = NULL,
           out_dir = "fake", OM_data = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "MS was input as", fixed = TRUE)
  # EM option chosen, but not specified
  expect_error(parse_MS(MS = "EM", EM_name = NULL, EM_dir = NULL,
           out_dir = "fake", OM_data = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "Management Strategy (MS) is EM (estimation model, but both EM_name and EM_dir are null",
           fixed = TRUE)
  # EM option overspecified
  expect_error(parse_MS(MS = "EM", EM_name = "cod", EM_dir = "other_fake_dir",
           out_dir = "fake", OM_data = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "Management Strategy (MS) is EM (estimation model, but both EM_name and EM_dir are specified",
           fixed = TRUE)
  # invalid EM name
  expect_error(parse_MS(MS = "EM", EM_name = "not_real_species",
           EM_dir = NULL, out_dir = "fake", OM_data = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "Currently, EM_name can only be one of the following", fixed = TRUE)
  # invalid EM_dir
  expect_error(parse_MS(MS = "EM", EM_name = NULL, EM_dir = "other_fake_dir",
           out_dir = "fake", OM_data = "fake_2",
           verbose = FALSE, nyrs_assess = 3),
           "Please change to a directory containing a valid SS model",
           fixed = TRUE)
  #TODO: need to catch invalid out_dir and OM_data? What about nyrs_assess?
})