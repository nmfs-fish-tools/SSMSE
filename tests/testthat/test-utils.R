context("test utility functions; Construct objects, move files, etc. ")

out_dir <- file.path(tempdir(), "test_utils")
dir.create(out_dir)
on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

test_that("create_scen_list works as expected", {
  # create some simple list that we want them to look like
  scen_list <- list(scen_1 =
                       list(
                         out_dir_scen = out_dir,
                         iter = 1,
                         OM_name = "cod",
                         OM_in_dir = NULL,
                         EM_name = "cod",
                         EM_in_dir = NULL,
                         MS = "EM",
                         use_SS_boot = TRUE,
                         nyrs = 6,
                         nyrs_assess = 3,
                         sample_struct = NULL
                       ),
                      scen_2 =
                        list(
                          out_dir_scen = out_dir,
                          iter = 2,
                          OM_name = "cod",
                          OM_in_dir = NULL,
                          EM_name = "cod",
                          EM_in_dir = NULL,
                          MS = "EM",
                          use_SS_boot = TRUE,
                          nyrs = 3,
                          nyrs_assess = 2,
                          sample_struct = NULL
                        )
                       )

 scen_list_out <- create_scen_list(scen_name_vec = c("scen_1", "scen_2"),
                                    iter_vec = 1:2,
                                    OM_name_vec = "cod",
                                    OM_in_dir_vec = NULL,
                                    use_SS_boot_vec = TRUE,
                                    EM_name_vec = "cod",
                                    EM_in_dir_vec = NULL,
                                    MS_vec = "EM",
                                    out_dir_scen_vec = out_dir,
                                    nyrs_vec = c(6, 3),
                                    nyrs_assess_vec = c(3, 2),
                                    sample_struct_list = NULL
                                    )
 # use expect equal because the names should be the same as well.
 expect_equal(scen_list_out, scen_list)
})

test_that("create_scen_list works with NAs", {
  scen_list <- list(scen_1 =
                        list(
                          out_dir_scen = out_dir,
                          iter_vec = 2,
                          OM_name = "cod",
                          OM_in_dir = NULL,
                          EM_name = "cod",
                          EM_in_dir = NULL,
                          MS = "EM",
                          use_SS_boot = TRUE,
                          nyrs = 6,
                          nyrs_assess = 3,
                          sample_struct = NULL
                        ),
                      scen_2 =
                        list(
                          out_dir_scen = out_dir,
                          iter_vec = 2,
                          OM_name = "cod",
                          OM_in_dir = NULL,
                          EM_name = NULL,
                          EM_in_dir = NULL,
                          MS = "no_catch",
                          use_SS_boot = TRUE,
                          nyrs = 3,
                          nyrs_assess = 2,
                          sample_struct = NULL
                        )
  )
  scen_list_out <- create_scen_list(scen_name_vec = c("scen_1", "scen_2"),
                                     iter_vec = 1:2,
                                     OM_name_vec = "cod",
                                     OM_in_dir_vec = NULL,
                                     use_SS_boot_vec = TRUE,
                                     EM_name_vec = c("cod", NA),
                                     EM_in_dir_vec = NULL,
                                     MS_vec = c("EM", "no_catch"),
                                     out_dir_scen_vec = out_dir,
                                     nyrs_vec = c(6, 3),
                                     nyrs_assess_vec = c(3, 2),
                                     sample_struct_list = NULL
  )
  expect_equal(scen_list_out, scen_list)
})

# TODO: Add checks for error messages.

# get_input_value tests.
dfr <- data.frame("year" = 1:5,
                 "value" = c(2, 2, 2, 3, 3),
                 "se_log" = 0.2)
dfr2 <- dfr
colnames(dfr2) <- NULL
test_that("get_input_value generates errors for bad input", {
  expect_error(get_input_value(data = dfr,
                               method = "m_common_value",
                               colname = "se_log"),
               "method was specified as m_common_value")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = "bad_colname"),
               "bad_colname not found in data")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = "se_log",
                               group = "bad_groupname"),
               "column bad_groupname not found")
  expect_error(get_input_value(data = dfr,
                               method = "only_value",
                               colname = "se_log",
                               group = "bad_groupname"),
               "column bad_groupname not found")
  expect_error(get_input_value(data = c(1, 2, 3),
                               method = "most_common_value",
                               colname = "se_log"),
               "data is not of class 'data.frame'")
  expect_error(get_input_value(data = dfr2,
                               method = "most_common_value",
                               colname = "se_log"),
               "The column names of data are NULL")
  expect_error(get_input_value(data = dfr,
                               method = 1,
                               colname = "se_log"),
               "method is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = 2),
               "colname is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = "se_log",
                               group = 3),
               "group is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = c("most_common_value", "only_value"),
                               colname = "se_log"),
               "method has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = c("val", "se_log")),
               "colname has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               group = c("se_log", "value"),
                               colname = "val"),
               "group has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               group = "value",
                               colname = "value"),
               "group and colname cannot be the same")

})

test_that("get_input_value works as expected", {
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "se_log")
  expect_equal(val, 0.2)
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "value")
  expect_equal(val, 2)
  val <- get_input_value(data = dfr,
                         method = "only_value",
                         colname = "se_log")
  expect_equal(val, 0.2)
  # use group
  val <- get_input_value(data = dfr,
                         method = "only_value",
                         colname = "se_log",
                         group = "value")
  expect_true(is.data.frame(val))
  expect_true(all(val$value %in% c(2, 3)))
  expect_true(unique(val$se_log) == 0.2)
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "se_log",
                         group = "value")
  expect_true(is.data.frame(val))
  expect_true(all(val$value %in% c(2, 3)))
  expect_true(unique(val$se_log) == 0.2)
  # switch the cols used
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "value",
                         group = "se_log")
  expect_true(is.data.frame(val))
  expect_true(nrow(val) == 1)
  expect_true(val$value == 2)
  expect_true(val$se_log == 0.2)

  expect_error(get_input_value(data = dfr,
                               method = "only_value",
                               colname = "value"),
               "Multiple unique values were found in data")
  dfr$new_cat <- c("G1", "G2", "G1", "G1", "G1")
  expect_error(get_input_value(data = dfr,
                               method = "only_value",
                               colname = "value",
                               group = "new_cat"),
               "Multiple unique values were found in data")
})

test_that("create_out_dirs works as expected with no EM", {
  created_mod_1 <- create_out_dirs(out_dir = out_dir,
                     niter = 1,
                     OM_name = "cod",
                     OM_in_dir = NULL)
  created_dir_name_1 <- file.path(out_dir, "1", "cod_OM")
  expect_true(dir.exists(created_dir_name_1))
  expect_true(created_mod_1[["OM_out_dir"]] == created_dir_name_1)
  expect_true(created_mod_1[["OM_in_dir"]] ==
                system.file("extdata", "models", "cod", package = "SSMSE"))
  # read cod in as if it is a custom model, not part of package.
  cod_OM_dir <- system.file("extdata", "models", "cod", package = "SSMSE")
  created_mod_2 <- create_out_dirs(out_dir = out_dir,
                     niter = 2,
                     OM_name = NULL,
                     OM_in_dir = cod_OM_dir)
  created_dir_name_2 <- file.path(out_dir, "2", "cod_OM")
  expect_true(dir.exists(created_dir_name_2))
  expect_true(created_mod_2[["OM_out_dir"]] == created_dir_name_2)
  expect_true(created_mod_2[["OM_in_dir"]] == cod_OM_dir)

  created_mod_3 <- create_out_dirs(out_dir = out_dir,
                                 niter = 3,
                                 OM_name = "custom_cod",
                                 OM_in_dir = cod_OM_dir)
  created_dir_name_3 <- file.path(out_dir, "3", "custom_cod_OM")
  expect_true(dir.exists(created_dir_name_3))
  expect_true(created_mod_3[["OM_out_dir"]] == created_dir_name_3)
  expect_true(created_mod_3[["OM_in_dir"]] == cod_OM_dir)
  # expect warning b/c iteration exists
  expect_warning(mod_3_repeat <- create_out_dirs(out_dir = out_dir,
                                 niter = 3,
                                 OM_name = "custom_cod",
                                 OM_in_dir = cod_OM_dir))
  expect_true(is.null(mod_3_repeat))

  # expect error because "bad_mod_name" is not a built in model.
  expect_error(create_out_dirs(out_dir = out_dir,
                             niter = 4,
                             OM_name = "bad_mod_name",
                             OM_in_dir = NULL),
               "OM_name bad_mod_name matched 0 models in SSMSE")
  # expect error because no path or name given for OM
  expect_error(create_out_dirs(out_dir = out_dir,
                               niter = 5,
                               OM_name = NULL,
                               OM_in_dir = NULL),
               "OM_name and OM_in_dir are both NULL")
})
unlink(file.path(out_dir, "5"), recursive = TRUE)

 test_that("create_out_dirs works as expected with an EM", {
  # EM from package data
  created_mod_1 <- create_out_dirs(out_dir = out_dir,
                                   niter = 5,
                                   OM_name = "cod",
                                   OM_in_dir = NULL,
                                   MS = "EM",
                                   EM_name = "cod",
                                   EM_in_dir = NULL)
  created_dir_name_1 <- file.path(out_dir, "5", "cod_EM_init")
  expect_true(dir.exists(created_dir_name_1))
  expect_true(created_mod_1[["EM_out_dir"]] == created_dir_name_1)
  expect_true(created_mod_1[["EM_in_dir"]] ==
                system.file("extdata", "models", "cod", package = "SSMSE"))
  # EM named, mock as custom
  cod_EM_in_dir <- system.file("extdata", "models", "cod", package = "SSMSE")
  created_mod_2 <- create_out_dirs(out_dir = out_dir,
                                   niter = 6,
                                   OM_name = "cod",
                                   OM_in_dir = NULL,
                                   MS = "EM",
                                   EM_name = "custom_cod",
                                   EM_in_dir = cod_EM_in_dir)
  created_dir_name_2 <- file.path(out_dir, "6", "custom_cod_EM_init")
  expect_true(dir.exists(created_dir_name_2))
  expect_true(created_mod_2[["EM_out_dir"]] == created_dir_name_2)
  expect_true(created_mod_2[["EM_in_dir"]] == cod_EM_in_dir)
  # EM not named, mock as custom
  cod_EM_in_dir <- system.file("extdata", "models", "cod", package = "SSMSE")
  created_mod_3 <- create_out_dirs(out_dir = out_dir,
                                   niter = 7,
                                   OM_name = "cod",
                                   OM_in_dir = NULL,
                                   MS = "EM",
                                   EM_name = NULL,
                                   EM_in_dir = cod_EM_in_dir)
  created_dir_name_3 <- file.path(out_dir, "7", "cod_EM_init")
  expect_true(dir.exists(created_dir_name_3))
  expect_true(created_mod_3[["EM_out_dir"]] == created_dir_name_3)
  expect_true(created_mod_3[["EM_in_dir"]] == cod_EM_in_dir)
  # EM not named, no path given for EM (expect error)
  expect_error(create_out_dirs(out_dir = out_dir,
                               niter = 8,
                               OM_name = "cod",
                               OM_in_dir = NULL,
                               MS = "EM",
                               EM_name = NULL,
                               EM_in_dir = NULL),
      "Management Strategy (MS) is EM (estimation model), but both EM_name and EM_in_dir are null",
      fixed = TRUE)
})


 test_that("copy_model_files works", {
   new_out_dir <- file.path(out_dir, "copy_model_files")
   OM_out_dir <- file.path(new_out_dir, "OM")
   EM_out_dir <- file.path(new_out_dir, "EM")
   dir.create(new_out_dir)
   dir.create(OM_out_dir)
   dir.create(EM_out_dir)
   cod_in_dir <- system.file("extdata", "models", "cod", package = "SSMSE")

   success <- copy_model_files(OM_in_dir = cod_in_dir, OM_out_dir = OM_out_dir)
   expect_equivalent(success, c(TRUE, TRUE))

   # expect_error b/c model files still exist
   expect_error(copy_model_files(OM_in_dir = cod_in_dir, OM_out_dir = OM_out_dir),
                "Problem copying SS OM .ss_new files", fixed = TRUE)
   unlink(OM_out_dir, recursive = TRUE)
   dir.create(OM_out_dir)
   success <- copy_model_files(OM_in_dir = cod_in_dir, OM_out_dir = OM_out_dir,
                    EM_in_dir = cod_in_dir, EM_out_dir = EM_out_dir)
   expect_equivalent(success, c(TRUE, TRUE))
   # copy over only the EM files to a new folder.
   new_EM_folder <- file.path(new_out_dir, "EM_2000")
   dir.create(new_EM_folder)
   success_2 <- copy_model_files(EM_in_dir = EM_out_dir,
                                 EM_out_dir = new_EM_folder)
   expect_equivalent(success, c(TRUE, TRUE))
   unlink(OM_out_dir, recursive = TRUE)
   dir.create(OM_out_dir)
   expect_error(copy_model_files(OM_in_dir = cod_in_dir, OM_out_dir = OM_out_dir,
                    EM_in_dir = cod_in_dir, EM_out_dir = EM_out_dir),
                "Problem copying SS EM files", fixed = TRUE)
 })

 test_that("clean init_mod_files_works", {
   # mock some values outside modle year in the modesl
   OM_dir <- file.path(out_dir, "copy_model_files", "OM")
   EM_dir <- file.path(out_dir, "copy_model_files", "EM")
   OM_dat_orig <- SS_readdat(file.path(OM_dir, "ss3.dat"), verbose = FALSE)
   dat <- SS_readdat(file.path(EM_dir, "ss3.dat"), verbose = FALSE)
   dat$CPUE[1, "year"] <- -dat$CPUE[1, "year"]
   dat$agecomp[1, "Yr"] <- -dat$agecomp[1, "Yr"]
   neg_val_CPUE <- dat$CPUE[1, "year"]
   neg_val_agecomp <- dat$agecomp[1, "Yr"]
   r4ss::SS_writedat(dat, file.path(EM_dir, "ss3.dat"), verbose = FALSE)

   clean_dat_list <- clean_init_mod_files(OM_out_dir = OM_dir, EM_out_dir = EM_dir)
   expect_true(all(clean_dat_list$EM_dat$CPUE$year >= 0))
   expect_true(all(clean_dat_list$EM_dat$agecomp$Yr >= 0))
   expect_true(all(clean_dat_list$OM_dat$CPUE$year == OM_dat_orig$CPUE$year))
   expect_true(all(clean_dat_list$OM_dat$lencomp$Yr == OM_dat_orig$lencomp$Yr))

   clean_dat_list_2 <- clean_init_mod_files(OM_out_dir = OM_dir, EM_out_dir = NULL)
   expect_true(all(clean_dat_list_2$EM_dat$CPUE$year >= 0))
   expect_true(all(clean_dat_list_2$EM_dat$agecomp$Yr >= 0))
   expect_true(is.null(clean_dat_list_2[[2]]))
 })
 
 test_that("setting seeds works", {
   iter_vec <- c(2,3)
   seed_rand <- set_MSE_seeds(iter_vec = iter_vec)
   expect_true(length(seed_rand) == 3)
   expect_true(length(seed_rand$scenario) == length(iter_vec))
   expect_true(length(seed_rand$iter[[1]]) == iter_vec[1])
   expect_true(length(seed_rand$iter[[2]]) == iter_vec[2])
   seed_1 <- set_MSE_seeds(seed = 123, 
                           iter_vec = iter_vec)
   seed_2 <- set_MSE_seeds(seed = 123, 
                           iter_vec = iter_vec)
   expect_equal(seed_1, seed_2)
 })
 
 test_that("setting seeds works with diff expected seed inputs", {
   iter_vec <- c(2, 3)
   # seed of length 3; does it make sense how these are set?
   seed_rand <- set_MSE_seeds(
     seed = c(10, 20, 30),
     iter_vec = iter_vec)
   expect_length(seed_rand, 3)
   expect_equal(seed_rand$global, 10)
   expect_equal(seed_rand$scenario, c(20, 30))
   expect_equal(length(seed_rand$iter[[1]]), iter_vec[1])
   expect_equal(length(seed_rand$iter[[2]]), iter_vec[2])
   # seed of length 7
   seed_rand <- set_MSE_seeds(
     seed = c(10, 20, 30, 40, 50, 60, 70, 80),
     iter_vec = iter_vec)
   expect_length(seed_rand, 3)
   expect_equal(seed_rand$global, 10)
   expect_equal(seed_rand$scenario, c(20, 30))
   expect_equal(seed_rand$iter[[1]], c(40, 50))
   expect_equal(seed_rand$iter[[2]], c(60, 70, 80))
 })
 
 test_that("setting seeds works with list inputs", {
   iter_vec <- c(2, 3)
   seed_input <- list(global = 10,
                      scenario = c(20, 30), 
                      iter = list(c(40, 50), c(60, 70, 80)))
   seed_rand <- set_MSE_seeds(
     seed = seed_input,
     iter_vec = iter_vec)
   expect_equal(seed_rand, seed_input)
 })

 test_that("setting seeds exits on error with bad input", {
   #need to check with NV on what are valid inputs for seed.
   expect_error(bad_val <- set_MSE_seeds(seed = c(1, 2, 3, 4),
                            iter_vec = c(2,3)),
                "The length of your seed vector doesn't match either")
 })