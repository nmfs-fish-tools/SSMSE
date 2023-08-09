context("Make sure wrapper functions to run SSMSE work")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-runSSMSE")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)
extdat_path <- system.file("extdata", package = "SSMSE")


test_that("run_SSMSE_iter runs with no EM", {
  skip_on_cran()
  new_temp_path <- file.path(temp_path, "no_EM")
  dir.create(new_temp_path)
  result <- run_SSMSE_iter(
    OM_name = "cod",
    MS = "no_catch",
    out_dir = new_temp_path,
    nyrs = 6,
    nyrs_assess = 3,
    iter_seed = list(global = 12345, scenario = 123456, iter = 1234567)
  )
  expect_true(file.exists(file.path(new_temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(result)
})


test_that("run_SSMSE runs with an EM, and works with summary funs", {
  skip_on_cran()
  nyrs <- 7
  datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
  # use sample_struct to determine its structure
  sample_struct <- create_sample_struct(
    dat = datfile, nyrs = nyrs,
    rm_NAs = TRUE
  ) # note warning
  result <- run_SSMSE(
    scen_name_vec = "H-ctl", # name of the scenario
    out_dir_scen_vec = temp_path, # directory in which to run the scenario
    iter_vec = 1, # run with 5 iterations each
    OM_name_vec = "cod",
    EM_name_vec = "cod", # cod is included in package data
    MS_vec = "EM", # The management strategy is specified in the EM
    custom_MS_source = NULL,
    use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
    run_EM_last_yr = FALSE,
    nyrs_vec = nyrs, # Years to project OM forward
    nyrs_assess_vec = 3, # Years between assessments
    sample_struct_list = list(sample_struct), # How to sample data for running the EM.
    seed = 12345
  ) # Set a fixed integer seed that allows replication
  expect_equivalent(result[["H-ctl"]][["errored_iterations"]], "No errored iterations")
  expect_true(file.exists(file.path(temp_path, "H-ctl", "1", "cod_OM", "data.ss_new")))
  expect_true(file.exists(file.path(temp_path, "H-ctl", "1", "cod_EM_106", "data.ss_new")))
  expect_length(result, 1)
  # some more specific values, specific to the scenario above.
  dat <- SS_readdat(file.path(temp_path, "H-ctl", "1", "cod_EM_106", "data.ss_new"), verbose = FALSE)
  added_catch <- dat[["catch"]][dat[["catch"]][["year"]] %in% sample_struct[["catch"]][["Yr"]], ]
  old_catch <- dat[["catch"]][dat[["catch"]][["year"]] < min(sample_struct[["catch"]][["Yr"]]), ]
  expect_true(all(added_catch[["catch_se"]] == unique(sample_struct[["catch"]][["SE"]])))
  added_CPUE <- dat[["CPUE"]][dat[["CPUE"]][["year"]] >= min(sample_struct[["CPUE"]][["Yr"]]), ]
  expect_true(all(sample_struct[["CPUE"]][["Yr"]] %in% unique(added_CPUE[["year"]])))
  added_agecomp <- dat[["agecomp"]][dat[["agecomp"]][["Yr"]] %in% sample_struct[["agecomp"]][["Yr"]], ]
  expect_true(all(sample_struct[["agecomp"]][["Yr"]] %in% unique(added_agecomp[["Yr"]])))
  # summarize 1 iteration of results
  summary_iter <- SSMSE_summary_iter(file.path(temp_path, "H-ctl", "1"))
  expect_true(length(summary_iter) == 3)
  summary_scen <- SSMSE_summary_scen(file.path(temp_path, "H-ctl"))
  expect_true(length(summary_scen) == 3)
  summary <- SSMSE_summary_all(temp_path, scenarios = "H-ctl")
  expect_true(length(summary) == 3)
  # make sure OM ran through the last year.
  expect_true((100 + nyrs) %in% summary[["ts"]][summary[["ts"]][["model_run"]] == "cod_OM", "year"])
  # test plotting
  index_plot_list <- plot_index_sampling(dir = file.path(temp_path, "H-ctl"))
  expect_length(index_plot_list, 2)
  expect_length(unique(index_plot_list[["index_dat"]][["model_run"]]), 3)
  # TODO: add plot testing when updating ggplots.
})

test_that("run_SSMSE runs multiple iterations/scenarios and works with summary funs", {
  # This tests takes a while to run, but is really helpful.
  new_temp_path <- file.path(temp_path, "mult_scenarios")
  skip_on_cran()
  nyrs <- 6
  datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
  # use sample_struct to determine its structure
  sample_struct <- create_sample_struct(dat = datfile, nyrs = nyrs) # note warning
  sample_struct[["lencomp"]] <- NULL
  sample_struct[["meanbodywt"]] <- NULL
  sample_struct[["MeanSize_at_Age_obs"]] <- NULL
  result <- run_SSMSE(
    scen_name_vec = c("H-ctl", "H-scen-2"), # name of the scenario
    out_dir_scen_vec = new_temp_path, # directory in which to run the scenario
    iter_vec = c(2, 2), # run with 5 iterations each
    OM_name_vec = "cod",
    EM_name_vec = "cod", # cod is included in package data
    MS_vec = "EM", # The management strategy is specified in the EM
    custom_MS_source = NULL,
    use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
    nyrs_vec = nyrs, # Years to project OM forward
    nyrs_assess_vec = 3, # Years between assessments
    run_EM_last_yr = FALSE,
    run_parallel = FALSE,
    sample_struct_list = list(sample_struct, sample_struct), # How to sample data for running the EM.
    seed = 12345
  ) # Set a fixed integer seed that allows replication
  expect_equivalent(result[["H-ctl"]][["errored_iterations"]], "No errored iterations")
  expect_true(file.exists(
    file.path(new_temp_path, "H-ctl", "1", "cod_OM", "data.ss_new")
  ))
  expect_true(file.exists(
    file.path(new_temp_path, "H-ctl", "1", "cod_EM_103", "data.ss_new")
  ))
  # this file should not exist b/c run_EM_last_yr is FALSE.
  expect_true(!file.exists(
    file.path(new_temp_path, "H-ctl", "1", "cod_EM_106", "data.ss_new")
  ))
  expect_equivalent(
    result[["H-scen-2"]][["errored_iterations"]],
    "No errored iterations"
  )
  expect_true(file.exists(
    file.path(new_temp_path, "H-scen-2", "1", "cod_OM", "data.ss_new")
  ))
  expect_true(file.exists(
    file.path(new_temp_path, "H-scen-2", "1", "cod_EM_103", "data.ss_new")
  ))
  expect_true(!file.exists(
    file.path(new_temp_path, "H-scen-2", "1", "cod_EM_106", "data.ss_new")
  ))
  expect_length(result, 2)
  # summarize results
  summary <- SSMSE_summary_all(dir = new_temp_path, run_parallel = FALSE)
  expect_true(length(summary) == 3)
})


OM_path_cod <- file.path(extdat_path, "models", "cod")
EM_path_cod <- file.path(extdat_path, "models", "cod")
test_that("cod works when treated as a custom model and run_EM_last_yr = TRUE works", {
  skip_on_cran()
  new_temp_path <- file.path(temp_path, "custom_cod")
  dir.create(new_temp_path)
  catch_add_yrs <- 101:106
  add_yrs <- c(102, 105)
  result <- run_SSMSE_iter(
    OM_name = NULL,
    OM_in_dir = OM_path_cod,
    MS = "EM",
    custom_MS_source = NULL,
    out_dir = new_temp_path,
    EM_name = NULL,
    EM_in_dir = EM_path_cod,
    run_EM_last_yr = TRUE,
    nyrs = 6,
    nyrs_assess = 3,
    iter_seed = list(global = 12345, scenario = 123456, iter = 1234567),
    sample_struct = list(
      catch = data.frame(Yr = catch_add_yrs, Seas = 1, FltSvy = 1),
      CPUE = data.frame(Yr = add_yrs, Seas = 7, FltSvy = 2),
      lencomp = data.frame(
        Yr = add_yrs, Seas = 1, FltSvy = 1,
        Sex = 0, Part = 0
      ),
      agecomp = data.frame(
        Yr = add_yrs, Seas = 1, FltSvy = 2,
        Sex = 0, Part = 0, Ageerr = 1,
        Lbin_lo = -1, Lbin_hi = -1
      )
    )
  )
  expect_true(file.exists(file.path(new_temp_path, "1", "cod_OM", "data.ss_new")))
  expect_true(file.exists(file.path(new_temp_path, "1", "cod_EM_106", "data.ss_new")))
  expect_true(result)
})

test_that("run_SSMSE runs with mean size at age and mean body length", {
  skip_on_cran()
  # this test is skipped unless the model it needs to run is available.
  # model from https://github.com/nmfs-stock-synthesis/ss-test-models/tree/main/models/Simple_with_Discard
  skip_if(!file.exists(file.path(extdat_path, "models", "Simple_with_Discard")))
  size_age_temp_path <- file.path(temp_path, "Simple_with_Discard")
  dir.create(size_age_temp_path)
  sample_struct <- create_sample_struct(
    dat = file.path(extdat_path, "models", "Simple_with_Discard", "data.ss"),
    nyrs = 6
  )
  sample_struct[["CPUE"]] <- na.omit(sample_struct[["CPUE"]])
  sample_struct[["lencomp"]][["Part"]] <- 0
  sample_struct[["agecomp"]][["Part"]] <- 0
  sample_struct[["meanbodywt"]][["Yr"]] <- 2002
  sample_struct[["meanbodywt"]][["Part"]] <- 1
  sample_struct[["MeanSize_at_Age_obs"]][["Yr"]] <- c(2002, 2004)
  sample_struct[["MeanSize_at_Age_obs"]][["Part"]] <- 0

  result <- run_SSMSE(
    scen_name_vec = "test_1",
    out_dir_scen_vec = size_age_temp_path,
    iter_vec = 1,
    OM_in_dir_vec = file.path(extdat_path, "models", "Simple_with_Discard"),
    EM_in_dir_vec = file.path(extdat_path, "models", "Simple_with_Discard"),
    MS_vec = "EM",
    nyrs_assess_vec = 3,
    nyrs_vec = 6,
    sample_struct_list = list(sample_struct)
  )
  expect_true(result[[1]][["errored_iterations"]] == "No errored iterations")
  # read in the data file produced from the last EM to make sure sampling occured
  out_dat <- r4ss::SS_readdat(
    file.path(
      temp_path, "Simple_with_Discard", "test_1", "1",
      "Simple_with_Discard_EM_2004", "data.ss_new"
    ),
    section = 1
  )
  # convert to r4ss names so colnames match
  sample_struct_converted <- convert_to_r4ss_names(sample_struct)
  # check that all mean size rows were added
  matches_meanbodywt <- merge(sample_struct_converted[["meanbodywt"]],
    out_dat[["meanbodywt"]],
    by = colnames(sample_struct_converted[["meanbodywt"]]),
    all = FALSE
  )
  expect_true(NROW(matches_meanbodywt) == NROW(sample_struct[["meanbodywt"]]))
  # check that all mean size at age rows were added
  matches_at_age <- merge(sample_struct_converted[["MeanSize_at_Age_obs"]],
    out_dat[["MeanSize_at_Age_obs"]],
    by = colnames(sample_struct_converted[["MeanSize_at_Age_obs"]])[-7],
    all = FALSE
  )
  expect_true(NROW(matches_at_age) == NROW(sample_struct[["MeanSize_at_Age_obs"]]))
})
