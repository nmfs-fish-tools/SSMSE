# create a test object of user input that can change the OM's future structure
# assume this could be compatible with cod and that there are 3 scenarios: scen1, scen2, and scen3

# Object 1 description ----
# Start by specifying 2  changes to make:
# 1. Make M vary randomly around its historic mean for scen2 and scen 3
# 2. Change 1 parameter of the selectivity curve at a certain time point from
#  5.1 to 4.5 for scen2 only

future_om_list <- vector(mode = "list", length = 2)
future_om_list <- lapply(future_om_list, function(x) x <- vector(mode = "list", length = 4))
names(future_om_list[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for M
future_om_list[[1]][["pars"]] <- "NatM_p_1_Fem_GP_1"
future_om_list[[1]][["scen"]] <- c("replicate", "scen2", "scen3")
future_om_list[[1]][["pattern"]] <- c("model_change", "normal")
future_om_list[[1]][["input"]] <- data.frame(
  first_yr_averaging = 1,
  last_yr_averaging = 100,
  last_yr_orig_val = 100,
  first_yr_final_val = 101,
  ts_param = "sd",
  method = "absolute",
  value = 0.01
) # jitter with a standard deviation equal to 0.01


# add values for selectivity curve param. step change occuring in year 103
future_om_list[[2]][["pars"]] <- "SizeSel_P_3_Fishery(1)" # had to figure this out from reading in the par file.
future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
future_om_list[[2]][["pattern"]] <- c("model_change", "normal") # defaults to normal (with SD 0, mean at last yr of mod val?)
future_om_list[[2]][["input"]] <- data.frame(
  first_yr_averaging = NA, # NA b/c not using historical values
  last_yr_averaging = NA, # NA b/c not using historical values
  last_yr_orig_val = 102,
  first_yr_final_val = 103,
  ts_param = "mean",
  method = "absolute",
  value = 4.5
)

# Object 2, apply change to all params ----
# Start by specifying 2  changes to make:
# 1. Apply a random jitter to all parameters change to all paramters
# NOTE: This is only applying the jitter in scenarios 2 and 3

future_om_list_2 <- vector(mode = "list", length = 1)
future_om_list_2 <- lapply(future_om_list_2, function(x) x <- vector(mode = "list", length = 4))
names(future_om_list_2[[1]]) <- c("pars", "scen", "pattern", "input")

# add jittered values using mean from historic time series to all future parameters.
future_om_list_2[[1]][["pars"]] <- "all"
future_om_list_2[[1]][["scen"]] <- c("randomize", "scen2", "scen3")
future_om_list_2[[1]][["pattern"]] <- "model_change" # defaults to using normal dist
# should CVs be an option to specify the random sampling inputs instead of standard dev
# probably more meaningful when there are many parameter types, so included here...
future_om_list_2[[1]][["input"]] <- data.frame(
  first_yr_averaging = c(1, 1),
  last_yr_averaging = c(100, 100),
  last_yr_orig_val = c(100, 100),
  first_yr_final_val = c(101, 101),
  ts_param = c("cv", "mean"),
  method = c("absolute", "multiplier"),
  value = c(0.1, 1)
)
# We will have to think more about how to handle the rec_dev and environmental vals as they have both variability in their deviations and
# specified st_dev in each year??? Not sure what the best way to handle is.
# NOTE: Having thought about it more I think that we
# should largely ignore the annual model uncertainties on rec_devs/implementation error/environmental indicies and just calculate sd/variance/autocorrelation from the
# time-series of their values, I think this makes more sense as that is the pattern we are trying to replicate.

# Object 3, use custom ---
# set future values for von bert k and and for survey q


future_om_list_3 <- vector(mode = "list", length = 2)
future_om_list_3 <- lapply(future_om_list_3, function(x) x <- vector(mode = "list", length = 4))
names(future_om_list_3[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list_3[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for list element 1
future_om_list_3[[1]][["pars"]] <- "VonBert_K_Fem_GP_1"
future_om_list_3[[1]][["scen"]] <- c("replicate", "all")
future_om_list_3[[1]][["pattern"]] <- "custom"
# is parameter name really necessary? should each param just be put in separate
# lists instead if there are multiple, as shown in this example?
# I don't think it is really any easier if you have it have it split out into lists,
# and their is a chance you could want to apply the same custom series to a number of
# parameters at once like mirrored fleets maybe? in which case the current method should be simpler?
# the following list has von bertK set as 0.2 for all years in scen1 and scen2,
# but set at 0.3 in scen3
future_om_list_3[[1]][["input"]] <- data.frame(
  par = "VonBert_K_Fem_GP_1",
  scen = rep("all", times = 6 * 5),
  iter = rep(1:5, times = 6),
  yr = rep(101:106, times = rep(5, times = 6)),
  value = rep(0.2, times = 6 * 5)
)

# add in vals for list element 2 - This parameter has a positive, deterministic
# trend over time that ends at 1.5 of the original mean value.
future_om_list_3[[2]][["pars"]] <- "LnQ_base_Survey(2)"
future_om_list_3[[2]][["scen"]] <- c("replicate", "all")
future_om_list_3[[2]][["pattern"]] <- c("model_change", "norm") # note norm is also the default
future_om_list_3[[2]][["input"]] <- data.frame(
  first_yr_averaging = NA,
  last_yr_averaging = NA,
  last_yr_orig_val = 100,
  first_yr_final_val = 106,
  ts_param = "mean",
  method = "multiplier",
  value = 1.5
)

# Object 4 ----
# Specify recdevs and implementation error
future_om_list_4 <- vector(mode = "list", length = 2)
future_om_list_4 <- lapply(future_om_list_4, function(x) x <- vector(mode = "list", length = 4))
names(future_om_list_4[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list_4[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for list element 1 - use recdevs with the same sd as the historic ones,
# but with autocorrelation. The mean will be the default as the last model year.
future_om_list_4[[1]][["pars"]] <- "rec_devs" # or recdevs, not sure which way to spell is better
future_om_list_4[[1]][["scen"]] <- c("replicate", "all")
future_om_list_4[[1]][["pattern"]] <- c("model_change", "normal")
future_om_list_4[[1]][["input"]] <- data.frame(
  first_yr_averaging = c(1, NA),
  last_yr_averaging = c(100, NA),
  last_yr_orig_val = c(100, 100),
  first_yr_final_val = c(101, 101),
  ts_param = c("sd", "ar_1_phi"), # coeff on ar 1 process.
  method = c("multiplier", "absolute"),
  value = c(1, 0.1)
) # NOTE: Auto correlation has to be a value less than 1 and greater than -1 to be stationary
# a value of 1 would be a perfect random walk. Negative values can have weird stationary distributions that
# oscillate positive to negative, kind of like a sound wave where the amplitude is the random walk.

# add in vals for list element 2
future_om_list_4[[2]][["pars"]] <- "impl_error"
future_om_list_4[[2]][["scen"]] <- c("randomize", "scen1", "scen2", "scen3")
future_om_list_4[[2]][["pattern"]] <- "custom"
future_om_list_4[[2]][["input"]] <- data.frame(
  par = "impl_error",
  scen = rep(c("scen1", "scen2", "scen3"), times = rep(6 * 5, times = 3)),
  iter = rep(1:5, times = 3 * 6),
  yr = rep(rep(101:106, times = rep(5, times = 6)), times = 3),
  value = c(rep(1.05, times = 6 * 5), rep(1.10, times = 6 * 5 * 2)) # this specifies catch as 5% or 10% greater than the catch from the MS

  # NOTE: While testing the timeseries sim I got to thinking, should we offer an option for the user to specify the random distribution function?
  # maybe just a couple of options like normal, lognormal, and uniform? not sure if others would be needed? just a thought. YES, for the model change option.
)


# Time-varying parameter input dataframe
# This object will be passed from a sampling function that calculates environmental linkage devs
# to a file update function that modifies SS files to incorporate new timevarying impacts
nyrs <- 100 # the total number of years to run MSE simulations for
Time_varying_devs <- data.frame(
  rec_devs = stats::rnorm(nyrs), # The functions I'm using are just examples. just input a vector of values of length nyrs
  LnQ_base_Survey_2 = stats::rnorm(nyrs), # We will need to modify the naming of some par slightly like this one to not have brackets
  Env_1 = rnorm(nyrs), # I think we should use Env_n or something similar for when we are extending an existing environmental index
  NatM_p_1_Fem_GP_1 = stats::rnorm(nyrs),
  impl_error = rep(0, nyrs)
)


test_that("Input checks work", {
  future_om_list_new <- check_future_om_list_str(future_om_list)
  expect_equal(future_om_list_new, future_om_list)
})

test_that("Partial matching and missing for pattern works", {
  future_om_list_mod <- future_om_list
  future_om_list_mod[[1]][["pattern"]][2] <- "norm"
  future_om_list_new <- check_future_om_list_str(
    future_om_list = future_om_list_mod
  )
  expect_true(future_om_list_new[[1]][["pattern"]][2] == "normal")
  future_om_list_mod[[1]][["pattern"]] <-
    future_om_list_mod[[1]][["pattern"]][1]
  future_om_list_new <- check_future_om_list_str(
    future_om_list = future_om_list_mod
  )
  # because the default is using a normal distribution
  expect_true(future_om_list_new[[1]][["pattern"]][2] == "normal")
})

test_that("incorrect inputs are caught", {
  future_om_list_mod <- future_om_list
  future_om_list_mod[[1]][["pattern"]] <- "badval"
  expect_error(
    check_future_om_list_str(future_om_list = future_om_list_mod),
    "'arg' should be one of"
  )
  future_om_list_mod <- future_om_list
  future_om_list_mod[[1]][["input"]] <- NULL
  expect_error(
    check_future_om_list_str(future_om_list = future_om_list_mod),
    "Names of list elements not correct"
  )
  future_om_list_mod <- future_om_list
  future_om_list_mod[[1]][["input"]] <- future_om_list_mod[[1]][["input"]][, -1]
  expect_error(check_future_om_list_str(future_om_list = future_om_list_mod),
    "Because pattern future_om_list[[",
    fixed = TRUE
  )
  future_om_list_mod <- future_om_list
  future_om_list_mod[[2]]["scen"] <- c("dontreplicate", "scen2")
  expect_error(check_future_om_list_str(future_om_list = future_om_list_mod),
    "'arg' should be one of",
    fixed = TRUE
  )
})

future_om_list <- check_future_om_list_str(future_om_list)
future_om_list_2 <- check_future_om_list_str(future_om_list_2)
future_om_list_3 <- check_future_om_list_str(future_om_list_3)
future_om_list_4 <- check_future_om_list_str(future_om_list_4)

scen_list <- create_scen_list(
  scen_name_vec = c("scen1", "scen2", "scen3"),
  out_dir_scen_vec = NULL,
  iter_vec = 5,
  OM_name_vec = "cod",
  EM_name_vec = "cod",
  MS_vec = "EM",
  use_SS_boot_vec = TRUE,
  nyrs_vec = c(6, 6, 6),
  nyrs_assess_vec = 3
)
test_that("checks with scen info doesnt error with valid input", {
  return_list <- check_future_om_list_vals(
    future_om_list = future_om_list,
    scen_list = scen_list
  )
  expect_equal(return_list, future_om_list)
  return_list_2 <- check_future_om_list_vals(
    future_om_list = future_om_list_3,
    scen_list = scen_list
  )
  expect_return <- future_om_list_3
  expect_return[[1]][["scen"]] <- c("replicate", "scen1", "scen2", "scen3")
  expect_return[[2]][["scen"]] <- c("replicate", "scen1", "scen2", "scen3")
  expect_equal(return_list_2, expect_return)
  return_list_3 <- check_future_om_list_vals(
    future_om_list = future_om_list_4,
    scen_list = scen_list
  )
  expect_return <- future_om_list_4
  expect_return[[1]][["scen"]] <- c("replicate", "scen1", "scen2", "scen3")
  expect_equal(return_list_3, expect_return)
})

test_that("Checks with scen info does catch bad input", {
  scen_list <- create_scen_list(
    scen_name_vec = c("scen1", "scen2", "scen3"),
    out_dir_scen_vec = NULL,
    iter_vec = 5,
    OM_name_vec = "cod",
    EM_name_vec = "cod",
    MS_vec = "EM",
    use_SS_boot_vec = TRUE,
    nyrs_vec = c(6, 6, 7),
    nyrs_assess_vec = 3
  )
  expect_error(
    check_future_om_list_vals(
      future_om_list = future_om_list_3,
      scen_list = scen_list
    ),
    paste0(
      "Cannot use 'scen = all' for custom if the number of years ",
      "extended forward differ across scenarios"
    )
  )

  scen_list <- create_scen_list(
    scen_name_vec = c("scen1", "scen2", "scen3"),
    out_dir_scen_vec = NULL,
    iter_vec = 5,
    OM_name_vec = "cod",
    EM_name_vec = "cod",
    MS_vec = "EM",
    use_SS_boot_vec = TRUE,
    nyrs_vec = c(6, 6, 6),
    nyrs_assess_vec = 3
  )
  bad_future_om_list <- future_om_list_4
  bad_future_om_list[[2]][["input"]] <- bad_future_om_list[[2]][["input"]][-1, ]
  expect_error(
    check_future_om_list_vals(
      future_om_list = bad_future_om_list,
      scen_list = scen_list
    ),
    "Expecting 90, but there are 89 rows"
  )
})

test_that("Creating the devs df works with sampling", {
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  future_om_list <- check_future_om_list_str(future_om_list = future_om_list)
  future_om_list <- check_future_om_list_vals(
    future_om_list = future_om_list,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 10
  )
  devs_df <- devs_list[["dev_vals"]]
  expect_true(nrow(devs_df) == 10)
  expect_equal(colnames(devs_df), c("yrs", "NatM_p_1_Fem_GP_1", "SizeSel_P_3_Fishery(1)"))
  expect_equivalent(devs_df[["yrs"]], 101:110)
  expect_equivalent(devs_df[["SizeSel_P_3_Fishery(1)"]], c(rep(0, 2), rep(4.5 - 5.275309, 8)), tolerance = 0.0001)
  expect_true(all(devs_df[["NatM_p_1_Fem_GP_1"]] > (-0.03))) # very unlikely to be less than 3 sds away, but possible...
  expect_true(all(devs_df[["NatM_p_1_Fem_GP_1"]] < (0.03))) # very unlikely to be greater than 3 sds away, but possible
})
test_that("Creating the devs df works with custom", {
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  future_om_list_3 <- check_future_om_list_str(future_om_list = future_om_list_3)
  future_om_list_3 <- check_future_om_list_vals(
    future_om_list = future_om_list_3,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = future_om_list_3,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 9 # note: replaces years 107 to 110 with 0s since no values provided.
  )
  devs_df <- devs_list[["dev_vals"]]
  expect_true(nrow(devs_df) == 9)
  expect_equal(colnames(devs_df), c("yrs", "VonBert_K_Fem_GP_1", "LnQ_base_Survey(2)"))
  expect_equivalent(devs_df[["yrs"]], 101:109)
  expect_equivalent(devs_df[["VonBert_K_Fem_GP_1"]], c(rep(0.2 - 0.2039802, 6), rep(0, 3)), tolerance = 0.001) # maybe provide a warning if 0s for custom changes?
  expect_equivalent(devs_df[["LnQ_base_Survey(2)"]],
    c(
      seq(0, 0.0207711 * 1.5 - 0.0207711, length.out = 7)[-1],
      rep(0.0207711 * 1.5 - 0.0207711, 3)
    ),
    tolerance = 0.0001
  )
})

test_that("Creating the devs df works with log normal dist", {
  tmp_future_om_list <- vector(mode = "list", length = 2)
  tmp_future_om_list <- lapply(
    tmp_future_om_list,
    function(x) x <- vector(mode = "list", length = 4)
  )
  names(tmp_future_om_list[[1]]) <- c("pars", "scen", "pattern", "input")
  names(tmp_future_om_list[[2]]) <- c("pars", "scen", "pattern", "input")

  # add in vals for M and steepness. Jitter after year 101, with a log normal dist.
  # note users inputs for lognormal should be on the nominal scale still
  tmp_future_om_list[[1]][["pars"]] <- c("NatM_p_1_Fem_GP_1", "SR_BH_steep")
  tmp_future_om_list[[1]][["scen"]] <- c("randomize", "scen1", "scen2")
  tmp_future_om_list[[1]][["pattern"]] <- c("model_change", "lognormal")
  tmp_future_om_list[[1]][["input"]] <- data.frame(
    first_yr_averaging = NA,
    last_yr_averaging = NA,
    last_yr_orig_val = 101,
    first_yr_final_val = 102,
    ts_param = "sd",
    method = "absolute",
    value = 0.02
  ) # jitter with a standard deviation equal to 0.01


  # add values for selectivity curve param. step change occuring in year 103
  tmp_future_om_list[[2]][["pars"]] <- "NatM_p_1_Fem_GP_1" # had to figure this out from reading in the par file.
  tmp_future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
  tmp_future_om_list[[2]][["pattern"]] <- c("model_change", "normal") # defaults to normal (with SD 0, mean at last yr of mod val?)
  tmp_future_om_list[[2]][["input"]] <- data.frame(
    first_yr_averaging = NA, # NA b/c not using historical values
    last_yr_averaging = NA, # NA b/c not using historical values
    last_yr_orig_val = 106,
    first_yr_final_val = 112, # use 12 years projection
    ts_param = "mean",
    method = "additive", # need to implmeent this option
    value = 0.2
  ) # so at end should be 0.4
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  tmp_future_om_list <- check_future_om_list_str(future_om_list = tmp_future_om_list)
  tmp_future_om_list <- check_future_om_list_vals(
    future_om_list = tmp_future_om_list,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_df <- devs_list[["dev_vals"]]
  # modify the following expectations
  expect_true(nrow(devs_df) == 12)
  expect_equal(colnames(devs_df), c("yrs", "NatM_p_1_Fem_GP_1", "SR_BH_steep"))
  expect_equivalent(devs_df[["yrs"]], 101:112)
  # TODO: figure out how to characterize all of the changes. Just put in the 2
  # obvious (I think) values for now.
  expect_equivalent(devs_df[12, "NatM_p_1_Fem_GP_1"], 0.2)
  expect_equivalent(devs_df[1, "SR_BH_steep"], 0)
})

test_that("Creating the devs df works for recdevs, implementation error", {
  # note: do we want to limit users from modifying regime and R0 since they are
  # able to modify the recdevs?
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  tmp_future_om_list_4 <- check_future_om_list_str(future_om_list = future_om_list_4)
  tmp_future_om_list_4 <- check_future_om_list_vals(
    future_om_list = tmp_future_om_list_4,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list_4,
    scen_name = "scen3",
    niter = 3,
    om_mod_path = om_path, nyrs = 6
  )
  devs_df <- devs_list[["dev_vals"]]
  # modify the following expectations
  expect_true(nrow(devs_df) == 6)
  expect_equal(colnames(devs_df), c("yrs", "rec_devs", "impl_error"))
  expect_equivalent(devs_df[["yrs"]], 101:106)
  # TODO:add better expectation for recdevs (not sure how this would work for multi area models)
  expect_true(all(devs_df[["rec_devs"]] != 0))
  expect_true(all(devs_df[["impl_error"]] == 1.1))
})

test_that("Creating the devs df works with time series options", {
  # TODO: get this up and running.
  tmp_future_om_list <- vector(mode = "list", length = 2)
  tmp_future_om_list <- lapply(
    tmp_future_om_list,
    function(x) x <- vector(mode = "list", length = 4)
  )
  names(tmp_future_om_list[[1]]) <- c("pars", "scen", "pattern", "input")
  names(tmp_future_om_list[[2]]) <- c("pars", "scen", "pattern", "input")

  # add in vals for M and steepness. Jitter after year 101, with a normal dist.
  tmp_future_om_list[[1]][["pars"]] <- c("NatM_p_1_Fem_GP_1", "SR_BH_steep")
  tmp_future_om_list[[1]][["scen"]] <- c("randomize", "scen1", "scen2")
  tmp_future_om_list[[1]][["pattern"]] <- c("model_change", "normal")
  tmp_future_om_list[[1]][["input"]] <- data.frame(
    first_yr_averaging = NA,
    last_yr_averaging = NA,
    last_yr_orig_val = 101,
    first_yr_final_val = 102,
    ts_param = c("sd", "ar_1_phi"), # The coefficient.
    method = "absolute",
    value = c(0.02, 0.8)
  )


  # add values for selectivity curve param. step change occuring in year 103
  tmp_future_om_list[[2]][["pars"]] <- "NatM_p_1_Fem_GP_1" # had to figure this out from reading in the par file.
  tmp_future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
  tmp_future_om_list[[2]][["pattern"]] <- c("model_change", "normal") # defaults to normal (with SD 0, mean at last yr of mod val?)
  tmp_future_om_list[[2]][["input"]] <- data.frame(
    first_yr_averaging = NA, # NA b/c not using historical values
    last_yr_averaging = NA, # NA b/c not using historical values
    last_yr_orig_val = 106,
    first_yr_final_val = 112, # use 12 years projection
    ts_param = "mean",
    method = "additive", # need to implmeent this option
    value = 0.2
  ) # so at end should be 0.4
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  tmp_future_om_list <- check_future_om_list_str(future_om_list = tmp_future_om_list)
  tmp_future_om_list <- check_future_om_list_vals(
    future_om_list = tmp_future_om_list,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )

  devs_df <- devs_list[["dev_vals"]]
  # modify the following expectations
  expect_true(nrow(devs_df) == 12)
  expect_equal(colnames(devs_df), c("yrs", "NatM_p_1_Fem_GP_1", "SR_BH_steep"))
  expect_equivalent(devs_df[["yrs"]], 101:112)
  # TODO: figure out how to characterize all of the changes. Just put in the 2
  # obvious (I think) values for now.
  expect_equivalent(devs_df[12, "NatM_p_1_Fem_GP_1"], 0.2)
  expect_equivalent(devs_df[1, "SR_BH_steep"], 0)

  # TODO: check that the steepness SR_BH_steep values are sampled correctly.
  # compare sampled values with arima
  # want theses:
  # not quite sure how to do this?
  # devs_list[["abs_vals"]][["SR_BH_steep"]]
  # devs_list[["future_om_list"]][[1]][["seed"]] # the seed
  # set.seed(devs_list[["future_om_list"]][[1]][["seed"]])
  # # first value:
  # #first_val <- rnorm(1, mean = 0, sd = 0)
  # 0.65 + arima.sim(list(order = c(1,1,0), ar = 0.8), n = 11, sd = 0.02)
})

test_that("creating the devs df works with cv", {
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  tmp_future_om_list <- future_om_list_2
  tmp_future_om_list <- check_future_om_list_str(future_om_list = tmp_future_om_list)
  tmp_future_om_list <- check_future_om_list_vals(
    future_om_list = tmp_future_om_list,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_df <- devs_list[["dev_vals"]]
  # modify the following expectations
  expect_true(nrow(devs_df) == 12)
  expect_length(colnames(devs_df), 38)
  expect_equivalent(devs_df[["yrs"]], 101:112)
  base_M <- unique(devs_list[["base_vals"]][["NatM_p_1_Fem_GP_1"]])
  base_sel <- unique(devs_list[["base_vals"]][["SizeSel_P_4_Fishery(1)"]])
  cv_val <- tmp_future_om_list[[1]][["input"]][tmp_future_om_list[[1]][["input"]][["ts_param"]] == "cv", "value"]
  # The following tests could fail, but are unlikely to. Test that the values
  # are within +/- 3 sigma.
  expect_true(sum(devs_df[, "NatM_p_1_Fem_GP_1"] <= (3 * base_M * cv_val)) > 10 &
    sum(devs_df[, "NatM_p_1_Fem_GP_1"] >= (-3 * base_M * cv_val)) > 10)
  expect_true(sum(devs_df[, "SizeSel_P_4_Fishery(1)"] <= (3 * base_sel * cv_val)) > 10 &
    sum(devs_df[, "SizeSel_P_4_Fishery(1)"] >= (-3 * base_sel * cv_val)) > 10)
  # TODO: consider this option more thoroughly. Some of these params shouldn't be
  # simultaneously (e.g., SR parameters and recdevs. What to do about 0 and
  # negative values?)
})


test_that("Creating the devs df works with timevarying pars in initial model", {
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "growth_timevary")
  future_om_list <- check_future_om_list_str(future_om_list = future_om_list)
  future_om_list <- check_future_om_list_vals(
    future_om_list = future_om_list,
    scen_list = scen_list
  )
  test_list <- future_om_list
  test_list[[1]][["input"]][["first_yr_averaging"]] <- 1990
  test_list[[1]][["input"]][["last_yr_averaging"]] <- 2009
  test_list[[1]][["input"]][["last_yr_orig_val"]] <- 2009
  test_list[[1]][["input"]][["first_yr_final_val"]] <- 2010
  test_list[[2]][["pars"]] <- "AgeSel_P_3_Fish1(1)"
  test_list[[2]][["input"]][["first_yr_averaging"]] <- 2000
  test_list[[2]][["input"]][["last_yr_averaging"]] <- 2009
  test_list[[2]][["input"]][["last_yr_orig_val"]] <- 2011
  test_list[[2]][["input"]][["first_yr_final_val"]] <- 2012

  # add another change LnQ_base_Surv1(3), which already has parameter devs.
  tmp_list <- list(
    pars = "LnQ_base_Surv1(3)",
    scen = c("replicate", "scen2"),
    pattern = c("model_change", "normal"),
    input = data.frame(
      first_yr_averaging = 1990,
      last_yr_averaging = 2009,
      last_yr_orig_val = 2011,
      first_yr_final_val = 2012,
      ts_param = "mean",
      method = "multiplier", value = 1.5
    )
  )
  test_list[[3]] <- tmp_list

  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = test_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 10
  )
  dev_vals <- devs_list[["dev_vals"]]
  dev_vals
  # check M are in the range expected
  expect_true(all(dev_vals[["NatM_p_1_Fem_GP_1"]] < 3 * test_list[[1]][["input"]][["value"]]))
  expect_true(all(dev_vals[["NatM_p_1_Fem_GP_1"]] > -3 * test_list[[1]][["input"]][["value"]]))
  # check Sel vals in range expected
  before_change_vals <- devs_list[["abs_vals"]][devs_list[["abs_vals"]][["yrs"]] <= test_list[[2]][["input"]][["last_yr_orig_val"]], "AgeSel_P_3_Fish1(1)"]
  after_change_vals <- devs_list[["abs_vals"]][devs_list[["abs_vals"]][["yrs"]] > test_list[[2]][["input"]][["last_yr_orig_val"]], "AgeSel_P_3_Fish1(1)"]
  base_vals_tmp <- unique(devs_list[["base_vals"]][["AgeSel_P_3_Fish1(1)"]])
  change_vals_tmp <- test_list[[2]][["input"]][["value"]]
  expect_true(all(before_change_vals == base_vals_tmp))
  expect_true(all(after_change_vals == change_vals_tmp))
  # check Q vals in range expected
  expect_true(all(dev_vals[["LnQ_base_Surv1(3)"]][1:2] == 0))
  expect_equivalent(dev_vals[["LnQ_base_Surv1(3)"]][3:10], rep(-0.176569, length.out = 8))
})

test_that("Tests a model with env link using historical values", {
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "SR_env_block")

  tmp_list <- list(list(
    pars = "SR_LN(R0)",
    scen = c("replicate", "scen2"),
    pattern = c("model_change", "normal"),
    input = data.frame(
      first_yr_averaging = 1990,
      last_yr_averaging = 2000,
      last_yr_orig_val = 2001,
      first_yr_final_val = 2002,
      ts_param = "mean",
      method = "multiplier", value = 1.1
    )
  ))
  tmp_list <- check_future_om_list_str(future_om_list = tmp_list)
  tmp_list <- check_future_om_list_vals(
    future_om_list = tmp_list,
    scen_list = scen_list
  )
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 10
  )
  expect_length(unique(devs_list[["dev_vals"]][["SR_LN(R0)"]]), 1)
  dat <- r4ss::SS_readdat(file.path(om_path, "data.ss_new"))
  dat <- dat[["envdat"]]
  env_vals <- dat[dat[["Yr"]] >= tmp_list[[1]][["input"]][["first_yr_averaging"]] &
    dat[["Yr"]] <= tmp_list[[1]][["input"]][["last_yr_averaging"]], "Value"]
  env_parval <- 0.862777 # hard coded based on the model used.
  base_val <- mean(unique(devs_list[["base_vals"]][["SR_LN(R0)"]]) + env_parval * env_vals)
  expect_equivalent(unique(devs_list[["abs_vals"]][["SR_LN(R0)"]]), base_val * 1.1)

  new_change <- list(
    pars = "SR_LN(R0)",
    scen = c("replicate", "scen2"),
    pattern = c("model_change", "normal"),
    input = data.frame(
      first_yr_averaging = 1995,
      last_yr_averaging = 2000,
      last_yr_orig_val = 2005,
      first_yr_final_val = 2007,
      ts_param = "mean",
      method = "multiplier",
      value = 1.2
    )
  )
  tmp_list[[2]] <- new_change
  devs_list <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 10
  )
  expect_length(unique(devs_list[["dev_vals"]][["SR_LN(R0)"]]), 3)
  env_vals <- dat[dat[["Yr"]] >= tmp_list[[2]][["input"]][["first_yr_averaging"]] &
    dat[["Yr"]] <= tmp_list[[2]][["input"]][["last_yr_averaging"]], "Value"]
  env_parval <- 0.862777 # hard coded based on the model used.
  base_val <- mean(unique(devs_list[["base_vals"]][["SR_LN(R0)"]]) + env_parval * env_vals)
  expect_equivalent(unique(devs_list[["abs_vals"]][["SR_LN(R0)"]][6:10]), base_val * 1.2)
})



test_that("Setting seeds works as intended", {
  # check replicate option works
  ext_files <- system.file(package = "SSMSE")
  om_path <- file.path(ext_files, "extdata", "models", "cod")
  tmp_future_om_list <- future_om_list
  tmp_future_om_list[[2]] <- NULL
  tmp_future_om_list <- check_future_om_list_str(future_om_list = tmp_future_om_list)
  tmp_future_om_list <- check_future_om_list_vals(
    future_om_list = tmp_future_om_list,
    scen_list = scen_list
  )
  devs_list_1 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_list_2 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen3",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_list_3 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen3",
    niter = 2,
    om_mod_path = om_path, nyrs = 12
  )
  expect_equal(devs_list_1[["dev_vals"]], devs_list_2[["dev_vals"]])
  # Need each iteration to be different when using replicate
  expect_true(all(devs_list_3[["dev_vals"]][["NatM_p_1_Fem_GP_1"]] !=
    devs_list_2[["dev_vals"]][["NatM_p_1_Fem_GP_1"]]))

  # now, check randomize option works.
  tmp_future_om_list[[1]][["scen"]][1] <- "randomize"

  devs_list_1 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen2",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_list_2 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen3",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_list_2_dup <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen3",
    niter = 1,
    om_mod_path = om_path, nyrs = 12
  )
  devs_list_3 <- convert_future_om_list_to_devs_df(
    future_om_list = tmp_future_om_list,
    scen_name = "scen3",
    niter = 2,
    om_mod_path = om_path, nyrs = 12
  )

  expect_true(all(devs_list_1[["dev_vals"]][["NatM_p_1_Fem_GP_1"]] !=
    devs_list_2[["dev_vals"]][["NatM_p_1_Fem_GP_1"]]))
  # Need each iteration to be different
  expect_true(all(devs_list_3[["dev_vals"]][["NatM_p_1_Fem_GP_1"]] !=
    devs_list_2[["dev_vals"]][["NatM_p_1_Fem_GP_1"]]))
  expect_equal(devs_list_2, devs_list_2_dup) # the same iter and scen should be the same vals.
})
