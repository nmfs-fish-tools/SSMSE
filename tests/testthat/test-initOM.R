context("Test functions in R script initialize OM")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-initOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
file.copy(file.path(extdat_path, "models", "cod"),
  temp_path,
  recursive = TRUE
)

test_that("create_OM can modify model", {
  skip_on_cran() # because runs ss.
  new_dat <- create_OM(
    OM_out_dir = file.path(temp_path, "cod"),
    overwrite = TRUE,
    verbose = FALSE,
    nyrs_assess = 3,
    rec_devs = rep(0, length = 6),
    verify_OM = TRUE
  ) # use the runtime check also.
  # note there are 2 NAs introduced by coercion warnings from SS_output that
  # are safe to ignore.
  dat_types <- c("CPUE", "lencomp", "agecomp")
  new_dat_yrs <- lapply(dat_types, function(type, datlist) {
    unique_yrs <- unique(datlist[[type]][, 1])
    ordered_yrs <- unique_yrs[order(unique_yrs)]
  },
  datlist = new_dat
  )
  names(new_dat_yrs) <- dat_types
  # check that no duplicated data
  expect_equal(
    new_dat$CPUE[, c("year", "seas", "index")],
    unique(new_dat$CPUE[, c("year", "seas", "index")])
  )
  cols_lencomp <- c("Yr", "Seas", "FltSvy", "Gender", "Part")
  expect_equal(
    new_dat$lencomp[, cols_lencomp],
    unique(new_dat$lencomp[, cols_lencomp])
  )
  cols_agecomp <- c(
    "Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo",
    "Lbin_hi"
  )
  expect_equal(
    new_dat$agecomp[, cols_agecomp],
    unique(new_dat$agecomp[, cols_agecomp])
  )
  # check that a valid model is produced by seeing if it can run.
  start <- r4ss::SS_readstarter(file.path(temp_path, "cod", "starter.ss"))
  expect_true(start$init_values_src == 1) # b/c should be running from.par.
  file.remove(file.path(temp_path, "cod", "control.ss_new"))
  run_ss_model(file.path(temp_path, "cod"), "-maxfn 0 -phase 50 -nohess",
    verbose = FALSE
  )
  expect_true(file.exists(file.path(temp_path, "cod", "control.ss_new")))
})

test_that("rm_sample_struct_hist works", {
  dat_path <- system.file("extdata", "models", "cod", "ss3.dat",
                             package = "SSMSE")
  dat <- r4ss::SS_readdat(dat_path, verbose = FALSE)
  CPUE_samp <- dat[["CPUE"]]
  # note: all these years are present in the initial dataset
  CPUE_samp <- CPUE_samp[CPUE_samp[["year"]] %in% c(30,40,50),
                         c("year", "seas", "index", "se_log")]
  lencomp_samp <- dat[["lencomp"]]
  # Note: only year 80 in present in the initial dataset
  lencomp_samp <- lencomp_samp[lencomp_samp[["Yr"]] %in% c(70, 80, 90), 1:6]
  agecomp_samp <- dat[["agecomp"]]
  # Note: only years 50 and 70 are present in the initial dataset
  agecomp_samp <- agecomp_samp[agecomp_samp[["Yr"]] %in% c(50, 62, 70),1:9]
  # note this list uses the r4ss names instead of the user input names,
  # as this function will be run when the user input has already been 
  # transformed to the r4ss names.
  sample <- list(CPUE = CPUE_samp,
                  lencomp = lencomp_samp,
                  agecomp = agecomp_samp)
  new_dat <- rm_sample_struct_hist(sample_struct_hist = sample, dat = dat)
  expect_true(nrow(new_dat[["CPUE"]]) == 3)
  expect_true(nrow(new_dat[["lencomp"]]) == 1)
  expect_true(nrow(new_dat[["agecomp"]]) == 2)
  # removing vals for only CPUE, and no values at all.
  new_dat <- rm_sample_struct_hist(sample_struct_hist = list(CPUE = CPUE_samp),
                                   dat = dat)
  expect_true(nrow(new_dat[["CPUE"]]) == 3)
  expect_true(nrow(new_dat[["lencomp"]]) == nrow(dat[["lencomp"]]))
  expect_true(nrow(new_dat[["agecomp"]]) == nrow(dat[["agecomp"]]))
  # remove no vals
  new_dat <- rm_sample_struct_hist(sample_struct_hist = NULL, dat = dat)
  expect_equal(new_dat,dat)
  # test if data doesn't have any lencomp
  dat[["lencomp"]] <- NULL
  new_dat <- rm_sample_struct_hist(sample_struct_hist = sample, dat = dat)
  expect_true(nrow(new_dat[["CPUE"]]) == 3)
  expect_true(is.null(new_dat[["lencomp"]]))
  expect_true(nrow(new_dat[["agecomp"]]) == 2)
})
