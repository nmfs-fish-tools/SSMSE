context("Test functions in R script checkinput.R")

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("check_catch_df works", {
  good_df <- data.frame(year = 1:5,
                        seas = 1,
                        fleet = 1,
                        catch = seq(100, 500, length.out = 5),
                        catch_se = 0.05)
  out <- check_catch_df(good_df)
  expect_equal(good_df, out)

  bad_col_df <- good_df
  colnames(bad_col_df)[1] <- "years"
  expect_error(check_catch_df(bad_col_df),
               "The catch data frame does not have the correct column names")
})

test_that("check_dir works", {
  good_dir <- file.path(extdat_path, "models", "cod")
  bad_dir <- file.path(extdat_path, "models")
  out <- check_dir(good_dir)
  expect_equal(out, good_dir)
  expect_error(check_dir(bad_dir),
               "Please change to a directory containing a valid SS model")
})

test_that("check_OM_dat works", {
  OM_dat <- r4ss::SS_readdat(file.path(extdat_path, "models", "cod", "ss3.dat"),
                   verbose = FALSE)
  EM_dat <- OM_dat # make the same so know that there should be no errors
  return_dat <- check_OM_dat(OM_dat = OM_dat, EM_dat = EM_dat)
  expect_equal(OM_dat, return_dat)
  # Check that exits on error if wrong years
  OM_dat_wrong_yr <- OM_dat
  OM_dat_wrong_yr$endyr <- 80 # should go to 100
  expect_error(check_OM_dat(OM_dat_wrong_yr, EM_dat),
               "OM_dat and EM_dat should have the same start and end years.")
  # check that exits on error if missing lcomps columns in OM
  OM_dat_miss_lcompcol <- OM_dat
  OM_dat_miss_lcompcol$lencomp <- OM_dat_miss_lcompcol$lencomp[, -7]
  expect_error(check_OM_dat(OM_dat_miss_lcompcol, EM_dat),
               "Column names for length composition were not the same")
  # check that exits on error if missing agecomp cols in OM
  OM_dat_miss_agecompcol <- OM_dat
  OM_dat_miss_agecompcol$agecomp <- OM_dat_miss_agecompcol$agecomp[, -10]
  expect_error(check_OM_dat(OM_dat_miss_agecompcol, EM_dat),
               "Column names for age composition were not the same")
  # test different version of same error together: missing values in CPUE,
  # lcomps, and agecomps
  OM_dat_miss_CPUE <- OM_dat
  OM_dat_miss_CPUE$CPUE <- OM_dat_miss_CPUE$CPUE[-c(1, 2), ] # remove first 2
  # check that exits on error if missing lcomps in OM
  OM_dat_miss_lcomp <- OM_dat
  OM_dat_miss_lcomp$lencomp <- OM_dat_miss_lcomp$lencomp[-1, ]
  # check that exits on error if missing agecomps in OM
  OM_dat_miss_agecomp <- OM_dat
  OM_dat_miss_agecomp$agecomp <- OM_dat_miss_agecomp$agecomp[-1, ]
  OM_dat_list <- list(OM_dat_miss_CPUE, OM_dat_miss_lcomp, OM_dat_miss_agecomp)
  # test together
  lapply(OM_dat_list, function(x) {
    expect_error(check_OM_dat(x, EM_dat),
                 "The OM_dat does not include all values")
  })

})

test_that("check_dat_str works", {
  # works with all inputs
  good_dat_str <- list(
    catch = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1),
    CPUE = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2),
    lencomp = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1, Sex = 0,
                         Part = 0),
    agecomp = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2, Sex = 0,
                         Part = 0, Ageerr = 1, Lbin_lo = -1, Lbin_hi = -1))
  out <- check_dat_str(good_dat_str)
  expect_equal(out, "no_error")
  # works with only 2 cols
  good_dat_str_2_col <- list(
    catch = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1),
    CPUE = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2))
  out_2 <- check_dat_str(good_dat_str_2_col)
  expect_equal(out, "no_error")
  # works when cols in different order
  good_dat_diff_order <- list(
    CPUE  = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2),
    catch = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1)
  )
  out_diff_order <- check_dat_str(good_dat_diff_order)
  expect_equal(out, "no_error")
  # wrong column names
  wrong_colnames <- good_dat_str
  colnames(wrong_colnames[[1]]) <- c("YEAR", "SEAS", "FLEET")
  expect_error(check_dat_str(wrong_colnames),
    "Invalid input for dat_str due to wrong column names in list component",
    fixed = TRUE)
  # duplicate names
  dup_names <- list(
    catch = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1),
    CPUE  = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2),
    CPUE  = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2)
  )
  expect_error(check_dat_str(dup_names), "There are repeated names in dat_str.",
               fixed = TRUE)
  # wrong name
  wrong_name <- list(
    catch = data.frame(Yr = 2000:2002, Seas = 1, FltSvy = 1),
    CPUE_wrong = data.frame(year = 2000:2002, Seas = 7, FltSvy = 2))
  expect_error(check_dat_str(wrong_name),
               "Invalid input for dat_str due to wrong list name",
               fixed = TRUE)
  # character strings included in a dataframe column
  chars <- list(
    catch = data.frame(Yr = 2000:2002, Seas = "wrong", FltSvy = 1),
    CPUE  = data.frame(Yr = 2000:2002, Seas = 7, FltSvy = 2))
  expect_error(check_dat_str(chars),
               "Some values in dat_str are not integers or numeric.",
               fixed = TRUE)
})

test_that("r4ss_obj_err prints errors as expected", {
  expect_error(r4ss_obj_err("my_object", "data list"),
               paste0("my_object was found to not be an r4ss data list. Please",
                     " read in my_object using r4ss read functions."),
                     fixed = TRUE)
})
