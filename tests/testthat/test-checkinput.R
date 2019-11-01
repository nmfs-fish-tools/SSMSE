context("Test functions in R script checkinput.R")

extdat_path <- system.file("extdata", package = "SSMSE")

test_that("check_catch_df works", {
  good_df <- data.frame(year     = 1:5, 
                        seas   = 1,
                        fleet    = 1,
                        catch    = seq(100, 500, length.out = 5),
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