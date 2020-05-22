context("test function in sample_struct.R")

out_dir <- file.path(tempdir(), "test_sample_struct")
dir.create(out_dir)
on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
OM_dat_path <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")

test_that("assumptions about r4ss colnames are true.", {
  # if this test is not passing, modifications need to be made to 
  # convert_to_r4ss_names function.
  # mock some values outside modle year in the modesl
  OM_dat_orig <- r4ss::SS_readdat(OM_dat_path, verbose = FALSE)
  r4ss_names <- list(
    catch = colnames(OM_dat_orig[["catch"]]),
    CPUE = colnames(OM_dat_orig[["CPUE"]]), 
    lencomp = colnames(OM_dat_orig[["lencomp"]]), 
    agecomp = colnames(OM_dat_orig[["agecomp"]]))
  assumed_str <- list(
    catch = data.frame(year = 101:106, seas = 1, fleet = 1),
    CPUE = data.frame(year = c(102, 105), seas = 7, index = 2),
    lencomp = data.frame(Yr = c(102, 105), Seas = 1, FltSvy = 1,
                         Gender = 0, Part = 0),
    agecomp = data.frame(Yr = c(102, 105), Seas = 1, FltSvy = 2,
                         Gender = 0, Part = 0, Ageerr = 1,
                         Lbin_lo = -1, Lbin_hi = -1))
  return <- check_sample_struct(sample_struct = assumed_str, valid_names = r4ss_names)
  expect_equal(return, "no_error")
})

test_that("convert_to_r4ss_names works", {
  test_sample_struct <- list(
    catch = data.frame(Yr = 101:106, Seas = 1, FltSvy = 1),
    CPUE = data.frame(Yr = c(102, 105), Seas = 7, FltSvy = 2),
    lencomp = data.frame(Yr = c(102, 105), Seas = 1, FltSvy = 1,
                         Sex = 0, Part = 0),
    agecomp = data.frame(Yr = c(102, 105), Seas = 1, FltSvy = 2,
                         Sex = 0, Part = 0, Ageerr = 1,
                         Lbin_lo = -1, Lbin_hi = -1)
  )
  r4ss_sample_struct <- convert_to_r4ss_names(test_sample_struct)
  expect_equal(names(r4ss_sample_struct), names(test_sample_struct))
  expect_equal(names(r4ss_sample_struct[["catch"]]), c("year", "seas", "fleet"))
  expect_equal(names(r4ss_sample_struct[["CPUE"]]), c("year", "seas", "index"))
  expect_equal(names(r4ss_sample_struct[["lencomp"]]), c("Yr", "Seas", "FltSvy", 
                                                         "Gender", "Part"))
  expect_equal(names(r4ss_sample_struct[["agecomp"]]), c("Yr", "Seas", "FltSvy", 
                                                         "Gender", "Part", "Ageerr",
                                                         "Lbin_lo", "Lbin_hi"))
})


test_that("create_sample_struct works", {
  # correctly working
  expect_warning(sample_struct <- create_sample_struct(OM_dat_path, nyrs = 20), 
                 "Pattern not found for lencomp")
  expect_sample_struct <- list(catch = data.frame(Yr = 101:120, Seas = 1, FltSvy = 1),
                               CPUE = data.frame(Yr = seq(105, 120, by = 5), Seas = 7, FltSvy = 2),
                               lencomp = data.frame(Yr = NA, Seas = 1, FltSvy = 1), # because irregular input
                               agecomp = data.frame(Yr = seq(105, 120, by = 5 ), Seas = 1, FltSvy = 2))
  expect_equal(sample_struct, expect_sample_struct)
  # try using one where missing lencomp data
  dat <- r4ss::SS_readdat(system.file("extdata", "models", "cod", "ss3.dat",
                                      package = "SSMSE"), verbose  = FALSE)
  dat$lencomp <- NULL
  sample_nolen <- create_sample_struct(dat, nyrs = 20)
  expect <- expect_sample_struct
  expect$lencomp <- NULL
  expect_equal(sample_struct, expect_sample_struct)
  # give bad input
  expect_error(create_sample_struct(OM_dat_path, nyrs = "twenty"),
               "nyrs is not of class 'numeric'")
  # todo: maybe need to expand this to account for CAL data?
})

test_that("get_full_sample_struct works", {
  spl_str <- list(
                 catch = data.frame(Yr = 101:110),
                 CPUE = data.frame(Yr = c(105, 110)), 
                 lencomp = data.frame(Yr = seq(102,110, by = 2)),
                 agecomp = data.frame(Yr = seq(102,110, by = 4)))
 full_spl_str <- get_full_sample_struct(spl_str, 
   system.file("extdata", "models", "cod", package = "SSMSE"))
 # reference to compare against
 full_spl_str_ref <- list(
   catch = data.frame(Yr = 101:110, Seas = 1, FltSvy = 1),
   CPUE = data.frame(Yr = c(105, 110), Seas = 7 , FltSvy = 2), 
   lencomp = data.frame(Yr = seq(102,110, by = 2), Seas = 1, FltSvy = 1, 
                        Sex = 0, Part = 0),
   agecomp = data.frame(Yr = seq(102,110, by = 4), Seas = 1, FltSvy = 2,
                        Sex = 0, Part = 0, Ageerr = 1, Lbin_lo = -1,
                        Lbin_hi = -1))
 expect_equal(full_spl_str, full_spl_str_ref)
 
 # TODO: add some more complex examples to verify that this will work in all
 # situations that it should?
 
})


