context("test function in sample_struct.R")

out_dir <- file.path(tempdir(), "test_sample_struct")
dir.create(out_dir)
on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
OM_dat_path <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")

test_that("assumptions about r4ss colnames are true.", {
  # if this test is not passing, modifications need to be made to
  # convert_to_r4ss_names function.
  # mock some values outside modle year in the modesl
  OM_dat_orig <-   dat_all_types <- r4ss::SS_readdat(
    system.file("extdata", "test_dat_all_types.dat", package = "SSMSE"))
  r4ss_names <- list(
    catch = colnames(OM_dat_orig[["catch"]]),
    CPUE = colnames(OM_dat_orig[["CPUE"]]),
    lencomp = colnames(OM_dat_orig[["lencomp"]]),
    agecomp = colnames(OM_dat_orig[["agecomp"]]),
    meanbodywt = colnames(OM_dat_orig[["meanbodywt"]]),
    MeanSize_at_Age_obs = colnames(OM_dat_orig[["MeanSize_at_Age_obs"]])
  )
  assumed_str <- list(
    catch = data.frame(year = 101:106, seas = 1, fleet = 1, catch_se = 0.005),
    CPUE = data.frame(year = c(102, 105), seas = 7, index = 2, se_log = 0.2),
    lencomp = data.frame(
      Yr = c(102, 105), Seas = 1, FltSvy = 1,
      Gender = 0, Part = 0, Nsamp = 125
    ),
    agecomp = data.frame(
      Yr = c(102, 105), Seas = 1, FltSvy = 2,
      Gender = 0, Part = 0, Ageerr = 1,
      Lbin_lo = -1, Lbin_hi = -1, Nsamp = 500
    ),
    meanbodywt = data.frame(
      Year = c(1999, 1999, 2001, 2001),
      Seas = 7,
      Fleet = c(1,2,1,2),
      Partition = 1,
      Type = 1,
      Std_in = 0.3
    ),
    MeanSize_at_Age_obs = data.frame(
      Yr = c(1971, 1995), 
      Seas = 7,
      FltSvy = c(1,1,2,2), 
      Gender = 3,
      Part = 0,
      AgeErr = 1
    )
  )
  return <- check_sample_struct(sample_struct = assumed_str, valid_names = r4ss_names)
  expect_equal(return, "no_error")
})

test_that("convert_to_r4ss_names works", {
  test_sample_struct <- list(
    catch = data.frame(Yr = 101:106, Seas = 1, FltSvy = 1, SE = 0.01),
    CPUE = data.frame(Yr = c(102, 105), Seas = 7, FltSvy = 2, SE = 0.05),
    lencomp = data.frame(
      Yr = c(102, 105), Seas = 1, FltSvy = 1,
      Sex = 0, Part = 0, Nsamp = 125
    ),
    agecomp = data.frame(
      Yr = c(102, 105), Seas = 1, FltSvy = 2,
      Sex = 0, Part = 0, Ageerr = 1,
      Lbin_lo = -1, Lbin_hi = -1, Nsamp = 500
    )
  )
  r4ss_sample_struct <- convert_to_r4ss_names(test_sample_struct)
  expect_equal(names(r4ss_sample_struct), names(test_sample_struct))
  expect_equal(names(r4ss_sample_struct[["catch"]]), c("year", "seas", "fleet", "catch_se"))
  expect_equal(names(r4ss_sample_struct[["CPUE"]]), c("year", "seas", "index", "se_log"))
  expect_equal(names(r4ss_sample_struct[["lencomp"]]), c(
    "Yr", "Seas", "FltSvy",
    "Gender", "Part", "Nsamp"
  ))
  expect_equal(names(r4ss_sample_struct[["agecomp"]]), c(
    "Yr", "Seas", "FltSvy",
    "Gender", "Part", "Ageerr",
    "Lbin_lo", "Lbin_hi", "Nsamp"
  ))
})


test_that("create_sample_struct works", {
  # correctly working
  expect_warning(
    sample_struct <- create_sample_struct(OM_dat_path, nyrs = 20),
    "Pattern not found for lencomp"
  )
  expect_sample_struct <- list(
    catch = data.frame(Yr = 101:120, Seas = 1, FltSvy = 1, SE = 0.005),
    CPUE = data.frame(Yr = seq(105, 120, by = 5), Seas = 7, FltSvy = 2, SE = 0.2),
    lencomp = data.frame(Yr = NA, Seas = 1, FltSvy = 1, Sex = 0, Part = 0, Nsamp = 125), # because irregular input
    agecomp = data.frame(
      Yr = seq(105, 120, by = 5), Seas = 1, FltSvy = 2,
      Sex = 0, Part = 0, Ageerr = 1, Lbin_lo = -1,
      Lbin_hi = -1, Nsamp = 500
    ), 
    meanbodywt = NA,
    MeanSize_at_Age_obs = NA
  )
  expect_equal(sample_struct, expect_sample_struct)
  # try using one where missing lencomp data
  dat <- r4ss::SS_readdat(system.file("extdata", "models", "cod", "ss3.dat",
    package = "SSMSE"
  ), verbose = FALSE)
  dat$lencomp <- NULL
  sample_nolen <- create_sample_struct(dat, nyrs = 20)
  expect <- expect_sample_struct
  expect$lencomp <- NA
  expect_equal(sample_nolen, expect)
  # give bad input
  expect_error(
    create_sample_struct(OM_dat_path, nyrs = "twenty"),
    "nyrs is not of class 'numeric'"
  )
  # make sure NA in sampling col works as expected.
  dat$catch$catch_se[5] <- 0.01
  expect_warning(
    sample_struct_mult_SE <- create_sample_struct(dat, nyrs = 20),
    "NA included in column"
  )
  expect_mult_SE <- expect
  expect_mult_SE$catch$SE <- NA
  expect_equal(sample_struct_mult_SE, expect_mult_SE)
  # todo: maybe need to expand this to account for CAL data?

  # test that create_sample_structure works when a pattern found but there is
  # no data to add for the nyrs specified
  expect_warning(sample_struct_no_dat <-
    create_sample_struct(OM_dat_path, nyrs = 1))
  expect_equivalent(
    sample_struct_no_dat[["CPUE"]],
    data.frame(Yr = NA, Seas = 7, FltSvy = 2, SE = 0.2)
  )
})

test_that("get_full_sample_struct works", {
  spl_str <- list(
    catch = data.frame(Yr = 101:110),
    CPUE = data.frame(Yr = c(105, 110)),
    lencomp = data.frame(Yr = seq(102, 110, by = 2)),
    agecomp = data.frame(Yr = seq(102, 110, by = 4))
  )
  full_spl_str <- get_full_sample_struct(
    spl_str,
    system.file("extdata", "models", "cod", package = "SSMSE")
  )
  # reference to compare against
  full_spl_str_ref <- list(
    catch = data.frame(Yr = 101:110, Seas = 1, FltSvy = 1, SE = 0.005),
    CPUE = data.frame(Yr = c(105, 110), Seas = 7, FltSvy = 2, SE = 0.2),
    lencomp = data.frame(
      Yr = seq(102, 110, by = 2), Seas = 1, FltSvy = 1,
      Sex = 0, Part = 0, Nsamp = 125
    ),
    agecomp = data.frame(
      Yr = seq(102, 110, by = 4), Seas = 1, FltSvy = 2,
      Sex = 0, Part = 0, Ageerr = 1, Lbin_lo = -1,
      Lbin_hi = -1, Nsamp = 500
    )
  )
  expect_equal(full_spl_str, full_spl_str_ref)

  # TODO: add some more complex examples to verify that this will work in all
  # situations that it should?
})

test_that("sample_str works with other data types", {
  dat_all_types <- r4ss::SS_readdat(
    system.file("extdata", "test_dat_all_types.dat", package = "SSMSE"))
  # can add this later if want to add generalized size comp sampling. Has not
  # yet been added.
  # dat_gen_size_comp <- r4ss::SS_readdat(
  #   system.file("extdata","test_dat_gen_size_comp.ss", package = "SSMSE"))
  # TODO: make type of the rows/columns standardized?
  struct <- create_sample_struct(dat_all_types, nyrs = 5)
  expect_equivalent(struct[["meanbodywt"]], data.frame(Yr = c(2003, 2005), 
                                                  Seas = 7,
                                                  FltSvy = c(1,1,2,2),
                                                  Part = 1,
                                                  Type = 1,
                                                  SE = 0.3))
  expect_equivalent(struct[["MeanSize_at_Age_obs"]], 
               data.frame(Yr = as.logical(NA), 
                          Seas = 7,
                          FltSvy = c(1,2), 
                          Sex = 3,
                          Part = 0, 
                          Ageerr = 1,
                          Nsamp = 20))
  })


