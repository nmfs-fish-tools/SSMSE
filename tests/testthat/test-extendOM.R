context("Test functions in extendOM.R script")

# create a temporary location to avoid adding files to the repo.
temp_path <- file.path(tempdir(), "test-extendOM")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

extdat_path <- system.file("extdata", package = "SSMSE")
cod_mod <- file.path(extdat_path, "models", "cod")
# copy cod to the temp_path
file.copy(cod_mod, temp_path, recursive = TRUE)
dat <- r4ss::SS_readdat(file.path(temp_path, "cod", "ss3.dat"))
# create a catch dataframe to add to add to the model
# just repeat the catch and se from the last year.
new_catch <- data.frame(
              year  = (dat$endyr+1):(dat$endyr+3),
              seas  = unique(dat$catch$seas)[1],
              fleet = unique(dat$catch$fleet)[1],
              catch = dat$catch$catch[nrow(dat$catch)],
              catch_se = dat$catch$catch_se[nrow(dat$catch)]
             )
new_yrs <- new_catch$year
test_that("extend_OM works with simple case", {
  # simple case: 1 fleet and season needs CPUE, lencomp, agecomp added
  return_dat <- extend_OM(catch = new_catch, 
                            discards = NULL,
                            OM_dir = file.path(temp_path, "cod"),
                            dummy_dat_scheme = "all",
                            nyrs = 3,
                          write_dat = FALSE,
                            verbose = FALSE)
  # check catch
  new_rows <- return_dat$catch[
                (nrow(return_dat$catch)-nrow(new_catch)+1):nrow(return_dat$catch), ]
  lapply(colnames(new_catch), function(x) {
    expect_equal(new_rows[, x], new_catch[, x])
  })
  # check CPUE
  expect_equivalent(
    return_dat$CPUE[return_dat$CPUE$year %in% new_yrs, "year"],
                    new_yrs)
  # check lencomp
  expect_equivalent(
    return_dat$lencomp[return_dat$lencomp$Yr %in% new_yrs, "Yr"], 
                    new_yrs)
  # check agecomp
  expect_equivalent(
    return_dat$agecomp[return_dat$agecomp$Yr %in% new_yrs, "Yr"],
                    new_yrs)
})

test_that("extend_OM works with more complicated cases", {
  # add more complex data to OM_dir: multiple fleets for CPUE, lencomp, agecomp
  complex_dat <- dat
  # make the new lines for the data.frame
  new_CPUE <- data.frame(year = c(40, 45), seas = 1, index = 1, obs = 100, 
                        se_log = 0.2)
  complex_dat$CPUE <- rbind(complex_dat$CPUE, new_CPUE)
  #lencomp
  new_lencomp <- data.frame(Yr = c(40, 45), Seas = 7, FltSvy = 2, Gender = 0,
                            Part = 0, Nsamp = 125)
  names_lencomp <- colnames(complex_dat$lencomp)[7:ncol(complex_dat$lencomp)]
  lencomp_dat <- matrix(1, nrow = nrow(new_lencomp), ncol = length(names_lencomp))
  colnames(lencomp_dat) <- names_lencomp
  new_lencomp <- cbind(new_lencomp, as.data.frame(lencomp_dat))
  complex_dat$lencomp <- rbind(complex_dat$lencomp, new_lencomp)
  # agecomp
  new_agecomp <- data.frame(Yr = c(40, 45), Seas = 7, FltSvy = 2, Gender = 0,
                            Part = 0, Ageerr = 0, Lbin_lo = -1, Lbin_hi = -1, 
                            Nsamp = 125)
  names_agecomp <- colnames(complex_dat$agecomp)[10:ncol(complex_dat$agecomp)]
  agecomp_dat <- matrix(1, nrow = nrow(new_agecomp), ncol = length(names_agecomp))
  colnames(agecomp_dat) <- names_agecomp
  new_agecomp <- cbind(new_agecomp, as.data.frame(agecomp_dat))
  complex_dat$agecomp <- rbind(complex_dat$agecomp, new_agecomp)
  r4ss::SS_writedat(complex_dat, file.path(temp_path, "cod", "ss3.dat"), 
                    overwrite = TRUE, verbose = FALSE)
  # run function
  return_dat <- extend_OM(catch = new_catch, 
                          discards = NULL,
                          OM_dir = file.path(temp_path, "cod"),
                          dummy_dat_scheme = "all",
                          nyrs = 3,
                          write_dat = FALSE,
                          verbose = FALSE)
  new_lencomp <- return_dat$CPUE[return_dat$CPUE$year %in% 101:103, ]
  dat_name <-  c("CPUE", "lencomp", "agecomp")
  yr_names <-  c("year", "Yr", "Yr")
  mapply(function(dat_name,yr_name, return_dat) {
    new_dat <- return_dat[[dat_name]]
    new_dat_sub <- new_dat[new_dat[, yr_name] %in% 101:103, ]
    expect_equivalent(nrow(new_dat_sub), 6)
    expect_true(length(unique(new_dat_sub[,yr_name])) == 3)
    invisible(dat_name)
  },
  dat_name = dat_name, yr_name = yr_names, 
  MoreArgs = list(return_dat = return_dat), 
  SIMPLIFY = FALSE)
})

test_that("extend_OM exits on error when it should", {
  # nyrs is too small (needs to be at least 3)
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(catch = new_catch, 
                         OM_dir = file.path(temp_path, "cod"),
                         nyrs = 1, 
                         verbose = FALSE), 
               "The maximum year input for catch")
  # Missing a column in the catch dataframe
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch[, -1], OM_dir = file.path(temp_path, "cod"), 
                         nyrs = 3),
               "The catch data frame does not have the correct")
  # wrong column in the catch dataframe
  alt_new_catch <- new_catch
  colnames(alt_new_catch)[1] <- "wrongname"
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(alt_new_catch, OM_dir = file.path(temp_path, "cod"), 
                         nyrs = 3),
               "The catch data frame does not have the correct")
  #path does not lead to model
  unlink(file.path(temp_path, "cod"), recursive = TRUE)
  file.copy(cod_mod, temp_path, recursive = TRUE)
  expect_error(extend_OM(new_catch, OM_dir = temp_path, nyrs = 3),
               "Please change to a directory containing a valid SS model")
})

test_that("check_future_catch works", {
  # doesn't flag anything
  return_catch <- check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"))
  expect_equal(return_catch, new_catch)
  summary <- r4ss::SS_read_summary(file.path(temp_path, "cod", "ss_summary.sso"))
  summary <- summary$biomass
  large_catch <- new_catch
  large_catch_val <- summary["TotBio_100", "Value"] + 1000
  large_catch[2,"catch"] <- large_catch_val
  expect_error(check_future_catch(catch = large_catch, 
                                  OM_dir = file.path(temp_path, "cod")), 
               "Some input values for future catch are higher")
  # catch has the wrong year
  wrong_yr_catch <- new_catch
  wrong_yr_catch[2,"year"] <- 5
  expect_error(check_future_catch(catch = wrong_yr_catch, 
                                  OM_dir = file.path(temp_path, "cod")), 
               "The highest year for which TotBio")
  # function cannot be used for other catch_units besides "bio"
  expect_error(check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"),
                                  catch_units = "num"), 
               "Function not yet implemented when catch is not in biomass")
  expect_error(check_future_catch(catch = new_catch, 
                                  OM_dir = file.path(temp_path, "cod"),
                                  catch_units = "wrong_value"), 
               "Function not yet implemented when catch is not in biomass")
})