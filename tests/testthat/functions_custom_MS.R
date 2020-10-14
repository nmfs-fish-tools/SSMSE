run_test_custom_MP <- function(OM_out_dir, OM_dat, nyrs_assess, MS, ...) {
  new_catch_list <- get_no_EM_catch_df(OM_dir = OM_out_dir,
                                       yrs = (OM_dat$endyr + 1):(OM_dat$endyr + nyrs_assess),
                                       MS = 'no_catch')
}