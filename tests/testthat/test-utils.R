# test utility functions for the packages. Construct objects, move files, etc.
out_dir <- file.path(tempdir(), "test_utils")

test_that("create_scen_list works as expected", {
  # create some simple list that we want them to look like
  scen_list <- list(scen_1 = 
                       list(
                         iter = 1:2,
                         OM_name = "cod",
                         use_SS_boot = TRUE,
                         EM_name = "cod",
                         EM_dir = NULL,
                         MS = "EM",
                         out_dir_scen = out_dir,
                         nyrs = 6,
                         nyrs_assess = 3, 
                         impl_error = NULL,
                         dat_str = NULL
                       ),
                      scen_2 = 
                        list(
                          iter = 1:2,
                          OM_name = "cod",
                          use_SS_boot = TRUE,
                          EM_name = "cod",
                          EM_dir = NULL,
                          MS = "EM",
                          out_dir_scen = out_dir,
                          nyrs = 3,
                          nyrs_assess = 2, 
                          impl_error = NULL,
                          dat_str = NULL
                        )
                       )

 scen_list_out <-  create_scen_list(scen_name_vec = c("scen_1", "scen_2"),
                                    iter_list = list(1:2),
                                    OM_name_vec = "cod", 
                                    use_SS_boot_vec = TRUE,
                                    EM_name_vec = "cod",
                                    EM_dir_vec = NULL,
                                    MS_vec = "EM",
                                    out_dir_scen_vec = out_dir,
                                    nyrs_vec = c(6,3),
                                    nyrs_assess_vec = c(3, 2),
                                    impl_error_vec = NULL,
                                    dat_str_list = NULL
                                    )
 #use expect equal because the names should be the same as well.
 expect_equal(scen_list_out, scen_list)
})

test_that("create_scen_list works with NAs", {
  scen_list <- list(scen_1 = 
                        list(
                          iter = 1:2,
                          OM_name = "cod",
                          use_SS_boot = TRUE,
                          EM_name = "cod",
                          EM_dir = NULL,
                          MS = "EM",
                          out_dir_scen = out_dir,
                          nyrs = 6,
                          nyrs_assess = 3, 
                          impl_error = NULL,
                          dat_str = NULL
                        ),
                      scen_2 = 
                        list(
                          iter = 1:2,
                          OM_name = "cod",
                          use_SS_boot = TRUE,
                          EM_name = NULL,
                          EM_dir = NULL,
                          MS = "no_catch",
                          out_dir_scen = out_dir,
                          nyrs = 3,
                          nyrs_assess = 2, 
                          impl_error = NULL,
                          dat_str = NULL
                        )
  )
  scen_list_out <-  create_scen_list(scen_name_vec = c("scen_1", "scen_2"),
                                     iter_list = list(1:2),
                                     OM_name_vec = "cod", 
                                     use_SS_boot_vec = TRUE,
                                     EM_name_vec = c("cod", NA),
                                     EM_dir_vec = NULL,
                                     MS_vec = c("EM", "no_catch"),
                                     out_dir_scen_vec = out_dir,
                                     nyrs_vec = c(6,3),
                                     nyrs_assess_vec = c(3, 2),
                                     impl_error_vec = NULL,
                                     dat_str_list = NULL
  )
  expect_equal(scen_list_out, scen_list)
})

#TODO: Add checks for error messages.

