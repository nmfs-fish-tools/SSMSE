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

#get_input_value tests.
dfr <- data.frame("year" = 1:5, 
                 "value" = c(2,2,2,3,3), 
                 "se_log" = 0.2)
dfr2 <- dfr
colnames(dfr2) <- NULL
test_that("get_input_value generates errors for bad input", {
  expect_error(get_input_value(data = dfr, 
                               method = "m_common_value", 
                               colname = "se_log"), 
               "method was specified as m_common_value")
  expect_error(get_input_value(data = dfr, 
                               method = "most_common_value",
                               colname = "bad_colname"), 
               "bad_colname not found in data")
  expect_error(get_input_value(data = dfr, 
                               method = "most_common_value",
                               colname = "se_log", 
                               group = "bad_groupname"), 
               "column bad_groupname not found")
  expect_error(get_input_value(data = dfr, 
                               method = "only_value",
                               colname = "se_log", 
                               group = "bad_groupname"), 
               "column bad_groupname not found")
  expect_error(get_input_value(data = c(1,2,3),
                               method = "most_common_value",
                               colname = "se_log"),
               "data is not of class 'data.frame'")
  expect_error(get_input_value(data = dfr2, 
                               method = "most_common_value",
                               colname = "se_log"), 
               "The column names of data are NULL")
  expect_error(get_input_value(data = dfr,
                               method = 1, 
                               colname = "se_log"), 
               "method is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = 2),
               "colname is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = "se_log", 
                               group = 3),
               "group is not of class 'character'")
  expect_error(get_input_value(data = dfr,
                               method = c("most_common_value", "only_value"),
                               colname = "se_log"),
               "method has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               colname = c("val", "se_log")), 
               "colname has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               group = c("se_log", "value"),
                               colname = "val"), 
               "group has length 2, not 1")
  expect_error(get_input_value(data = dfr,
                               method = "most_common_value",
                               group = "value",
                               colname = "value"), 
               "group and colname cannot be the same")
  
})

test_that("get_input_value works as expected", {
  val <- get_input_value(data = dfr,
                         method = "most_common_value", 
                         colname = "se_log")
  expect_equal(val, 0.2)
  val <- get_input_value(data = dfr,
                         method = "most_common_value", 
                         colname = "value")
  expect_equal(val, 2)
  val <- get_input_value(data = dfr,
                         method = "only_value",
                         colname = "se_log")
  expect_equal(val, 0.2)
  # use group
  val <- get_input_value(data = dfr,
                         method = "only_value",
                         colname = "se_log", 
                         group = "value")
  expect_true(is.data.frame(val))
  expect_true(all(val$value %in% c(2, 3)))
  expect_true(unique(val$se_log) == 0.2)
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "se_log", 
                         group = "value")
  expect_true(is.data.frame(val))
  expect_true(all(val$value %in% c(2, 3)))
  expect_true(unique(val$se_log) == 0.2)
  #switch the cols used
  val <- get_input_value(data = dfr,
                         method = "most_common_value",
                         colname = "value", 
                         group = "se_log")
  expect_true(is.data.frame(val))
  expect_true(nrow(val) == 1)
  expect_true(val$value == 2)
  expect_true(val$se_log == 0.2)
  
  expect_error(get_input_value(data = dfr, 
                               method = "only_value", 
                               colname = "value"), 
               "Multiple unique values were found in data")
  dfr$new_cat <- c("G1", "G2", "G1", "G1","G1")
  expect_error(get_input_value(data = dfr, 
                               method = "only_value", 
                               colname = "value", 
                               group = "new_cat"),
               "Multiple unique values were found in data")
})

