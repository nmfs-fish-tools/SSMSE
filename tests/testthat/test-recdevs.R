context("Code in recdevs.R works")

test_that("Build recdevs works as expected for option none", {
  recdevs <- build_rec_devs(rec_dev_pattern = "none", 
                            n_scenarios = 1,
                            yrs = 6, 
                            iter_vec = 3
                            )
  expected_output <- list(list(rep(0, 6), rep(0, 6), rep(0, 6)))
  expect_equal(recdevs, expected_output)
  
  recdevs <- build_rec_devs(rec_dev_pattern = "none",
                            n_scenarios = 2,
                            yrs = c(3,6),
                            iter_vec = c(3,2)
  )
  expected_output <- list(scen_1 = list(rep(0, 3), rep(0, 3), rep(0, 3)), 
                          scen_2 = list(rep(0, 6), rep(0, 6)))
  expect_equivalent(recdevs, expected_output)
})

test_that("Build recdevs works as expected for option rand", {
  iter_vec <- c(3,3)
  seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
  recdevs <- build_rec_devs(rec_dev_pattern = "rand" ,
                            n_scenarios = 2,
                            yrs = c(3,3),
                            iter_vec = iter_vec,
                            scope = 1,
                            rec_dev_pars = c(3, 1),
                            stddev = 0.2,
                            rec_autoCorr = NA,
                            seed = seed_vals)
  expect_length(recdevs, 2)
  expect_length(recdevs[[1]], iter_vec[1])
  # expectations to make sure scope = 1 works
  expect_equivalent(recdevs[[1]], recdevs[[2]])
  # setting seeds works
  second_run <- build_rec_devs(rec_dev_pattern = "rand" ,
                               n_scenarios = 2,
                               yrs = c(3,3),
                               iter_vec = iter_vec,
                               scope = 1,
                               rec_dev_pars = c(3, 1),
                               stddev = 0.2,
                               rec_autoCorr = NA,
                               seed = seed_vals)
  expect_equal(recdevs, second_run)
  
  #test scope = 2
  recdevs_scope_2 <- build_rec_devs(rec_dev_pattern = "rand" ,
                                    n_scenarios = 2,
                                    yrs = c(3,3),
                                    iter_vec = iter_vec,
                                    scope = 2,
                                    rec_dev_pars = c(3, 1),
                                    stddev = 0.2,
                                    rec_autoCorr = NA,
                                    seed = seed_vals)
  expect_true(all(!is.na(unlist(recdevs_scope_2))))
  recdevs_scope_3 <- build_rec_devs(rec_dev_pattern = "rand" ,
                                    n_scenarios = 2,
                                    yrs = c(3,3),
                                    iter_vec = iter_vec,
                                    scope = 3,
                                    rec_dev_pars = c(3, 1),
                                    stddev = 0.2,
                                    rec_autoCorr = NA,
                                    seed = seed_vals)
  expect_true(all(!is.na(unlist(recdevs_scope_3))))


})

# TODO: Add these tests
# 
# test_that("Build recdevs works as expected for option autocorrelated random", {
#   iter_vec <- c(3,2)
#   seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
#   recdevs <- build_rec_devs(rec_dev_pattern = "AutoCorr_rand",
#                             n_scenarios = 2,
#                             yrs = c(3,6),
#                             iter_vec = iter_vec,
#                             scope = 3,
#                             rec_dev_pars = c(3, 1),
#                             stddev = list(0.2, 0.2),
#                             rec_autoCorr = NA,
#                             seed = seed_vals)
#   browser()
#   expect_length(recdevs, 2)
#   expect_length(recdevs[[1]], iter_vec[1])
# })
# 
# test_that("Build recdevs works as expected for option autocorrelated specified", {
#   iter_vec <- c(3,2)
#   seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
#   recdevs <- build_rec_devs(rec_dev_pattern = "AutoCorr_Spec",
#                             n_scenarios = 2,
#                             yrs = c(3,6),
#                             iter_vec = iter_vec,
#                             scope = 3,
#                             rec_dev_pars = c(3, 1),
#                             stddev = 0.2,
#                             rec_autoCorr = NA,
#                             seed = seed_vals)
#   browser()
#   expect_length(recdevs, 2)
#   expect_length(recdevs[[1]], iter_vec[1])
# })
# 
# test_that("Build recdevs works as expected for custom values", {
#   iter_vec <- c(3,2)
#   seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
#   recdevs <- build_rec_devs(rec_dev_pattern = "vector",
#                             n_scenarios = 2,
#                             yrs = c(3,6),
#                             iter_vec = iter_vec,
#                             scope = 3,
#                             rec_dev_pars = c(3, 1),
#                             stddev = 0.2,
#                             rec_autoCorr = NA,
#                             seed = seed_vals)
#   expect_length(recdevs, 2)
#   expect_length(recdevs[[1]], iter_vec[1])
# })
