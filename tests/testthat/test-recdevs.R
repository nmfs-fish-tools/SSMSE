context("Code in recdevs.R works")

test_that("Build recdevs works as expected for option none", {
  recdevs <- build_rec_devs(
    rec_dev_pattern = "none",
    yrs = 6,
    iter_vec = 3
  )
  expected_output <- list(list(rep(0, 6), rep(0, 6), rep(0, 6)))
  expect_equal(recdevs, expected_output)

  recdevs <- build_rec_devs(
    rec_dev_pattern = "none",
    yrs = c(3, 6),
    iter_vec = c(3, 2)
  )
  expected_output <- list(
    scen_1 = list(rep(0, 3), rep(0, 3), rep(0, 3)),
    scen_2 = list(rep(0, 6), rep(0, 6))
  )
  expect_equivalent(recdevs, expected_output)
})

test_that("Build recdevs works as expected for option rand", {
  iter_vec <- c(3, 3)
  seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
  recdevs <- build_rec_devs(
    rec_dev_pattern = "rand",
    yrs = c(3, 3),
    iter_vec = iter_vec,
    scope = 1,
    rec_dev_pars = c(3, 1),
    stddev = rep(0.2, 2), # which value is used if scope = 1?
    rec_autoCorr = NULL,
    seed = seed_vals
  )
  expect_length(recdevs, 2)
  expect_length(recdevs[[1]], iter_vec[1])
  # expectations to make sure scope = 1 works

  expect_equivalent(recdevs[[1]], recdevs[[2]])
  # setting seeds works
  second_run <- build_rec_devs(
    rec_dev_pattern = "rand",
    yrs = c(3, 3),
    iter_vec = iter_vec,
    scope = 1,
    rec_dev_pars = c(3, 1),
    stddev = rep(0.2, 2), # needs to be same length as number of scenarios
    rec_autoCorr = NA,
    seed = seed_vals
  )
  expect_equal(recdevs, second_run)

  # test scope = 2
  # iterations the same across scenarios, but different across iterations.
  recdevs_scope_2 <- build_rec_devs(
    rec_dev_pattern = "rand",
    yrs = c(3, 2),
    iter_vec = c(2, 5),
    scope = 2,
    rec_dev_pars = c(3, 1),
    stddev = rep(0.2, 2),
    rec_autoCorr = NA,
    seed = seed_vals
  )
  # scenario 1, iteration 1 should be equal to scenario 2, iteration 2 first val
  expect_equal(recdevs_scope_2[[1]][[1]][1], recdevs_scope_2[[2]][[1]][1])
  # scenario 1, iterations 2 should be equal to scenario 2, iteration 2 first val
  expect_equal(recdevs_scope_2[[1]][[2]][1], recdevs_scope_2[[2]][[2]][1])
  # scenario 1, iteration 1 first val should not equal scenario 1, iteration 2 first val
  expect_true(recdevs_scope_2[[1]][[1]][1] != recdevs_scope_2[[1]][[2]][1])
  expect_true(all(!is.na(unlist(recdevs_scope_2))))
  expect_true(all(!is.na(unlist(recdevs_scope_2))))

  recdevs_scope_3 <- build_rec_devs(
    rec_dev_pattern = "rand",
    yrs = c(3, 3),
    iter_vec = iter_vec,
    scope = 3,
    rec_dev_pars = c(3, 1),
    stddev = rep(0.2, 2),
    rec_autoCorr = NA,
    seed = seed_vals
  )
  expect_length(recdevs_scope_3, 2)
  expect_true(all(!is.na(unlist(recdevs_scope_3))))
  # recdevs across scenarios are different:
  expect_true(recdevs_scope_3[[1]][[1]][1] != recdevs_scope_3[[2]][[1]][1])
  # recdev across iterations are different:
  expect_true(recdevs_scope_3[[1]][[1]][1] != recdevs_scope_3[[1]][[2]][1])
})

test_that("Build recdevs works as expected for option autocorrelated random", {
  iter_vec <- c(3, 2)
  seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
  set.seed(12)
  ts <- rnorm(40)
  # generate the rec_autoCorr input (which is complex)
  rec_autoCorr <- vector("list", 2)
  rec_autoCorr[[1]] <- stats::arima(x = ts[1:20], order = c(0, 0, 4))
  rec_autoCorr[[2]] <- stats::arima(x = ts[21:40], order = c(0, 0, 4))

  recdevs <- build_rec_devs(
    rec_dev_pattern = "AutoCorr_rand",
    yrs = c(3, 6),
    iter_vec = iter_vec,
    scope = 3,
    rec_dev_pars = c(3, 1),
    stddev = c(0.2, 0.2),
    rec_autoCorr = rec_autoCorr,
    seed = seed_vals
  )
  expect_length(recdevs, 2)
  expect_length(recdevs[[1]], iter_vec[1])
})

test_that("Build recdevs works as expected for option autocorrelated specified", {
  iter_vec <- c(3, 2)
  seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
  set.seed(12)
  ts <- rnorm(40)
  # generate the rec_autoCorr input (which is complex)
  rec_autoCorr <- vector("list", 2)
  rec_autoCorr[[1]] <- list(mean = 0.1, sd = 0.1)
  rec_autoCorr[[2]] <- list(mean = 0.2, sd = 0.1)

  recdevs <- build_rec_devs(
    rec_dev_pattern = "AutoCorr_Spec",
    yrs = c(3, 6),
    iter_vec = iter_vec,
    scope = 3,
    rec_dev_pars = c(3, 1),
    stddev = c(0.2, 0.2),
    rec_autoCorr = rec_autoCorr,
    seed = seed_vals
  )
  expect_length(recdevs, 2)
  expect_length(recdevs[[1]], iter_vec[1])
})

# TODO: add tests for vector option (the custom recdevs list???)
# test_that("Build recdevs works as expected for custom values", {
#   set.seed <- 123
#   iter_vec <- c(3, 2)
#   yrs_vec <- c(3, 6)
#   custom_recdevs_vec <- rnorm(iter_vec[1] * yrs_vec[1] + iter_vec[2] * yrs_vec[2])
#   custom_recdevs_matrix <- as.matrix(custom_recdevs_vec, nrow = sum(iter_vec))
#
#   seed_vals <- set_MSE_seeds(seed = 123, iter_vec = iter_vec)
#   recdevs <- build_rec_devs(rec_dev_pattern = "vector",
#                             yrs = yrs_vec,
#                             iter_vec = iter_vec,
#                             scope = 3,
#                             rec_dev_pars = custom_recdevs_matrix,
#                             stddev = NA,
#                             rec_autoCorr = NA,
#                             seed = seed_vals)
# })
