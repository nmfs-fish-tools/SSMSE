# functions to process implmentation error

#' Put implementation error of 0 into a matrix
#'
#' @param yrs a vector of years
#' @return A length(yrs) row, 2 column matrix containing the years in the first
#' column and the implementation error values in the second.
get_impl_error_matrix <- function(yrs) {
  temp_impl_error <- matrix(NA, nrow = length(yrs), ncol = 2)
  temp_impl_error[, 1] <- yrs
  temp_impl_error[, 2] <- rep(0, times = length(yrs))
  colnames(temp_impl_error) <- c("year", "impl_error")
  temp_impl_error
}

#' calculate a sequence of implemetation errors.
#'
#' @param breaks a vector of break-points between year groups rescaled to a mean of zero
#' @param n_impl_errors the number of implementation error to simulate equal to nyrs x nseas x nfleets
#' @param n_impl_error_groups the number of fleets times number of seasons
#' @param target_mean The target mean of the observed log normal values
#' @param inp_mean the input mean to achieve desired mean and standard deviation of log normal values
#' @param inp_stdev the input standard deviation to achieve desired mean and standard deviation of log normal values
#' @return a vector of implementation errors.
calc_impl_errors <- function(breaks, n_impl_errors, n_impl_error_groups, target_mean, inp_mean, inp_stdev) {
  impl_error_seq <- stats::rlnorm(n_impl_errors, inp_mean, inp_stdev)
  for (k in 1:(length(breaks) - 1)) {
    for (ng in 1:n_impl_error_groups) {
      fleet_errors <- seq((breaks[k] + ng), (breaks[k + 1]), n_impl_error_groups)
      temp_impl_error <- impl_error_seq[fleet_errors]
      temp_impl_error <- target_mean[ng] * temp_impl_error /
        (sum(temp_impl_error) / length(temp_impl_error))
      impl_error_seq[fleet_errors] <- temp_impl_error
    }
  }
  return(impl_error_seq)
}


#' calculate the input variance from target mean and variance
#'
#' @param target_mean The target mean of the observed log normal values
#' @param targ_var the target variance to the simulated log normal values
#' @return the squared difference between observed and target variances
get_inp_var <- function(target_mean, targ_var) {
  inp_var <- sqrt(log(targ_var / target_mean^2 + 1))
}

#' calculate input mean to achieve target mean
#'
#' @param target_mean The target mean of the observed log normal values
#' @param inp_var the input variance to the random log normal generator
#' @return the input mean required to achieve target mean.
get_inp_mean <- function(target_mean, inp_var) {
  inp_mean <- log(target_mean) - 0.5 * inp_var
}

#' check that user input implementation error vectors are the correct length
#'
#' @param length_impl_error_seq The length of the input implementation error sequence
#' @param expected_length the expected length of the implementation error sequence
check_impl_error <- function(length_impl_error_seq, expected_length) {
  if (length_impl_error_seq != expected_length) {
    stop(
      "Wrong number of implementation pars. You input ",
      length_impl_error_seq, " when it should have been ",
      expected_length, " equal to nyrs*nseas*Nfleet"
    )
  }
}


#' Build a list of implementation error vectors for each scenario and iteration.
#'
#' @param yrs the number of years to simulate implementation error for.
#' @param nyrs_assess the number of years between assessments.
#' @param n_impl_error_groups the number of fleets times number of seasons
#' @param scope scope over which implementation errors will be randomized.
#' @param impl_error_pattern how to simulate implementation errors.
#' @param impl_error_pars implementation error simulation parameters dependent on chosen pattern.
#' @param n_scenarios The number of scenarios simulated
#' @param iter_vec a vector of iteration names.
#' @param seed a list prespecified random seed values to enable replication of simulated implementation error
#' @return A list of scenarios with lists of interations in each with a vector of
#'  implementation errors for each simulation year-fleet-season combination.
build_impl_error <- function(yrs, nyrs_assess, n_impl_error_groups, scope, impl_error_pattern, impl_error_pars, n_scenarios, iter_vec, seed) {
  if (is.null(impl_error_pars)) {
    impl_error_pars <- c(nyrs_assess, 1, 0)
  }
  impl_error <- list()
  if (impl_error_pattern == "none") {
    for (i in 1:n_scenarios) {
      impl_error[[i]] <- list()
      for (j in 1:iter_vec[i]) {
        impl_error[[i]][[j]] <- rep(1, yrs[i] * n_impl_error_groups[i])
      }
    }
  } else if (impl_error_pattern == "rand") {
    for (i in 1:n_scenarios) {
      inp_var <- get_inp_var(impl_error_pars[2:(n_impl_error_groups[i] + 1)], impl_error_pars[(n_impl_error_groups[i] + 2):(n_impl_error_groups[i] * 2 + 1)]^2)
      inp_mean <- get_inp_mean(impl_error_pars[2:(n_impl_error_groups[i] + 1)], inp_var)
      inp_stdev <- sqrt(inp_var)

      breaks <- unique(c(
        seq(
          0, (yrs[i] * n_impl_error_groups[i]),
          (impl_error_pars[1] * n_impl_error_groups[i])
        ),
        (yrs[i] * n_impl_error_groups[i])
      ))
      if (scope == 1) {
        set.seed((seed[["global"]] + 999))

        impl_error_seq <- calc_impl_errors(breaks, (yrs[i] * n_impl_error_groups[i]), impl_error_pars[2:(n_impl_error_groups[i] + 1)], inp_mean, inp_stdev)
      }
      impl_error[[i]] <- list()
      if (scope == 2) {
        set.seed((seed[["scenario"]][i] + 999))

        impl_error_seq <- calc_impl_errors(breaks, (yrs[i] * n_impl_error_groups[i]), impl_error_pars[2:(n_impl_error_groups[i] + 1)], inp_mean, inp_stdev)
      }
      for (j in 1:iter_vec[i]) {
        if (scope == 3) {
          set.seed((seed[["iter"]][[i]][j] + 999))

          impl_error_seq <- calc_impl_errors(breaks, (yrs[i] * n_impl_error_groups[i]), impl_error_pars[2:(n_impl_error_groups[i] + 1)], inp_mean, inp_stdev)
        }
        impl_error[[i]][[j]] <- impl_error_seq
      }
    }
  } else if (impl_error_pattern == "vector") {
    row <- 1
    for (i in 1:n_scenarios) {
      if (scope == 1) {
        impl_error_seq <- impl_error_pars
        check_impl_error(length(impl_error_seq), (yrs[i] * n_impl_error_groups[i]))
      }
      impl_error[[i]] <- list()
      if (scope == 2) {
        impl_error_seq <- impl_error_pars[i, ]
        check_impl_error(length(impl_error_seq), (yrs[i] * n_impl_error_groups[i]))
      }
      for (j in 1:iter_vec[i]) {
        if (scope == 3) {
          impl_error_seq <- impl_error_pars[row, ]
          check_impl_error(length(impl_error_seq), (yrs[i] * n_impl_error_groups[i]))
          row <- row + 1
        }
        impl_error[[i]][[j]] <- impl_error_seq
      }
    }
  }
  return(impl_error)
}
