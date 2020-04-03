# functions to process implmentation error
#'@param yrs a vector of years
#'@param impl_error a vector of implementation error
get_impl_error_matrix <- function(yrs, impl_error) {
  temp_impl_error <- matrix(NA, nrow = length(yrs), ncol = 2)
  temp_impl_error[, 1] <- yrs
  temp_impl_error[, 2] <- rep(0, times = length(yrs))
  colnames(temp_impl_error) <-c ("year","impl_error")
  temp_impl_error
}