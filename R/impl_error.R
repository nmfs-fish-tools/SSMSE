# functions to process implmentation error

#' Put implementation error of 0 into a matrix
#' 
#'@param yrs a vector of years
#'@return A length(yrs) row, 2 column matrix containing the years in the first
#' column and the implementation error values in the second.
get_impl_error_matrix <- function(yrs) {
  temp_impl_error <- matrix(NA, nrow = length(yrs), ncol = 2)
  temp_impl_error[, 1] <- yrs
  temp_impl_error[, 2] <- rep(0, times = length(yrs))
  colnames(temp_impl_error) <-c ("year","impl_error")
  temp_impl_error
}