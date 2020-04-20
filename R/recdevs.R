# functions to process recdevs

#' Put recdevs into a matrix
#' 
#' @param yrs a vector of years
#' @param rec_devs a vector of recdevs
#' @param sum_to_zero Should the last rec_dev returned be a negative number to 
#'  ensure that ther recruitment deviations sum to zero? Defaults to FALSE.  
#' @return A length(yrs) row, 2 column matrix containing the years in the first
#' column and the rec_devs values in the second.
get_rec_devs_matrix <- function(yrs, rec_devs, sum_to_zero = FALSE) {
  # TODO: add check that yrs and rec_devs are same length?
  if(sum_to_zero == FALSE & length(rec_devs) < length(yrs)) {
    stop("length  of rec_devs is less than length of yrs.")
  } else if (sum_to_zero & length(rec_devs) < (length(yrs)-1)) {
    stop("rec_devs is not long enough. With sum_to_zero = TRUE, it should be at",
         " least length(yrs) - 1 ")
  }
  temp_recdev<-matrix(NA, length(yrs), ncol = 2)
  temp_recdev[,1]<- yrs
  # only use the first rec_devs, for the same length as yrs.
  temp_recdev[,2] <- rec_devs[seq_along(yrs)]
  colnames(temp_recdev) <- c("year","recdev")
  if(sum_to_zero) {
    temp_recdev[nrow(temp_recdev),2] <- -sum(temp_recdev[-nrow(temp_recdev),2])
  }
  temp_recdev
  }