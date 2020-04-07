# functions to process recdevs

#' Put recdevs into a matrix
#' 
#' @param yrs a vector of years
#' @param rec_devs a vector of recdevs  
#' @return A length(yrs) row, 2 column matrix containing the years in the first
#' column and the rec_devs values in the second.
get_rec_devs_matrix <- function(yrs, rec_devs) {
  # TODO: add check that yrs and rec_devs are same length?
  temp_recdev<-matrix(NA, length(yrs), ncol = 2)
  temp_recdev[,1]<- yrs
  # only use the first rec_devs, for the same length as yrs.
  temp_recdev[,2] <- rec_devs[seq_along(yrs)]
  colnames(temp_recdev) <- c("year","recdev")
  temp_recdev
  }