# functions to process recdevs
#' @param yrs a vector of years
#' @param rec_devs a vector of recdevs  
get_rec_devs_matrix <- function(yrs, rec_devs) {
  # TODO: add check that yrs and rec_devs are same length?
  temp_recdev<-matrix(NA, length(yrs), ncol = 2)
  temp_recdev[,1]<- yrs
  temp_recdev[,2] <- rec_devs[seq_along(yrs)]
  colnames(temp_recdev) <- c("year","recdev")
  temp_recdev
  }