# functions to process recdevs

#' Put recdevs into a matrix
#' 
#' @param yrs a vector of years
#' @param rec_devs a vector of recdevs
#' @param sum_to_zero Should the last rec_dev returned be a negative number to 
#'  ensure that ther recruitment deviations sum to zero? Defaults to FALSE.  
#' @return A length(yrs) row, 2 column matrix containing the years in the first
#'  column and the rec_devs values in the second.
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


#' calculate a vector of recruitment deviations
#' 
#' @param breaks a vector of break-points between year groups rescaled to a mean of zero
#' @param yrs the number of years to simulate recruitment deviations for
#' @param stddev the standard deviation of simulated recruitment deviations 
#' @return a vector of recruitment deviations.
calc_rec_devs <- function(breaks, yrs, stddev) {
  rec_dev_seq <- stats::rnorm(yrs)
  for(k in 1:(length(breaks)-1)) {
    temp_rec_dev <- rec_dev_seq[(breaks[k]+1):(breaks[k+1])]
    temp_rec_dev <- temp_rec_dev - (sum(temp_rec_dev)/length(temp_rec_dev))
    temp_rec_dev <- temp_rec_dev/stats::sd(temp_rec_dev)
    rec_dev_seq[(breaks[k]+1):(breaks[k+1])] <- temp_rec_dev*stddev
  }
  return(rec_dev_seq)
}

#' check that user input recruitment deviation vectors are the correct length
#' 
#' @param length_recdev_seq The length of the input recruitment deviation sequence
#' @param expected_length the expected length of the recruitment deviation sequence
check_recdev_error <- function(length_recdev_seq,expected_length){
  if(length_recdev_seq != expected_length) {
    stop("Wrong number of recruitement deviation pars. You input ",
         length_recdev_seq," when it should have been ",
         expected_length," equal to nyrs")
  }
}



#' Build an array of recruitment deviation vectors for every scenario and iteration
#' 
#' @param yrs the number of years to simulate recruitment deviations for.
#' @param scope scope over which recruitment devations will be randomized.
#' @param rec_dev_pattern how to simulate recruitment devations. 
#' @param rec_dev_pars recruitment devation simulation parameters dependent on chosen pattern.
#' @param stddev the standard deviation of simulated recruitment deviations 
#' @param scen_name_vec a vector of scenario names 
#' @param iter_list a list of iteration names.
#' @return A list of scenarios with lists of interations in each with a vector of 
#'  rec devs for each simulation year.
build_rec_devs <- function(yrs, nyrs_assess, scope, rec_dev_pattern, rec_dev_pars, stddev, scen_name_length, iter_list) {

  if(is.null(rec_dev_pars)) {
    rec_dev_pars <- c(nyrs_assess,1) 
  }
  
  if(rec_dev_pattern == "none") {
    rec_dev_list <- list()
    for(i in 1:scen_name_length) {
      rec_dev_list[[i]] <- list()
      for(j in 1:length(iter_list[[i]])) {
        rec_dev_list[[i]][[j]]<-rep(0,yrs)
      }
    }
  }else if(rec_dev_pattern == "rand") {
    breaks <- unique(c(seq(0, yrs, rec_dev_pars[1]), yrs))
    rec_dev_list <- list()
    if(scope == 1) { rec_dev_seq<-calc_rec_devs(breaks, yrs, stddev)}
    for(i in 1:scen_name_length) {
      rec_dev_list[[i]] <- list()
      if(scope == 2) { rec_dev_seq<-calc_rec_devs(breaks, yrs, stddev)}
      for(j in 1:length(iter_list[[i]])) {
        if(scope==3) { rec_dev_seq<-calc_rec_devs(breaks, yrs, stddev)}
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  }else if(rec_dev_pattern == "vector") {
    rec_dev_list <- list()
    row = 1
    if(scope == 1) { 
      rec_dev_seq <- rec_dev_pars
      check_recdev_error(length(rec_dev_seq),(yrs))
    }
    for(i in 1:scen_name_length) {
      rec_dev_list[[i]] <- list()
      if(scope==2) { 
        rec_dev_seq <- rec_dev_pars[i,]
        check_recdev_error(length(rec_dev_seq),(yrs))
      }
      for(j in 1:length(iter_list[[i]])) {
        if(scope == 3) {
          rec_dev_seq <- rec_dev_pars[row, ]
          check_recdev_error(length(rec_dev_seq),(yrs))
          row <- row + 1
        }
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  }
  return(rec_dev_list)
}