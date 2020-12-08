# functions to process recdevs

#' Put recdevs into a matrix
#'
#' @param yrs a vector of years
#' @param rec_devs a vector of recdevs
#' @return A length(yrs) row, 2 column matrix containing the years in the first
#'  column and the rec_devs values in the second.
get_rec_devs_matrix <- function(yrs, rec_devs) {
  # TODO: add check that yrs and rec_devs are same length?
  if (length(rec_devs) < length(yrs)) {
    stop("length  of rec_devs is less than length of yrs.")
  }
  temp_recdev <- matrix(NA, length(yrs), ncol = 2)
  temp_recdev[, 1] <- yrs
  # only use the first rec_devs, for the same length as yrs.
  temp_recdev[, 2] <- rec_devs[seq_along(yrs)]
  colnames(temp_recdev) <- c("year", "recdev")
  
  return(temp_recdev)
}


#' calculate the input variance from target mean and variance
#'
#' @param target_mean The target mean of the observed log normal values
#' @param targ_var the target variance to the simulated log normal values
#' @return the squared difference between observed and target variances
get_inp_var_recs <- function(target_mean, targ_var) {
  inp_var <- sqrt(log(targ_var / target_mean^2 + 1))
}

#' calculate input mean to achieve target mean
#'
#' @param target_mean The target mean of the observed log normal values
#' @param inp_var the input variance to the random log normal generator
#' @return the input mean required to achieve target mean.
get_inp_mean_recs <- function(target_mean, inp_var) {
  inp_mean <- log(target_mean) - 0.5 * inp_var
}

#' calculate a vector of recruitment deviations
#'
#' @param breaks a vector of break-points between year groups rescaled to a mean of zero
#' @param yrs the number of years to simulate recruitment deviations for
#' @param target_mean the target mean of the final achieved recruitment deviations (default=0)
#' @param stddev the standard deviation of simulated log recruitment deviations
#' @return a vector of recruitment deviations.
calc_rec_devs <- function(breaks, yrs, target_mean=1, stddev=0) {
  rec_dev_seq <- stats::rnorm(yrs)
  input_mean <- get_inp_mean_recs(target_mean=target_mean,inp_var=(stddev^2))
  for (k in 1:(length(breaks) - 1)) {
    temp_rec_dev <- rec_dev_seq[(breaks[k] + 1):(breaks[k + 1])]
    # zscore to mean center and put in stddev units
    temp_rec_dev <- (temp_rec_dev - mean(temp_rec_dev))/stats::sd(temp_rec_dev)
    # apply the standard deviation
    temp_rec_dev <- temp_rec_dev * stddev
    # apply the corrected mean
    temp_rec_dev <- temp_rec_dev + input_mean
    # set rec devs seq
    rec_dev_seq[(breaks[k] + 1):(breaks[k + 1])] <- temp_rec_dev
    
  }
  return(rec_dev_seq)
}

#' check that user input recruitment deviation vectors are the correct length
#'
#' @param length_recdev_seq The length of the input recruitment deviation sequence
#' @param expected_length the expected length of the recruitment deviation sequence
check_recdev_error <- function(length_recdev_seq, expected_length) {
  if (length_recdev_seq != expected_length) {
    stop("Wrong number of recruitement deviation pars. You input ",
         length_recdev_seq, " when it should have been ",
         expected_length, " equal to nyrs")
  }
}

#' calculate a vector of auto-correlated recruitment deviations
#'
#' @param breaks a vector of break-points between year groups rescaled to a mean of zero
#' @param yrs the number of years to simulate recruitment deviations for
#' @param rec_autocorr_mean a vector of the mean estimated autocorrelation parameters with length equal to the order of the timelag
#' @param rec_autocorr_sd a vector of the standard deviation of estimated autocorrelation parameters with length equal to the order of the timelag
#' @param target_mean the target mean of the final achieved recruitment deviations (default=0)
#' @param stddev the standard deviation of simulated recruitment deviations
#' @return a vector of recruitment deviations.
calc_autoCor_rec_devs <- function(breaks, yrs, rec_autocorr_mean, rec_autocorr_sd, target_mean=1, stddev=0) {
  model_parms<-stats::rnorm(n=length(rec_autocorr_mean),mean=rec_autocorr_mean,sd=rec_autocorr_sd)
  rec_dev_seq <- as.vector(stats::arima.sim(model=list(ma=model_parms), n=yrs))
  input_mean <- get_inp_mean_recs(target_mean=target_mean,inp_var=(stddev^2))
  for (k in 1:(length(breaks) - 1)) {
    #Select subset of recdevs to be adjusted
    temp_rec_dev <- rec_dev_seq[(breaks[k] + 1):(breaks[k + 1])]
    #Subract mean to center the recdevs to zero
    temp_rec_dev <- temp_rec_dev - (sum(temp_rec_dev) / length(temp_rec_dev))
    #Divide by the standard deviation to unit scale the revdevs
    temp_rec_dev <- temp_rec_dev / stats::sd(temp_rec_dev)
    #Multiple by the target standard deviation
    temp_rec_dev <- temp_rec_dev * stddev
    #Add the adjusted mean deviation
    temp_rec_dev <- temp_rec_dev + input_mean 
    #return adjusted devs to the recdev sequence
    rec_dev_seq[(breaks[k] + 1):(breaks[k + 1])] <- temp_rec_dev
  }
  return(rec_dev_seq)
}

#' Build an array of recruitment deviation vectors for every scenario and iteration
#'
#' @param rec_dev_pattern how to simulate recruitment devations.
#' @param iter_vec An integer vector (with length n_scenarios) of number of 
#'  iterations by scenario. length(iter_vec) should be the number of scenarios simulated.
#' @param yrs the number of years to simulate recruitment deviations for for each 
#'  scenario. An integer vector with length equal to the number of scenarios.
#' @param scope scope over which recruitment devations will be randomized.
#' @param rec_dev_pars recruitment devation simulation parameters dependent on chosen pattern.
#' @param stddev the standard deviation of simulated recruitment deviations
#' @param rec_autoCorr a list of auto-correlation paramters and standard deviations
#' @param seed a list prespecified random seed values to enable replication of simulated rec devs
#' @return A list of scenarios with lists of interations in each with a vector of
#'  rec devs for each simulation year.
build_rec_devs <- function(
  rec_dev_pattern = c("none", "rand", "AutoCorr_rand", "AutoCorr_Spec", 
                      "vector"),
  yrs,
  iter_vec,
  scope = c("2", "1", "3"),
  rec_dev_pars = NULL,
  stddev = NULL,
  rec_autoCorr = NULL, 
  seed = NULL) {
  
  #input checks - to add
  scope <- match.arg(as.character(scope), choices = c("2", "1", "3"))
  rec_dev_pattern <- match.arg(rec_dev_pattern, 
                               choices = c("none", "rand", "AutoCorr_rand", 
                                           "AutoCorr_Spec", "vector"))
  n_scenarios <- length(iter_vec) # should be 1 value for each scenario
  
  # initialize the vector.
  rec_dev_list <- vector(mode = "list", length = n_scenarios)
  # pattern specific filling of recdevs.
  if (rec_dev_pattern == "none") {
    for (i in 1:n_scenarios) {
      rec_dev_list[[i]] <- vector(mode = "list", length = length(iter_vec))
      for (j in 1:iter_vec[i]) {
        rec_dev_list[[i]][[j]] <- rep(0, yrs[i])
      }
    }
  } else if (rec_dev_pattern == "rand") {
    if (scope == 2) { 
      if(length(unique(stddev)) != 1) {
        warning("stddev input to function build_rec_devs is different among ",
               "scenarios, but scope = 2. Using stddev value from the first ",
               "scenario.")
      }
      # find the scenario that has the longest years
      set.seed(seed[["global"]]) # b/c using the same across scenarios.
      yrs_max <- max(yrs)
      iter_max <- max(iter_vec)
      breaks <- unique(c(seq(0, yrs_max, rec_dev_pars[1]), yrs_max))
      rec_dev_iter_list <- vector("list", length = iter_max)
      for(i in seq_len(iter_max)) {
        rec_dev_iter_list[[i]] <-
          calc_rec_devs(breaks = breaks, yrs = yrs_max, 
                        stddev = stddev[1]*rec_dev_pars[2])
      }
    }
    for (i in 1:n_scenarios) {
      breaks <- unique(c(seq(0, yrs[i], rec_dev_pars[1]), yrs[i]))
      if (scope == 1) { 
        
        set.seed(seed[["global"]])
        
        rec_dev_seq <- calc_rec_devs(breaks = breaks, yrs = yrs[i], 
                                     stddev = stddev[i]*rec_dev_pars[2])
      }
      rec_dev_list[[i]] <- vector(mode = "list", length = length(iter_vec))
      for (j in 1:iter_vec[i]) {
        if (scope == 2) {
          # pull the rec dev vector for the iteration,and shortening to the
          # appropriate length of years for the scenario.
          # should be the same across scenarios.
          rec_dev_seq <- rec_dev_iter_list[[j]][seq_len(yrs[i])]
        }
        if (scope == 3) { 
          
          set.seed(seed[["iter"]][[i]][j])
          
          rec_dev_seq <- calc_rec_devs(breaks = breaks, yrs = yrs[i], 
                                       stddev = stddev[i]*rec_dev_pars[2])
        }
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  } else if (rec_dev_pattern == "AutoCorr_rand") {
    for (i in 1:n_scenarios) {
      rec_autocorr_mean<-rec_autoCorr[[i]][["coef"]][1:4]
      rec_autocorr_sd<-sqrt(c(rec_autoCorr[[i]][["var.coef"]][1,1],rec_autoCorr[[i]][["var.coef"]][2,2], rec_autoCorr[[i]][["var.coef"]][3,3], rec_autoCorr[[i]][["var.coef"]][4,4]))
      rec_autocorr_mean<-ifelse(abs(rec_autocorr_mean)<(1.96*abs(rec_autocorr_sd)), 0, rec_autocorr_mean)  
      breaks <- unique(c(seq(0, yrs[i], rec_dev_pars[1]), yrs[i]))
      if (scope == 1) { 
        
        set.seed(seed[["global"]])
        
        rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks, yrs = yrs[i], 
                                             rec_autocorr_mean = rec_autocorr_mean, 
                                             rec_autocorr_sd = c(0,0,0,0), 
                                             stddev = stddev[i]*rec_dev_pars[2])
      }
      rec_dev_list[[i]] <- vector(mode = "list", length = length(iter_vec))
      if (scope == 2) { 
        
        set.seed(seed[["scenario"]][i])
        
        rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks,  yrs = yrs[i],
                                             rec_autocorr_mean = rec_autocorr_mean, 
                                             rec_autocorr_sd = rec_autocorr_sd, 
                                             stddev = stddev[i]*rec_dev_pars[2])
      }
      for (j in 1:iter_vec[i]) {
        if (scope == 3) { 
          
          set.seed(seed[["iter"]][[i]][j])
          
          rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks,  yrs = yrs[i],
                                               rec_autocorr_mean = rec_autocorr_mean, 
                                               rec_autocorr_sd = rec_autocorr_sd, 
                                               stddev = stddev[i]*rec_dev_pars[2])
        }
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  } else if (rec_dev_pattern == "AutoCorr_Spec") {
    for (i in 1:n_scenarios) {
      breaks <- unique(c(seq(0, yrs[i], rec_dev_pars[1]), yrs[i]))
      if (scope == 1) { 
        
        set.seed(seed[["global"]])
        
        rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks, yrs = yrs[i], 
                                             rec_autocorr_mean = rec_autoCorr[[i]][["mean"]], 
                                             rec_autocorr_sd = rec_autoCorr[[i]][["sd"]], 
                                             stddev = stddev[i])
      }
      rec_dev_list[[i]] <- vector(mode = "list", length = length(iter_vec))
      if (scope == 2) { 
        
        set.seed(seed[["scenario"]][i])
        
        rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks, yrs = yrs[i], 
                                             rec_autocorr_mean = rec_autoCorr[[i]][["mean"]], 
                                             rec_autocorr_sd = rec_autoCorr[[i]][["sd"]], 
                                             stddev = stddev[i])
      }
      for (j in 1:iter_vec[i]) {
        if (scope == 3) { 
          
          set.seed(seed[["iter"]][[i]][j])
          
          rec_dev_seq <- calc_autoCor_rec_devs(breaks = breaks, yrs = yrs[i], 
                                               rec_autocorr_mean = rec_autoCorr[[i]][["mean"]], 
                                               rec_autocorr_sd = rec_autoCorr[[i]][["sd"]], 
                                               stddev = stddev[i])
        }
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  } else if (rec_dev_pattern == "vector") {
    row = 1
    
    for (i in 1:n_scenarios) {
      if (scope == 1) {
        rec_dev_seq <- rec_dev_pars
        check_recdev_error(length(rec_dev_seq), (yrs[i]))
      }
      rec_dev_list[[i]] <- vector(mode = "list", length = length(iter_vec))
      if (scope == 2) {
        rec_dev_seq <- rec_dev_pars[i, ]
        check_recdev_error(length(rec_dev_seq), (yrs[i]))
      }
      for (j in 1:iter_vec[i]) {
        if (scope == 3) {
          rec_dev_seq <- rec_dev_pars[row, ]
          check_recdev_error(length(rec_dev_seq), (yrs[i]))
          row <- row + 1
        }
        rec_dev_list[[i]][[j]] <- rec_dev_seq
      }
    }
  }
  return(rec_dev_list)
}
