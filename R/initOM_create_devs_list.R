#' Create the devs dataframe for a scenario and iteration from user input
#'
#'This function parses user inputs to convert it into a dataframe of deviations.
#'
#' @param future_om_list User input to runSSMSE
#' @param scenario The scenario name
#' @param niter The iteration number
#' @param om_mod_path Path to the OM files. Used to reference parameter names.
#' @param nyrs The total number of years that the model will be extended forward.
#' @param global_seed A global seed to set, then pull new seeds from global seed + 1.
#' Defaults to 123.
#' @author Kathryn Doering
#' @return A list including 3 dataframes: devs_df, the additive deviations 
#'  relative to the base values; base_df, the base values of the parameter with 
#'  deviations; abs_df, the absolute future values by year (first col) and 
#'  parameter (parameterss in different cols).
convert_future_om_list_to_devs_df <- function(future_om_list, scen_name,
                                              niter, om_mod_path, nyrs, 
                                              global_seed = 123
                                              ) {
  if(isTRUE(is.null(future_om_list))) { # in case future_om_list is NULL to start with.
    dev_vals_df <- NULL
    return(dev_vals_df)
  }
  # create the seeds to use for each list element of future_om_list, one seed per element.
  # add some arbitrary numbers to the global seed; but maybe there is a more
  # thoughtful way to do this? 
  # note: may need to complicated this more, if a different seed is needed for 
  # the given iteration?
  seeds_to_use <- rep(global_seed, length.out = length(future_om_list)) + 
                   seq(from = 235, by = 5, length.out = length(future_om_list))
  
  #add seeds (alternative to this would be adding the seeds to this list earlier)
  future_om_list <- mapply(function(fut_list, seed) {
    fut_list$seed <- seed
    fut_list
  }, fut_list = future_om_list, seed = seeds_to_use, SIMPLIFY = FALSE)
  
  # get rid of the list elements that don't apply to this scenario
  future_om_list <- lapply(future_om_list, function(fut_list, scen) {
     scenarios <- fut_list[["scen"]][-1]
     if(scen %in% scenarios) {
       return(fut_list)
     } else {
       return(NULL)
     }
  }, scen = scen_name)
  if(isTRUE(is.null(future_om_list))) { # in case no changes apply to future_om_list
    dev_vals_df <- NULL
    return(dev_vals_df)
  }
  # construct a dataframe of values (not devs yet). Initialize it using the base
  # params in the model. 
  
  pars_to_change <- unique(unlist(lapply(future_om_list, function(x) x[["pars"]])))

  start <- r4ss::SS_readstarter(file = file.path(om_mod_path, "starter.ss"),
                                verbose = FALSE)
  dat <- r4ss::SS_readdat(file.path(om_mod_path, start[["datfile"]]), verbose = F)
  par <- r4ss::SS_readpar_3.30(parfile = file.path(om_mod_path, "ss.par"),
                               datsource = file.path(om_mod_path, start$datfile),
                               ctlsource = file.path(om_mod_path, start$ctlfile),
                               verbose = FALSE)
  where_par <- match_parname(list_pars = pars_to_change, par = par)
  vals_df <- data.frame(yrs = seq_len(nyrs) + dat[["endyr"]])
  tmp_vals <- setNames(data.frame(
    matrix(NA, nrow = nrow(vals_df),ncol = length(pars_to_change))),
    where_par[["pars"]])
  vals_df <- cbind(vals_df, tmp_vals)
  # Get the base values for these parameters.
  for(p in where_par$pars) {
    vals_df[[p]] <- where_par[where_par$pars == p, "est"]
  }
  base_vals <- vals_df
  #now, add changes on top of this. Note this is the TOTAL value.
  for (i in seq_len(length(future_om_list))) {
    vals_df <- add_dev_changes(fut_list = future_om_list[[i]], scen = scen_name, iter = niter, 
                               par = par, dat = dat, vals_df = vals_df, nyrs = nyrs)
  }
  # remove base values from pars? I think soo...
  vals_df
  dev_vals_df <- vals_df - base_vals
  if(!all(dev_vals_df$yrs == 0)) {
    stop("Incorrect assumption made regarding dataframe addition, please open ",
    "an issue in the SSMSE repository.")
  } 
  # correct the years
  dev_vals_df$yrs <- vals_df$yrs
  
  # return the base vals, deviations, and absolute vals.
  return_list <- list(base_vals = base_vals, dev_vals = dev_vals_df, abs_vals = vals_df)
  return_list
} 

#' Match parameter name to parameter names in the par file
#' 
#' @param list_pars the parameter names to find
#' @param the parfile in which to find list_pars
#' @return A dataframe containing the parameter name and which object it is in
#'  in the par object.
# Find the par name in a par file. Use partial matching?
match_parname <- function(list_pars, par) {
  # make table of values
  par_name_tbl <- data.frame(pars = rownames(par[["MG_parms"]]),
                             obj = "MG_parms")
  par_name_tbl <- rbind(par_name_tbl, 
                        data.frame(pars = rownames(par[["SR_parms"]]),
                                   obj = "SR_parms"))
  par_name_tbl <- rbind(par_name_tbl,
                        data.frame(pars = rownames(par[["Q_parms"]]),
                                   obj = "Q_parms"))
  par_name_tbl <- rbind(par_name_tbl,
                        data.frame(pars = rownames(par[["S_parms"]]),
                                   obj = "S_parms"))
  par_name_tbl <- rbind(par_name_tbl, 
                        data.frame(pars = "rec_devs", obj = "recdev1"))
  if(isTRUE(list_pars == "all")) {
    # note that impl_error is not included in all for now; not sure if it should be?
    return(par_name_tbl)
  }
  par_name_tbl <- rbind(par_name_tbl, data.frame(pars = "impl_error", obj = NA))
  where_pars <- match(list_pars, par_name_tbl[["pars"]])
  subset_parname_tbl <- par_name_tbl[where_pars,]
  est <- vector(mode = "numeric", length = nrow(subset_parname_tbl))
  for (r in seq_len(nrow(subset_parname_tbl))) {
    if (subset_parname_tbl[r,"pars"] == "rec_devs") {
       est[r] <- 0 # Is this the right assumption? or pull some val from recdevs1?
    } else if (subset_parname_tbl[r, "pars"] == "impl_error") {
       est[r] <- 0 # is this the right assumption? or pull some val from Fcast_impl_error?
    } else {
      tmp_tbl <- par[[subset_parname_tbl[r, "obj"]]]
      tmp_est <- tmp_tbl[ rownames(tmp_tbl) == subset_parname_tbl[r,"pars"],"ESTIM"]
      est[r] <- tmp_est
    }
  }
  subset_parname_tbl <- cbind(subset_parname_tbl, data.frame(est = est))
  subset_parname_tbl
}

#' Sample deviations from normal, lognormal, or AR-1 process.
#' 
#' @param mean A single value or vector of mean parameters
#' @param sd A single value or vector of sd parameter
#' @param ar_1_phi The phi (coefficient) value for an ar 1 process. Should be 
#'  Between -1 and 1. 0 means an AR 1 process will NOT be used. 1 Indicates a
#'  random walk.
#' @param seed the seed that will be set before sampling
#' @param ndevs The number of deviations to expect
#' @author Kathryn Doering
sample_devs <- function(mean,
                        sd,
                        ar_1_phi = 0,
                        seed,
                        ndevs, 
                        dist = c("normal", "lognormal")) {
  # checks
  dist <- match.arg(dist, several.ok = FALSE)
    #note: arima.sim(list(order = c(1,0,0), ar = phi), n = nyrs)+mean

  if(length(mean) == 1) {
    mean <- rep(mean, length = ndevs)
  }
  assertive.properties::assert_is_of_length(mean, ndevs)

  if(length(sd) == 1) {
    sd <- rep(sd, length = ndevs)
  }
  assertive.properties::assert_is_of_length(sd, ndevs)
  # sample
  set.seed(seed)
  if(isTRUE(is.na(ar_1_phi) | unique(ar_1_phi) == 0)) {
    devs <- switch(dist,
           normal = rnorm(n = ndevs, mean = mean, sd = sd), 
           # other way of writing: obs * exp(rnorm(n = 1, mean = 0, sd = sd) - sd^2/2)
           # note term in front is the bias correction to make E(x) = exp(mu) 
           # instead of med(x) = exp(mu)
           lognormal = exp(-sd^2/2)*rlnorm(n = ndevs, meanlog = log(mean), sdlog = sd)) 
  } else {
    if(dist == "lognormal") {
      stop("Lognormal sampling cannot be use with autocorrelation. Please use",
      "normal distribution.")
    }
    if(length(ar_1_phi) == 1) {
      ar_1_phi <- rep(ar_1_phi, length = ndevs)
    }
    assertive.properties::is_of_length(ar_1_phi, ndevs)
    #custom function to sample, becasue sd and phi have to be fixed for the 
    # arima_sim function.
    # need to think more about if this is correct or  not. this isn't exactly
    # an ar 1 process as described....because the variance isn't constant.
    devs <- vector(mode = "numeric", length = ndevs)
    nonstat_warning <- TRUE
    for (d in seq_len(ndevs)) {
      if(d == 1) {
        past_dev <- 0 # I think
      } else {
        past_dev <- devs[d-1]
      }
      if(abs(ar_1_phi[d]) < 1) {
        devs[d] <- mean[d]*(1-ar_1_phi[d]) + ar_1_phi[d]*past_dev + 
          rnorm(1, mean = 0, sd = sd[d]/sqrt(1/(1-ar_1_phi[d]^2)))    
      } else {
        devs[d] <- mean[d] + ar_1_phi[d]*past_dev + rnorm(1, mean = 0, sd = sd[d])
        nonstat_warning <- TRUE
      }
    }
    if(nonstat_warning) {
      warning("An AR1 process with phi >= 1 was called, therefore will be nonstationary.")
    }
  }
  devs
}

#' Add the deviation changes from the list obj to an existing df
#' 
#' @param fut_list A single change input
#' @param scen The scenario name
#' @param iter The iteration name
#' @param par the parameter list
#' @param dat The data list
#' @param nyrs The number of years to extend the model forward
#' @param vals_df The dataframe with future om values
add_dev_changes <- function(fut_list, scen, iter, par, dat, vals_df, nyrs) {
  where_par <- match_parname(list_pars = fut_list[["pars"]], par = par)
  
  # model_change sampling -------
  # do sampling for things that require sampling
  if(fut_list[["pattern"]][1] == "model_change") {
    # create a dataframe of deviations; but may don't need to do? May be easier to 
    # create the values we want, then back calc the deviations later?
    # dev_vals_df <- vals_df
    # dev_vals_df[,seq_len(ncol(dev_vals_df))[-1]] <- 0 # Fill with 0s b/c just want the devs.
    # now, parse the input part (and pattern[2])

    if(fut_list$scen[1] == "randomize") {
      # need a different seed for each scenario. Just add the number element
      # the scen is to the seed (check: will this behave pseudorandomly?)
      fut_list$seed <- fut_list$seed + which(fut_list$scen == scen)
    }
    set.seed(fut_list$seed) # set the seed before sampling.
    # if there are no tv devs in the original model, than taking a historical
    # average is unnecessary.
    for(i in where_par$pars) {
      # find the mean: 1) is a mean specified? If not, use the most recent
      # value. 
        # do some calcs to figure out the mean value
        # this holds if the parameter isn't time varying 
        # calculate the mean. The  below holds with or without a trend
 
        mean <- calc_par_trend(val_info = fut_list$input, 
                               val_line = "mean",
                               par = par,
                               ref_parm_value = where_par[where_par$pars == i, "est"],
                               vals_df = vals_df, # use to potentially get trend start value.
                               parname = i
                               )
        sd <- calc_par_trend(val_info = fut_list$input, 
                             val_line = "sd",
                             par = par,
                             ref_parm_value = 0, # may not always be correct?
                             vals_df = vals_df, # use to get trend start value, and last yr. 
                             parname = i
                             )
        ar_1_phi <- calc_par_trend(val_info = fut_list$input, 
                                   val_line = "ar_1_phi",
                                   par = par,
                                   ref_parm_value = 0, # may not always be correct?
                                   vals_df = vals_df, # use to get trend start value, and last yr. 
                                   parname = i
                                  )
          
      # if it is specified: then parse this input to figure out what the 
      # sampled mean should be.
      # find the sd: 2) is the sd specified? If not, use sd of 1 to sample (or historical sd?)
      devs <- sample_devs(mean = mean, sd = sd, ar_1_phi = ar_1_phi,
                               ndevs = nyrs,
                               seed = fut_list$seed, dist = fut_list$pattern[2])
      # this works for 1 parameter only

      vals_df[[i]] <- devs # Maybe these should just replace what is alredy there?

      # Now, should have a complete list of the dev vals to apply
    }
  } else if (fut_list$pattern[1] == "custom") {
    # custom ---
    for (i in where_par$pars) {
      tmp_base <- where_par[where_par$pars == i, "est"]
      # custom will just replace any values for the pattern.
      #filter out the columns needed for the given scenario and iteration
      custom_vals <- fut_list$input[fut_list$input$scen %in% c("all", scen) &
                               fut_list$input$par == i &
                               fut_list$input$iter == iter,]
      custom_vals <- dplyr::select(custom_vals, yr, value) %>%
        dplyr::rename(yrs = yr)
      vals_df <- dplyr::left_join(vals_df, custom_vals, by = "yrs") %>% 
        tidyr::replace_na(replace = list(value = tmp_base))
      vals_df[[i]] <- vals_df[["value"]] # move to the correct column
      vals_df[["value"]] <- NULL
    }
  }
  vals_df
  }
  
  #' Calculate the parameter trend
  #' 
  #' @start_val The start value for the trend
  #' @val_info The line in the input df containing info about the parameter.
  #' @val_line Which line in val info to use. Can be mean or sd, but need to add
  #'  more options for time varying parameters, perhaps.
  #' @ref_parm_value This is the historic parameter that the end trend value.
  #'  Can be NA if the there is no line in val_info for the given parameter
  #' @vals_df Use to get start val and last year
  #' @parname Name of the parameter with devs from the SS model.
  #'  will reference, if using a relative method.
  #' @par the par file list
  calc_par_trend <- function(val_info,val_line = c("mean", "sd", "ar_1_phi"), ref_parm_value, vals_df, parname, par) {
    
    val_line <- match.arg(val_line, several.ok = FALSE)
    # determine historical value, if necessary. This will replace the ref_parm_value
    # determine the ref_parm_value
    
    if(isTRUE(!is.na(val_info[val_info$ts_param == val_line, "first_yr_averaging"]) &
              !is.na(val_info[val_info$ts_param == val_line, "last_yr_averaging"]))) {
      if(parname == "rec_devs") {
        tmp_vals <- data.frame(yrs = par$recdev1[,1], rec_devs = par$recdev1[,2])
        tmp_vals_2 <- data.frame(yrs = vals_df$yrs, rec_devs = vals_df$rec_devs)
        tmp_vals <- rbind(tmp_vals, tmp_vals_2)
        to_include <- which(tmp_vals$yr >= val_info[val_info$ts_param == val_line, "first_yr_averaging"] &
                              tmp_vals$yr <= val_info[val_info$ts_param == val_line, "last_yr_averaging"])
        tmp_vals <- tmp_vals[to_include, "rec_devs"]
        ref_parm_value <- switch(val_line, 
                                 mean = mean(tmp_vals), 
                                 sd = sd(tmp_vals), 
                                 ar_1_phi = stats::arima(tmp_vals, order = c(1,0,0))$coef[1])
      } else { # for all other parameters
        # check for tv devs
        if(isTRUE(!is.null(par[["parm_devs"]]))) { #TODO: implement this and remove the stop
          stop("code not yet written to deal with tv parms in original OM")
          # in this case, will need to do the historical avg if user requests it.
        } 
        # the base parm value should just be used as the reference in this case.
        ref_parm_value <- ref_parm_value # keeep using the default.
      }
    }
 
    # determine start val
    if(isTRUE(length(val_info[val_info$ts_param == val_line, "last_yr_orig_val"]) == 1 & 
              val_line == "mean")) {
      start_val_yr <- val_info[val_info$ts_param == val_line, "last_yr_orig_val"]
      if(start_val_yr %in% vals_df$yrs) {
      start_val <- vals_df[vals_df$yrs == start_val_yr, parname] 
      } else {
        # This should only be used when the start value is 1 year before the 
        # devs start.
        assert_is_identical_to_true(start_val_yr == (vals_df$yrs[1]) -1 )
        start_val <- ref_parm_value # Note: this may not be true if orig. val is time varying?
      }
    } else { # just use the base value
      start_val <- ref_parm_value # I'm not sure this is right or not for the sd???
    }
    last_yr <- vals_df[nrow(vals_df), "yrs"]
    #TODO: not sure this is exactly right. Getting confused about which is the
    if(isFALSE(val_line %in% val_info$ts_param)) {
      # use the start value
      return(start_val)
    }
    yrs_out <- val_info[val_info$ts_param == val_line, "first_yr_final_val"] -
      val_info[val_info$ts_param == val_line, "last_yr_orig_val"] + 1
    if(is.na((yrs_out))) yrs_out <- 2 # this is just a ste
    # get end value
    end <- switch(
      val_info[val_info$ts_param == val_line, "method"], 
      absolute = val_info[val_info$ts_param == val_line, "value"], 
      additive = val_info[val_info$ts_param == val_line, "value"]+ref_parm_value,
      multiplier = val_info[val_info$ts_param == val_line, "value"]*ref_parm_value)
    # Calculate a linear trend in between them
    trend <- seq(from = start_val, to = end, length.out = yrs_out)
    trend <- trend[-1] # because don't want to keep the year with the original value
    # after, want to keep the trend at the same value.
    end_val <- rep(end, length.out = last_yr - 
      val_info[val_info$ts_param == val_line, "first_yr_final_val"] )
    trend <- c(trend, end_val)
    # before, want to keep the trend at the original value. This is not always needed.
    if(val_info[val_info$ts_param == val_line, "last_yr_orig_val"] >= 
       vals_df$yrs[1]) {
      beg_val <- rep(start_val, 
        length.out =  val_info[val_info$ts_param == val_line,
                               "last_yr_orig_val"] - vals_df$yrs[1] + 1)
      trend <- c(beg_val, trend)
    }
    trend
  }
  #TODO: add test that allows CV to be used instead of sd? might be necessary
  # for the all params option..