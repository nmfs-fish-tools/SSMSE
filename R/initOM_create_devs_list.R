#' Create the devs dataframe for a scenario from user input
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
#' @return A dataframe of devs values with parameter names to put into the OM
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
  #now, add changes on top of this. 
  for (i in seq_len(length(future_om_list))) {
    vals_df <- add_dev_changes(fut_list = future_om_list[[i]], scen = scen_name, iter = niter, 
                               par = par, dat = dat, vals_df = vals_df, nyrs = nyrs)
  }
  vals_df
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
                        data.frame(pars = "rec_devs", obj = "recdevs1"))
  if(isTRUE(list_pars == "all")) {
    # note that impl_error is not included in all for now; not sure if it should be?
    return(par_name_tbl)
  }
  par_name_tbl <- rbind(par_name_tbl, data.frame(pars = "impl_error", obj = NA))
  where_pars <- match(list_pars, par_name_tbl[["pars"]])
  subset_parname_tbl <- par_name_tbl[where_pars,]
  est <- vector(mode = "numeric", length = nrow(subset_parname_tbl))
  for(r in seq_len(nrow(subset_parname_tbl))) {
    tmp_tbl <- par[[subset_parname_tbl[r, "obj"]]]
    tmp_est <- tmp_tbl[ rownames(tmp_tbl) == subset_parname_tbl[r,"pars"],"ESTIM"]
    est[r] <- tmp_est
  }
  subset_parname_tbl <- cbind(subset_parname_tbl, data.frame(est = est))
  subset_parname_tbl
}


#' @param mean A single value or vector of mean parameters
#' @param sd A single value or vector of sd parameter
#' @param seed the seed that will be set before sampling
#' @param ndevs The number of deviations to expect
sample_devs <- function(mean,
                        sd,
                        ts_params = NA,
                        seed,
                        ndevs, 
                        dist = c("normal", "lognormal")) {
  # checks
  if(!is.na(ts_params)) {
    stop("sampling fxn for time series params not yet written")
  }
  if(length(mean) > 1) {
    assertive.properties::assert_is_of_length(mean, ndevs)
  }
  if(length(sd) > 1) {
    assertive.properties::assert_is_of_length(sd, ndevs)
  }
  # sample
  set.seed(seed)
  devs <- switch(dist,
         normal = rnorm(n = ndevs, mean = mean, sd = sd), 
         # user input is on the log scale? need to make this clear.
         lognormal = rlnorm(n = ndevs, meanlog = mean, sdlog = sd)) 
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
    # check for tv devs
    if(isTRUE(!is.null(par[["parm_devs"]]))) {
      stop("code not yet written to deal with tv parms in original OM")
      # in this case, will need to do the historical avg if user requests it.
    }
    # if there are no tv devs in the original model, than taking a historical
    # average is unnecessary.
    for(i in where_par$pars) {
      # change seed used based on if replicate or random option is used.
      # next, look at distribution to see which ts_params to expect.

      # see what tv_params are there. Fill in missing values for parameters that
      # are not specified. (Skipping for now....)
      
      # TODO: finish this section. Do the sampling as needed. 
      # sampling needs to be done if patttern is listed as model_change.
      # look at input df to determine what should be used to do the sampling
      if(fut_list$pattern[[2]] == "normal") { #need a mean and sd
        ts_params <- NA  # not sure if this is really necessary; 
        
        # find the mean: 1) is a mean specified? If not, use the most recent
        # value. 
          # do some calcs to figure out the mean value
          # this holds if the parameter isn't time varying 
          # calculate the mean. The  below holds with or without a trend
  
          mean <- calc_par_trend(val_info = fut_list$input, 
                                 val_line = "mean",
                                 ref_parm_value = where_par[where_par$pars == i, "est"],
                                 vals_df = vals_df, # use to potentially get trend start value.
                                 parname = i
                                 )
          sd <- calc_par_trend(val_info = fut_list$input, 
                               val_line = "sd",
                               ref_parm_value = 0, # may not always be correct?
                               vals_df = vals_df, # use to get trend start value, and last yr. 
                               parname = i
                               )

            
        # if it is specified: then parse this input to figure out what the 
        # sampled mean should be.
        # find the sd: 2) is the sd specified? If not, use sd of 1 to sample (or historical sd?)
        if(isTRUE(any(!fut_list$input$ts_param %in% c("mean", "sd")))) {
          stop("timeseries params not yet implemented")
          # do some calcs to figure out the time varying parameter values
          #TODO, but leave empty for now.
        }
        devs <- sample_devs(mean = mean, sd = sd, ts_params = ts_params,
                                 ndevs = nyrs,
                                 seed = fut_list$seed, dist = "normal")
        # this works for 1 parameter only

        vals_df[[i]] <- devs # Maybe these should just replace what is alredy there?
      }

      # Now, should have a complete list of the dev vals to apply
    }
  } else if (fut_list$pattern[1] == "custom") {
    # custom ---
    for (i in where_par$pars) {
    # custom will just replace any values for the pattern.
    #filter out the columns needed for the given scenario and iteration
    custom_vals <- fut_list$input[fut_list$input$scen %in% c("all", scen) &
                             fut_list$input$par == i &
                             fut_list$input$iter == iter,]
    custom_vals <- dplyr::select(custom_vals, yr, value) %>%
      dplyr::rename(yrs = yr)
    vals_df <- dplyr::left_join(vals_df, custom_vals, by = "yrs") %>% 
      tidyr::replace_na(replace = list(value = 0))
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
  calc_par_trend <- function(val_info,val_line = c("mean", "sd"), ref_parm_value, vals_df, parname) {
    val_line <- match.arg(val_line, several.ok = FALSE)
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
    yrs_out <- val_info$first_yr_final_val - val_info$last_yr_orig_val + 1
    if(is.na((yrs_out))) yrs_out <- 2 # this is just a ste
    # get end value
    end <- switch(
      val_info[val_info$ts_param == val_line, "method"], 
      absolute = val_info[val_info$ts_param == val_line, "value"], 
      multiplier = val_info[val_info$ts_param == val_line, "value"]*ref_parm_value)
    # Calculate a linear trend in between them
    trend <- seq(from = start_val, to = end, length.out = yrs_out)
    trend <- trend[-1] # because don't want to keep the year with the original value
    # after, want to keep the trend at the same value.
    end_val <- rep(end, length.out = last_yr - val_info$first_yr_final_val)
    trend <- c(trend, end_val)
    # before, want to keep the trend at the original value. This is not always needed.
    if(val_info$last_yr_orig_val >= vals_df$yrs[1]) {
      beg_val <- rep(start_val, length.out =  val_info$last_yr_orig_val - vals_df$yrs[1] + 1)
      trend <- c(beg_val, trend)
    }
    trend
  }