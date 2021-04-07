#' Create the devs dataframe for a scenario from user input
#'
#'This function parses user inputs to convert it into a dataframe of deviations.
#'
#' @param future_om_list User input to runSSMSE
#' @param scenario The scenario name
#' @param iteration The scenario number
#' @param om_mod_path Path to the OM files. Used to reference parameter names.
#' @param nyrs The total number of years that the model will be extended forward.
#' @param global_seed A global seed to set, then pull new seeds from global seed + 1.
#' Defaults to 123.
#' @author Kathryn Doering
#' @return A dataframe of devs values with parameter names to put into the OM
convert_future_om_list_to_devs_df <- function(future_om_list, scen_name,
                                              niter, om_mod_path, nyrs, tvdevs,
                                              global_seed = 123
                                              ) {
  if(isTRUE(is.null(future_om_list))) { # in case future_om_list is NULL to start with.
    dev_vals_df <- NULL
    return(dev_vals_df)
  }
  # create the seeds to use for each list element of future_om_list, one seed per element.
  # add some arbitrary numbers to the global seed; but maybe there is a more
  # thoughtful way to do this? 
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

  dev_vals_list <- lapply(future_om_list, function(fut_list, om, nyrs, scen) {
    # do sampling for things that require sampling
    if(fut_list[["pattern"]][1] == "model_change") {
      start <- r4ss::SS_readstarter(file = file.path(om, "starter.ss"), 
                                    verbose = FALSE)
      dat <- r4ss::SS_readdat(file.path(om, start[["datfile"]]), verbose = F)
      par <- r4ss::SS_readpar_3.30(parfile = file.path(om, "ss.par"), 
                            datsource = file.path(om, start$datfile), 
                            ctlsource = file.path(om, start$ctlfile), 
                            verbose = FALSE)
      where_par <- match_parname(list_pars = fut_list[["pars"]], par = par)
      dev_vals_df <- data.frame(yrs = seq_len(nyrs) + dat[["endyr"]])
      # now, parse the input part (and pattern[2])
      # check for tv devs
      if(isTRUE(!is.null(par[["parm_devs"]]))) {
        stop("code not yet written to deal with tv parms in original OM")
        # in this case, will need to do the historical avg if user requests it.
      }
      # if there are no tv devs, than taking a historical average is unnecessary;
      # is just a single value
      # this assumes a single value. will need to loop over pars instead fo
      # some cases.
      hist_val <- par[[where_par[["obj"]] ]][
        rownames(par[[where_par[["obj"]]]])== where_par[["pars"]],
        "ESTIM"]
      # change seed used based on if replicate or random option is used.
      # next, look at distribution to see which ts_params to expect.
      if(fut_list$scen == "randomize") {
        # need a different seed for each scenario. Just add the number element
        # the scen is to the seed (check: will this behave pseudorandomly?)
        fut_list$seed <- fut_list$seed + which(fut_list$scen == scen)
      }
      # see what tv_params are there. Fill in missing values for parameters that
      # are not specified. (Skipping for now....)
      
      # TODO: finish this section. Do the sampling as needed. 
      # sampling needs to be done if patttern is listed as model_change.
      if(fut_list$pattern == "model_change") {
        # sampling needs to be done.
        dist <- fut_list$pattern[[2]]
        # look at input df to determine what should be used to do the sampling
        if(dist == "normal") { #need a mean and sd
          # set up the default vaues first
          tv_parms <- NA
          mean <- hist_val # would need to add some deviation on this if tving
          sd <- 1*mean # is 1 or the using a cv of 1 with most recent value a better default?
          
          # find the mean: 1) is a mean specified? If not, use the most recent
          # value. 
          if(isTRUE(any(fut_list$input$ts_param == "mean"))) {
            # do some cals to figure out the mean value
          }
          # if it is specified: then parse this input to figure out what the 
          # sampled mean should be.
          # find the sd: 2) is the sd specified? If not, use sd of 1 to sample (or historical sd?)
          if(isTRUE(any(fut_list$input$ts_param == "sd"))) {
            # do some cals to figure out the mean value
          }
          
        }
      }
      
      
      # calculate the devs
      # put them in a new column of the dataframe.
      
      dev_vals_df
    }
    
  }, om = om_mod_path, nyrs = nyrs, scen = scen_name)
  
  # transform values from list into a data frame
  # data.frame <- 
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
  subset_parname_tbl
}