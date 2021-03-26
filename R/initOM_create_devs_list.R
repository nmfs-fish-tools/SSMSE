#' Create the devs dataframe for a scenario from user input
#'
#'This function parses user inputs to convert it into a dataframe of deviations.
#'
#' @param future_om_list User input to runSSMSE
#' @param scenario The scenario name
#' @param iteration The scenario number
#' @param om_mod_path Path to the OM files. Used to reference parameter names.
#' @param nyrs The total number of years that the model will be extended forward.
#' @author Kathryn Doering
#' @return A dataframe of devs values with parameter names to put into the OM
convert_future_om_list_to_devs_df <- function(future_om_list, scen_name,
                                              niter, om_mod_path, nyrs, tvdevs 
                                              ) {
  #Note: may need to think about how this function is called, as it may not be
  # possible to call it for each iteration and scenario separately, as in some
  # cases, values are shared across scenarios
  if(isTRUE(is.null(future_om_list))) { # in case future_om_list is NULL to start with.
    dev_vals_df <- NULL
    return(dev_vals_df)
  }
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
  dev_vals_list <- lapply(future_om_list, function(fut_list, om, nyrs) {
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
      # next, look at distribution to see which ts_params to expect.
      # see what tv_params are there. Fill in missing values for parameters that
      # are not specified. 
      
      # Do the sampling as needed. 
      # calculate the devs
      # put them in a new column of the dataframe.
      
      dev_vals_df
    }
    
  }, om = om_mod_path, nyrs = nyrs)
  
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