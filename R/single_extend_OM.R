
#' Add in future parameter values
#' 
#' @param clt A control file as read in by r4ss::SS_readctl
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param parlist A parameter file as read in by r4ss::SS_readpar_3.30
#' @param future_om_list A future OM parameter specification object
#' @param nyrs The total number of years to run the MSE
#' @param nscen The scenario number
#' @param scen_name The scenario name 
#' @param niter The iteration number
#' 
extend_OM_full_period <- function(ctl, dat, parlist, future_om_list, nyrs, nscen, scen_name, niter) {

  applicable_changes<-list()
  
  for(i in 1:length(future_om_list)){
    temp_change<-future_om_list[[i]]
    
  }
  
  for(i in 1:length(ctl[["MG_parms"]])){
    
  }
  #First check the parameter file list for any time varying parameters with deviations.
  if(is.null(parlist[["parm_devs"]])){
    N_dev_pars <- 0
  }else{
    N_dev_pars <- length(parlist[["parm_devs"]])
    dev_pars_loc<-NULL
    ref_par<-0
    if(!is.null(clt[["MG_parms_tv"]])){
      dev_pars_loc <- c(dev_pars_loc,(which(ctl[["MG_parms"]][,9]!=0)+ref_par))
      ref_par <- ref_par+length(ctl[["MG_parms"]][,9])
    }
    if(!is.null(clt[["MG_parms_tv"]])){
      dev_pars_loc <- c(dev_pars_loc,(which(ctl[["MG_parms"]][,9]!=0)+ref_par))
      ref_par <- ref_par+length(ctl[["MG_parms"]][,9])
    }
    
    dev_pars_loc <- vector(length=N_dev_pars)
    for(i in 1:N_dev_pars){
      
    }
    
  }

  
  
}


#' Add in future parameter values
#' 
#' @param clt A control file as read in by r4ss::SS_readctl
#' 
#' 
get_dev_loc <- function(ctl){
  parvals<-ctl[[""]]


#Build mortality and growth parameter list
if(length(grep("MGparm",parvals))>0){
  #Read in the values for mortality and growth parameters
  MG_seq <- as.numeric(parvals[(grep("MGparm",parvals)+1)])
  #Create list object from the base control file parameter matrix
  if(!is.null(ctllist$MG_parms)){
    parlist$MG_parms<-ctllist$MG_parms[,3:4]
  }else{
    stop("Missing ctllist$MG_parms")
  }
  #Add time varying mortality and growth parameters if they exist
  if(any(ctllist$MG_parms[, c("env_var&link", "dev_link", "Block")] != 0)) {
    if(!is.null(ctllist$MG_parms_tv)) {
      parlist$MG_parms <- rbind(parlist$MG_parms,ctllist$MG_parms_tv[,3:4])
    } else {
      tmp_parlabs <- get_tv_parlabs(full_parms = ctllist[["MG_parms"]], 
                                    block_design = ctllist[["Block_Design"]])
      tmp_tv <- data.frame(INIT = rep(NA, times = length(tmp_parlabs)),
                           PRIOR = rep(NA, times = length(tmp_parlabs)),
                           row.names = tmp_parlabs, stringsAsFactors = FALSE)
      parlist$MG_parms <- rbind(parlist$MG_parms, tmp_tv)
    }
    dev_temp <- ctllist$MG_parms[ctllist$MG_parms[,9]>0,,drop=FALSE]
    if(length(dev_temp[,9])>0){
      dev_parm_start <- c(dev_parm_start,dev_temp[,10])
      dev_parm_end <- c(dev_parm_end,dev_temp[,11])
      dev_parm_labels <- c(dev_parm_labels,paste0(rownames(dev_temp),"_dev_seq"))
    }
  }
  #Add seasonal mortality and growth parameters if they exist
  if(!is.null(ctllist$MG_parms_seas)){
    parlist$MG_parms <- rbind(parlist$MG_parms,ctllist$MG_parms_seas[,3:4])
  }
  #Rename columns and add final parameter estimate data from par file
  colnames(parlist$MG_parms) <- c("INIT","ESTIM")
  parlist$MG_parms[,2] <- MG_seq
}

}


#' Get time varying parameter labels
#' 
#' function to add get the names of short time varying parameter lines
#' @param full_parms the dataframe with the full parameter lines in the control
#'  file as read in by r4ss.
#' @param block_design The block design in the control file as read in by r4ss.
get_tv_parlabs <- function(full_parms, 
                           block_design) {
  # Figure out parameters are time varying
  tmp_tv <- list(env   = full_parms[,"env_var&link"], 
                 dev   = full_parms[,"dev_link"],
                 block = full_parms[,"Block"])
  par_num <- lapply(tmp_tv, function(x) which(x != 0))
  loop_pars <- unlist(par_num)
  loop_pars <- loop_pars[order(loop_pars)]
  parlab <- vector() # a maybe faster approach is if we could initialize with the correct size.
  for(i in loop_pars) {
    tmp_parname <- rownames(full_parms)[i]
    # Add lines to the data frame as you go. (maybe can use the same approach as long parlines)
    if(i %in% par_num[["env"]]) {
      parlab <- c(parlab, paste0("# ", tmp_parname, "_ENV_add"))
    }
    if(i %in% par_num[["block"]]) {
      n_blk <- full_parms$Block[i]
      tmp_blk_design <- block_design[[n_blk]]
      # Get the start year for each block
      blk_start_yrs <- tmp_blk_design[seq(1, length(tmp_blk_design), by = 2)]
      parlab <- c(parlab,
                  paste0("# ", 
                         rep(tmp_parname,times = length(blk_start_yrs)),
                         "_BLK", n_blk, "add", blk_start_yrs))
    }
    if(i %in% par_num[["dev"]]) {
      # parameter name if there is devs
      parlab <- c(parlab, 
                  paste0("# ", 
                         rep(tmp_parname, times = 2), 
                         c("_dev_se", "_dev_autocorr")))
    }
  }
  invisible(parlab)
}
