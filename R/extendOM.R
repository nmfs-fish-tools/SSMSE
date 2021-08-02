# Functions to update catch and discards in the OM beyond the base end year.

#' Extend the OM forward using next years' catch
#'
#' Add the EM defined catch values for the next years.
#' @author Kathryn Doering & Nathan Vaughan
#' @param OM_dir The full path to the OM directory.
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g.,
#'  year,	season, fleet,	catch,	catch_se). Must input either a catch and/or a harvest rate
#'  data frame. If both are input the catch will override harvest rate as the management unit
#'  but harvest rate will be used as a starting guess for search.
#' @param harvest_rate A dataframe of harvest rate (F) values and associated information to
#'  add to the OM. The column names are as in an SS datafile. If harvest rate is input without
#'  a corresponding catch the OM will assume effort based management an use harvest rate directly
#'  with implementation error added. 
#' @param catch_basis data frame with columns year, seas, fleet, basis that specifies if catch
#'  should reference retained biomass (1) or dead biomass (2). Any year/season/fleet not listed will assume 
#'  a value of 1 referencing retained biomass. Entering -99 for any of year, season, or fleet will
#'  apply the basis across all values of that variable (i.e. a single row with -99, -99, -99, 1 would implement
#'  retained biomass for all cases)  
#' @param F_limit data frame with columns year, fleet, season, limit that specifies a maximum F 
#'  allowed in the OM or a negative value to specify a multiple of the historic maximum F. Any year/season/fleet 
#'  not listed will assume a value of 1.5. Entering -99 for any of year, season, or fleet will
#'  apply the limit across all values of that variable (i.e. a single row with -99, -99, -99, -2 would implement
#'  a cap of twice the historic maximum F for all cases)  
#' @param EM_pars a dataframe of parameter value updates to modify OM 
#' @param write_dat Should the datafile be overwritten? Defaults to TRUE.
#' @param impl_error The implementation error
#' @param seed A random initialization seed for SS to allow reproducibility
#' @template verbose
#' @return A new dat list object (format as created by r4ss::SS_readdat) that
#'  has been extended forward  as if read in by r4ss function SS_readdat
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
update_OM <- function(OM_dir,
                      catch = NULL,
                      harvest_rate = NULL,
                      catch_basis = NULL,
                      F_limit = NULL,
                      EM_pars = NULL,
                      write_dat = TRUE,
                      impl_error = NULL, 
                      verbose = FALSE,
                      seed = NULL) {
  # input checks

  if(is.null(catch) & is.null(harvest_rate)){
    stop("You have to input either a catch or a harvest rate")
  }
  if(!is.null(catch)){
    check_catch_df(catch)
  }
  check_dir(OM_dir)
  
  # read in the starter file to get OM file names
  start <- r4ss::SS_readstarter(file.path(OM_dir, "starter.ss"),
    verbose = FALSE
  )
  # extend the number of yrs in the model and add in catch
  dat <- r4ss::SS_readdat(file.path(OM_dir, start[["datfile"]]),
    verbose = FALSE,
    section = 1
  )
  # read in control file
  ctl <- r4ss::SS_readctl(
    file = file.path(OM_dir, start[["ctlfile"]]),
    version = "3.30", use_datlist = TRUE, datlist = dat,
    verbose = FALSE
  )
  # read in parameter file
  parlist <- r4ss::SS_readpar_3.30(
    parfile = file.path(OM_dir, "ss.par"),
    datsource = dat, ctlsource = ctl,
    verbose = FALSE
  )
  

  # SINGLE_RUN_MODS: No real need for this check as we are working in Fs that we can cap. Better just to give a 
  # warning that the fishery bounded at an F limit.
  
  if (is.null(seed)) {
    seed <- stats::runif(1, 1, 9999999)
  }

  start[["seed"]] <- seed

  r4ss::SS_writestarter(start,
    dir = OM_dir, verbose = FALSE, overwrite = TRUE,
    warn = FALSE
  )


  # first run OM with catch as projection to calculate the true F required to achieve EM catch in OM  # SINGLE_RUN_MODS: delete
  # Apply implementation error to the catches before it is added to the OM
  # modify forecast file ----
  
  catch_intended<-rbind(catch,harvest_rate)
  catch_intended<-catch_intended[!duplicated(catch_intended[,1:3]),]
  catch_intended<-cbind(catch_intended,catch_intended[,"catch"],catch_intended[,"catch"],rep(1,length(catch_intended[,"catch"])),rep(1,length(catch_intended[,"catch"])),rep(1,length(catch_intended[,"catch"])),rep(1.5,length(catch_intended[,"catch"])),rep(2,length(catch_intended[,"catch"])))
  colnames(catch_intended)<-c("year", "seas", "fleet", "catch","F","F_ref","Catch_ref","basis","basis_2","scale","F_lim","last_adjust")
  for(i in seq_along(catch_intended[,"catch"])){
    
    if(!is.null(F_limit)){
      F_lim <- F_limit[(F_limit[, "year"] == catch_intended[i, "year"]) &
                         (F_limit[, "seas"] == catch_intended[i, "seas"]) &
                         (F_limit[, "fleet"] == catch_intended[i, "fleet"]), "limit"]
      if(length(F_lim)!=1){
        F_lim <- 1.5
      }
    }else{
      F_lim <- 1.5
    }
    catch_intended[i,"F_lim"] <- F_lim
    
    if(!is.null(catch_basis)){
      basis_2 <- catch_basis[(catch_basis[, "year"] == catch_intended[i, "year"]) &
                         (catch_basis[, "seas"] == catch_intended[i, "seas"]) &
                         (catch_basis[, "fleet"] == catch_intended[i, "fleet"]), "basis"]
      if(length(basis_2)!=1){
        basis_2 <- 1
      }
    }else{
      basis_2 <- 1
    }
    catch_intended[i,"basis_2"] <- basis_2
    
    last_F <- parlist[["F_rate"]][which(parlist[["F_rate"]][,c("year")]==(catch_intended[i,c("year")]-1) & 
                                          parlist[["F_rate"]][,c("seas")]==catch_intended[i,c("seas")] &
                                          parlist[["F_rate"]][,c("fleet")]==catch_intended[i,c("fleet")]),"F"]
    
    if(length(last_F)==0){
      last_F<-0
    }
    
    last_catch <- dat[["catch"]][which(dat[["catch"]][,c("year")]==(catch_intended[i,c("year")]-1) & 
                                         dat[["catch"]][,c("seas")]==catch_intended[i,c("seas")] &
                                         dat[["catch"]][,c("fleet")]==catch_intended[i,c("fleet")]),"catch"]
    
    if(length(last_catch)==0){
      last_catch<-0
    }
    
    if(length(which(parlist[["F_rate"]][,c("year")]==catch_intended[i,c("year")] & 
                    parlist[["F_rate"]][,c("seas")]==catch_intended[i,c("seas")] &
                    parlist[["F_rate"]][,c("fleet")]==catch_intended[i,c("fleet")]))==1){
    catch_intended[i,"F_ref"] <- which(parlist[["F_rate"]][,c("year")]==catch_intended[i,c("year")] & 
                                  parlist[["F_rate"]][,c("seas")]==catch_intended[i,c("seas")] &
                                  parlist[["F_rate"]][,c("fleet")]==catch_intended[i,c("fleet")])
    }else{catch_intended[i,"F_ref"]<-NA}
    
    catch_intended[i,"Catch_ref"] <- which(dat[["catch"]][,c("year")]==catch_intended[i,c("year")] & 
                                      dat[["catch"]][,c("seas")]==catch_intended[i,c("seas")] &
                                      dat[["catch"]][,c("fleet")]==catch_intended[i,c("fleet")])
    
    if(is.na(catch_intended[i,"F_ref"])){
      catch_intended[i,"F"] <- 0
    }else{
      catch_intended[i,"F"] <- parlist[["F_rate"]][catch_intended[i,"F_ref"],"F"]
    }
    
    if(is.na(catch_intended[i,"Catch_ref"])){
      catch_intended[i,"catch"] <- 0
    }else{
      catch_intended[i,"catch"] <- dat[["catch"]][catch_intended[i,"Catch_ref"],"catch"]
    }
    
    if(!is.null(catch)){
      temp_catch <- catch[catch[,"year"]==catch_intended[i,"year"] & 
                          catch[,"seas"]==catch_intended[i,"seas"] & 
                          catch[,"fleet"]==catch_intended[i,"fleet"], "catch"]
      if(length(temp_catch)==1){
        catch_intended[i,"catch"] <- temp_catch
        catch_intended[i,"basis"] <- 1
      }else{
        catch_intended[i,"catch"] <- 0.001
        catch_intended[i,"basis"] <- 2
      }
    }else{
      catch_intended[i,"catch"] <- 0.001
      catch_intended[i,"basis"] <- 2
    }
    
    if(!is.null(harvest_rate)){
      temp_harvest_rate <- harvest_rate[harvest_rate[,"year"]==catch_intended[i,"year"] & 
                                        harvest_rate[,"seas"]==catch_intended[i,"seas"] & 
                                        harvest_rate[,"fleet"]==catch_intended[i,"fleet"], "catch"]
      if(length(temp_harvest_rate)==1){
        catch_intended[i,"F"] <- temp_harvest_rate
      }else{
        if(last_catch<=0){
          if(catch_intended[i,"catch"]==0){
            catch_intended[i,"F"] <- 0
          }else{
            if(last_F==0){
              catch_intended[i,"F"] <- 0.01
            }else{
              catch_intended[i,"F"] <- last_F
            }
          }
        }else{
          
          catch_intended[i,"F"] <- (-log(1-((catch_intended[i,"catch"]/max(last_catch,0.01))*(1-exp(-max(last_F,0.01))))))
        }
      }
      
      if(catch_intended[i,"basis"] == 2){
        catch_intended[i,"catch"] <- max(last_catch,0.01)*(1-exp(-catch_intended[i,"F"]))/(1-exp(-max(last_F,0.01)))
      }
    }else{
      if(last_catch<=0){
        if(catch_intended[i,"catch"]==0){
          catch_intended[i,"F"] <- 0
        }else{
          if(last_F==0){
            catch_intended[i,"F"] <- 0.01
          }else{
            catch_intended[i,"F"] <- last_F
          }
        }
      }else{
        catch_intended[i,"F"] <- (-log(1-((catch_intended[i,"catch"]/max(last_catch,0.01))*(1-exp(-max(last_F,0.01))))))
      }
    }
    
    
    if(!is.null(impl_error)){
      # temp_impl_error <- impl_error[impl_error[,"year"]==temp_catch[i,"year"] & 
      #                               impl_error[,"seas"]==temp_catch[i,"seas"] & 
      #                               impl_error[,"fleet"]==temp_catch[i,"fleet"] ,"error"]
      temp_impl_error <- impl_error[impl_error[,"year"]==catch_intended[i,"year"],"error"]
      if(length(temp_impl_error)==1){
        if(temp_impl_error>=0){
         catch_intended[i,c("catch","F")] <- catch_intended[i,c("catch","F")] * temp_impl_error
        }
      }
    }
    if(!is.na(catch_intended[i,"Catch_ref"])){
      dat[["catch"]][catch_intended[i,"Catch_ref"],"catch"] <- catch_intended[i,"catch"]
      if(catch_intended[i,"catch"]==0){
        dat[["catch"]] <- dat[["catch"]][-catch_intended[i,"Catch_ref"],]
        catch_intended[i,"Catch_ref"] <- NA
      }
    }
    if(!is.na(catch_intended[i,"F_ref"])){
      parlist[["F_rate"]][catch_intended[i,"F_ref"],"F"] <- catch_intended[i,"F"]
      if(catch_intended[i,"F"]==0){
        parlist[["F_rate"]] <- parlist[["F_rate"]][-catch_intended[i,"F_ref"],]
        catch_intended[i,"F_ref"] <- NA
      }
    }
    
    if(catch_intended[i,"catch"]==0 | catch_intended[i,"F"]==0){
      catch_intended[catch_intended[,"year"] > catch_intended[i,"year"],"Catch_ref"] <- pmax(0,catch_intended[catch_intended[,"year"] > catch_intended[i,"year"],"Catch_ref"] - 1)
      catch_intended[catch_intended[,"year"] > catch_intended[i,"year"],"F_ref"] <- pmax(0,catch_intended[catch_intended[,"year"] > catch_intended[i,"year"],"F_ref"] - 1)
    }
  }
  
  catch_intended <- catch_intended[catch_intended[,"catch"]>0,,drop=FALSE]
  catch_intended <- catch_intended[catch_intended[,"F"]>0,,drop=FALSE]
  
  #dat[["catch"]] <-  dat[["catch"]][dat[["catch"]][,"catch"]>0,]
  #parlist[["F_rate"]] <- parlist[["F_rate"]][parlist[["F_rate"]][,"F"]>0,]
  
  if(!is.null(EM_pars)){
    for(i in grep("rec_devs",names(EM_pars))){
      matches<-lapply(EM_pars[["year"]],FUN=function(x,y){which(y==x)},y=parlist[["recdev_forecast"]][,"year"])
      locations<-rep(EM_pars[["year"]],unlist(lapply(matches,length)))
      parlist[["recdev_forecast"]][unlist(matches),c("year","recdev")] <- EM_pars[locations,c(1,i)]
    }
    
    for(i in grep("Env_",names(EM_pars))){
      dat[["envdat"]][is.element(dat[["envdat"]]["Yr"],EM_pars[["year"]]) & is.element(dat[["envdat"]]["Variable"],as.numeric(strsplit(names(EM_pars)[i],"Env_")[[1]][2])), c("yr","Value")] <- EM_pars[,c(1,i)]
    }
    
    dev_names <- paste0(names(EM_pars),"_dev_seq")
    for(i in which(is.element(names(parlist[["parm_devs"]]),dev_names))){
      parlist[["parm_devs"]][[i]][is.element(parlist[["parm_devs"]][[i]][,"year"],EM_pars[,"year"]),] <- EM_pars[,c(1,i)]
    }
  }
  
  # write new files
  r4ss::SS_writepar_3.30( 
    parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
    overwrite = TRUE, verbose = FALSE 
  ) 
  
  r4ss::SS_writedat(dat,
                    outfile = file.path(OM_dir, start[["datfile"]]),
                    overwrite = TRUE,
                    verbose = FALSE
  )
  
  if(length(catch_intended[,1])>0){
    achieved_Catch <- FALSE
  }else{
    achieved_Catch <- TRUE
  }
  search_loops <- 0
  while (achieved_Catch == FALSE) {
    if(max(abs(catch_intended[,"last_adjust"]-1))>0.001 & search_loops < 20){
      achieved_Catch <- FALSE
      
      r4ss::SS_writepar_3.30( 
        parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
        overwrite = TRUE, verbose = FALSE 
      )
    
      search_loops <- search_loops + 1
      
      # Run SS with the new catch set as forecast targets. This will use SS to
      # calculate the F required in the OM to achieve these catches.
      run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess",
        verbose = verbose,
        debug_par_run = TRUE
      )
      # Load the SS results
      outlist <- r4ss::SS_output(OM_dir,
        verbose = FALSE, printstats = FALSE,
        covar = FALSE, warn = FALSE, readwt = FALSE
      )
      
      # Extract the achieved F and Catch 
      F_list <- get_F( 
        timeseries = outlist[["timeseries"]],
        fleetnames = dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "fleetname"]
      )
  
      units_of_catch <- dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "units"]
  
      names(units_of_catch) <- as.character(which(dat[["fleetinfo"]][["type"]] %in% c(1, 2)))
  
      ret_catch <- get_retained_catch(
        timeseries = outlist[["timeseries"]],
        units_of_catch = units_of_catch
      )
      
      dead_catch <- get_dead_catch(
        timeseries = outlist[["timeseries"]],
        units_of_catch = units_of_catch
      )
      
      F_achieved <- F_list[["F_df"]][, c("Yr", "Seas", "Fleet", "F")]  
      colnames(F_achieved) <- c("year", "seas", "fleet", "F")
      
      for (i in 1:length(catch_intended[,1])) {
        scale_ratio <- catch_intended[i, "basis"]
  
        if(catch_intended[i, "basis"]==1){
          intended_landings <- catch_intended[i, "catch"]
          
          if(catch_intended[i, "basis_2"]==1){
            achieved_landings <- ret_catch[ret_catch[, "Yr"] == catch_intended[i, "year"] &
                                        ret_catch[, "Seas"] == catch_intended[i, "seas"] &
                                        ret_catch[, "Fleet"] == catch_intended[i, "fleet"], "retained_catch"]
          }else if(catch_intended[i, "basis_2"]==2){
            achieved_landings <- dead_catch[dead_catch[, "Yr"] == catch_intended[i, "year"] &
                                          dead_catch[, "Seas"] == catch_intended[i, "seas"] &
                                          dead_catch[, "Fleet"] == catch_intended[i, "fleet"], "retained_catch"]
          }else{
            stop("intended basis 2 should be equal to 1 or 2 but it is not")
          }
          
          achieved_F <- F_achieved[F_achieved[, "year"] == catch_intended[i, "year"] &
                                     F_achieved[, "seas"] == catch_intended[i, "seas"] &
                                     F_achieved[, "fleet"] == catch_intended[i, "fleet"], "F"]
          
          if(intended_landings==0){
            target_F <- 0
          }else if(achieved_landings==0){
            if(achieved_F > 0){
              catch_intended[i, "basis_2"] <- 2
            }else{
              target_F <- catch_intended[i, "F"] + 0.01
              catch_intended[i, "F"] <- target_F
            }
          }else{
            target_F <- (-log(1-((intended_landings/achieved_landings)*(1-exp(-achieved_F)))))
          }
        }else if(catch_intended[i, "basis"]==2){
          target_F <- catch_intended[i, "F"]
          
          achieved_F <- F_achieved[F_achieved[, "year"] == catch_intended[i, "year"] &
                                     F_achieved[, "seas"] == catch_intended[i, "seas"] &
                                     F_achieved[, "fleet"] == catch_intended[i, "fleet"], "F"]
          
        }else{
          stop("Something is wrong basis should be 1 or 2")
        }
        
        if(target_F == 0){
          catch_intended[i,"last_adjust"] <- 1
          catch_intended[i, "scale"] <- 0
        }else if(achieved_F==0){
          catch_intended[i,"last_adjust"] <- 1
          catch_intended[i, "scale"] <- 1
        }else{
          catch_intended[i,"last_adjust"] <- target_F/achieved_F
          catch_intended[i, "scale"] <- catch_intended[i, "scale"]*((target_F/achieved_F)-1)*(1-exp(runif(1,-5,0)))
        }
        
        if(!is.na(catch_intended[i,"F_ref"])){
          parlist[["F_rate"]][catch_intended[i,"F_ref"],"F"] <- max(0,min(catch_intended[i,"F"]*catch_intended[i, "scale"],catch_intended[i,"F_lim"]))
          if(parlist[["F_rate"]][catch_intended[i,"F_ref"],"F"]==0 | parlist[["F_rate"]][catch_intended[i,"F_ref"],"F"]==catch_intended[i,"F_lim"]){
            catch_intended[i,"last_adjust"] <- 1
          }
        }
      }
    }else {
      achieved_Catch <- TRUE
        
      parlist <- r4ss::SS_readpar_3.30(
        parfile = file.path(OM_dir, "ss.par"),
        datsource = dat, ctlsource = ctl,
        verbose = FALSE
      )
      
      if (search_loops == 20) {
        utils::write.csv("The catch search loop ran for 20 iterations without achieving targets",
                           file = file.path(OM_dir, "search_took_too_long.csv")
        )
      }
    }
  }
  
  # TODO: need to add code to overwrite OM parameter devs if updates are input from a custom EM
  
  invisible(dat)
}

#' Add in years of sampling data needed
#' 
#' @param sample_struct The sampling structure, as specified by the user.
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param nyrs_extend Number of years to extend the OM forward. Use 0 if using
#'  this function on the initial (historical) run of the OM.
add_sample_struct <- function(sample_struct, dat, nyrs_extend) {
  if (is.null(sample_struct)) {
    return(dat)
  }
  if(nyrs_extend > 0) {
    subset_yr_start <- dat[["endyr"]] + 1
    subset_yr_end <- dat[["endyr"]] + nyrs_extend
  }
  if (nyrs_extend == 0) {
    subset_yr_start <- dat[["styr"]]
    subset_yr_end <- dat[["endyr"]]
  }
  
  tmp_CPUE <- sample_struct[["CPUE"]]
  if (!is.null(tmp_CPUE)) {
    tmp_CPUE <- tmp_CPUE[tmp_CPUE[["year"]] >= subset_yr_start &
                           tmp_CPUE[["year"]] <= subset_yr_end, ]
    if (nrow(tmp_CPUE) > 0) {
      tmp_CPUE[["obs"]] <- 1 # dummy observation
      tmp_CPUE <- tmp_CPUE[, c("year", "seas", "index", "obs", "se_log")]
      tmp_CPUE[["index"]] <- -abs(tmp_CPUE[["index"]])
      dat[["CPUE"]] <- rbind(dat[["CPUE"]], tmp_CPUE)
    }
  }
  
  # This method of adding new data doesn't work if len comp is not already
  # turned on. Add warninig for now, but could potentially turn on len comp
  # for the user in the OM?
  if (dat[["use_lencomp"]] == 0 & !is.null(sample_struct[["lencomp"]])) {
    warning(
      "Length composition is not specified in the OM, but the lencomp ",
      "sampling was requested through sample_struct. Please turn on ",
      "length comp in the OM to allow lencomp sampling."
    )
  }
  if (dat[["use_lencomp"]] == 1 & !is.null(sample_struct[["lencomp"]])) {
    tmp_lencomp <- sample_struct[["lencomp"]]
    tmp_lencomp <- tmp_lencomp[tmp_lencomp[["Yr"]] >= subset_yr_start &
                                 tmp_lencomp[["Yr"]] <= subset_yr_end, ]
    if (nrow(tmp_lencomp) > 0) {
      # get col names
      lencomp_dat_colnames <- colnames(dat[["lencomp"]])[7:ncol(dat[["lencomp"]])]
      tmp_df_dat <- matrix(1,
                           nrow = nrow(tmp_lencomp),
                           ncol = length(lencomp_dat_colnames)
      )
      colnames(tmp_df_dat) <- lencomp_dat_colnames
      tmp_lencomp <- cbind(tmp_lencomp, as.data.frame(tmp_df_dat))
      tmp_lencomp[["FltSvy"]] <- -abs(tmp_lencomp[["FltSvy"]]) # make sure negative
      dat[["lencomp"]] <- rbind(dat[["lencomp"]], tmp_lencomp)
    }
  }
  # TODO: can write code that adds age comp obs when dat[["agecomp"]] is NULL.
  if (is.null(dat[["agecomp"]]) & !is.null(sample_struct[["agecomp"]])) {
    warning(
      "Age composition is not specified in the OM, but the agecomp ",
      "sampling was requested through sample_struct. Please turn on ",
      "age comp in the OM by adding at least  to allow agecomp ",
      "sampling."
    )
  }
  if (!is.null(dat[["agecomp"]]) & !is.null(sample_struct[["agecomp"]])) {
    tmp_agecomp <- sample_struct[["agecomp"]]
    tmp_agecomp <- tmp_agecomp[tmp_agecomp[["Yr"]] >= subset_yr_start &
                                 tmp_agecomp[["Yr"]] <= subset_yr_end, ]
    if (nrow(tmp_agecomp) > 0) {
      # get col names
      agecomp_dat_colnames <- colnames(dat[["agecomp"]])[10:ncol(dat[["agecomp"]])]
      tmp_df_dat <- matrix(1,
                           nrow = nrow(tmp_agecomp),
                           ncol = length(agecomp_dat_colnames)
      )
      colnames(tmp_df_dat) <- agecomp_dat_colnames
      tmp_agecomp <- cbind(tmp_agecomp, as.data.frame(tmp_df_dat))
      tmp_agecomp[["FltSvy"]] <- -abs(tmp_agecomp[["FltSvy"]]) # make sure negative
      dat[["agecomp"]] <- rbind(dat[["agecomp"]], tmp_agecomp)
    }
  }
  dat
}


#' Check future catch smaller than the last year's population size.
#'
#' Note that it could still be possible to take out too much catch from the
#' population, so this may not catch all instances of too much catch
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g.,
#'  year,	season, fleet,	catch,	catch_se).
#' length of the number of years (only works when catch is for 1 fleet)
#' @param OM_dir The full path to the OM directory.
#' @param datfile The optional name (as a character string) of the datafile,
#'  presumed to exist in \code{OM_dir}. Defaults to NULL, and if is NULL, the
#'  function will get the datfile name from the starter.ss file in \code{OM_dir}.
#' @param catch_units What units is the catch in? "bio" for biomass or "num" for
#'   numbers? Defaults to "bio".
#' @author Kathryn Doering
#' @importFrom r4ss SS_read_summary SS_readstarter SS_readdat
check_future_catch <- function(catch, OM_dir, catch_units = "bio",
                               datfile = NULL) {
  # TODO: add checks for discards???
  # input checks
  check_catch_df(catch)
  check_dir(OM_dir)
  summary <- r4ss::SS_read_summary(file.path(OM_dir, "ss_summary.sso"))
  if (is.null(datfile)) {
    start <- SS_readstarter(file.path(OM_dir, "starter.ss"), verbose = FALSE)
    dat <- SS_readdat(file.path(OM_dir, start[["datfile"]]),
      verbose = FALSE,
      section = 1
    )
  } else {
    dat <- SS_readdat(file.path(OM_dir, datfile),
      verbose = FALSE,
      section = 1
    )
  }
  if (is.null(summary)) {
    stop(
      "File ss_summary.sso was not found in directory: ", OM_dir, ". Please",
      " add the file to the directory or change OM_dir to one with this ",
      "file."
    )
  }
  if (is.null(dat)) {
    stop(
      "Datafile was not found in directory: ", OM_dir, ". Please",
      " add the file to the directory or change OM_dir to one with this ",
      "file."
    )
  }
  # TODO: check that can you always get biomass for any model? Probalby not if
  # catch units are in numbers. Any other scenarios when this is true?
  if (catch_units == "bio") {
    tot_bio_lyear <-
      summary[["biomass"]][
        grep(paste0("TotBio_", dat[["endyr"]]), rownames(summary[["biomass"]])),
      ]
    if (dat[["endyr"]] >= min(catch[["year"]])) {
      stop(
        "The highest year for which TotBio in ss_summary.sso is available (in",
        " the dir ", OM_dir, " is ", dat[["endyr"]], " which is equal to or higher than ",
        "the minimum year value in catch, which is ", min(catch[["year"]]), ". ",
        "The catch should only contain values in the future compared to the ",
        "model summary."
      )
    }
    if (any(catch[["catch"]] > tot_bio_lyear[["Value"]])) {
      stop(
        "Some input values for future catch are higher than the most recent",
        " year's total biomass. Recent total biomass: ",
        tot_bio_lyear[["Value"]], "; future catch: ",
        paste0(catch[["catch"]], collapse = ", ")
      )
      # TODO: Maybe write a warning and work around instead of stop?
    }
  } else {
    stop("Function not yet implemented when catch is not in biomass.")
  }
  # return catch invisibly
  invisible(catch)
}
