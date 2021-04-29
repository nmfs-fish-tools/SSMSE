
#' Add in future parameter values
#' 
#' @param clt A control file as read in by r4ss::SS_readctl
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param parlist A parameter file as read in by r4ss::SS_readpar_3.30
#' @param timeseries SS output timeseries data
#' @param future_om_dat A data frame with random sample data for future parameter
#' 
#' @author Nathan Vaughan
add_OM_devs <- function(ctl, dat, parlist, timeseries, future_om_dat) {

  #First check if their is any data to add if not just pass back the original files with end year updated
  if(!is.null(future_om_dat)){
    
    #First check for recruitment deviation projections and implement them
    for(i in grep("rec_devs",names(future_om_dat))){
      late_years <- dat[["endyr"]]-dat[["MainRdevYrLast"]]
      late_devs <- parlist[["recdev_forecast"]][0:late_years,"recdev"]
      parlist[["recdev_forecast"]][,"year"] <- (dat[["endyr"]]-late_years+1):(dat[["endyr"]]+length(future_om_dat[,i]))
      parlist[["recdev_forecast"]][,"recdev"] <- c(late_devs,future_om_dat[,i])
    }
    
    #Next check for environmental index projections and add them
    for(i in grep("Env_",names(future_om_dat))){
      temp_env<-data.frame(Yr=(dat[["endyr"]]+1):(dat[["endyr"]]+length(future_om_dat[,i])),
                           Variable=rep(as.numeric(strsplit(names(future_om_dat)[i],"Env_")[[1]][2]),length(future_om_dat[,i])),
                           Value=future_om_dat[,i])
      dat[["envdat"]]<-rbind(dat[["envdat"]],temp_env)  
    }
    
    #Set up dummy time varying parameter lines for the control file and parameter file which will be added in for new data
    tv_dummy <- data.frame(LO=c(-10,-10), HI=c(10,10), INIT=c(1,1), PRIOR=c(1,1), PR_SD=c(1,1), PR_type=c(0,0), PHASE=c(-1,-1))
    tv_par_dummy <- data.frame(INIT=1,ESTIM=1)
    old_par_devs <- parlist[["parm_devs"]]
    new_par_devs <- list()
    
    #Now loop over each parameter group (i.e. Mortality and Growth params, Stock Recruitment params, Catchability params, and Selectivity params)
    for(s in 1:4){
      #For each group subset out the relevant sections of the control and paramter files for reading and modification
      if(s==1){
        temp_ctl <- ctl[["MG_parms"]]
        temp_tv <- ctl[["MG_parms_tv"]]
        temp_par <- parlist[["MG_parms"]]
      }else if(s==2){
        temp_ctl <- ctl[["SR_parms"]]
        temp_tv <- ctl[["SR_parms_tv"]]
        temp_par <- parlist[["SR_parms"]]
      }else if(s==3){
        temp_ctl <- ctl[["Q_parms"]]
        temp_tv <- ctl[["Q_parms_tv"]]
        temp_par <- parlist[["Q_parms"]]
      }else if(s==4){
        temp_ctl <- rbind(ctl[["size_selex_parms"]],ctl[["age_selex_parms"]],ctl[["dirichlet_parms"]])
        temp_tv <- rbind(ctl[["size_selex_parms_tv"]],ctl[["age_selex_parms_tv"]],ctl[["pars_2D_AR"]])
        temp_par <- parlist[["S_parms"]]
      }
      
      #Now select which elements of this group will be updated
      temp_mods <- which(is.element(names(future_om_dat),row.names(temp_ctl)))
      
      #Find all of the existing blocks that have time varying aspects already so they can be carried over and modified 
      #as needed to interface with the new projected variations
      TV_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("env_var&link", "dev_link", "Block")] != 0),])),2*length(temp_ctl))
      block_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("Block")] != 0),])),2*length(temp_ctl))
      env_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("env_var&link")] != 0),])),2*length(temp_ctl))
      dev_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("dev_link")] != 0),])),2*length(temp_ctl))
      old_tv <- temp_tv
      old_par <- temp_par
      old_par_tv <- old_par[(length(old_par[,1])-length(old_tv[,1])+1):length(old_par[,1]),]
        
      new_par <- old_par[-((length(old_par[,1])-length(old_tv[,1])+1):length(old_par[,1])),]
      new_par_tv <- old_par_tv[0, , drop=FALSE]
      new_tv <- old_tv[0, , drop=FALSE]
      new_par_devs <- list()
      
      #If there are future om time varying effects for this group of parameters enter a loop to implement them
      if(length(temp_mods)>0){  
        #Now loop over each of the future om updates to be applied to this parameter group
        for(i in temp_mods){
          #First identify the location in the group of the particular parameter you will be updating
          current_par <- which(is.element(row.names(temp_ctl),names(future_om_dat)[i]))
          
          #Now transfer over all time varying parameters with a lower index directly to the new updated parameter list
          for(j in TV_existing[TV_existing<current_par]){
            trans_par <- grep(names(temp_ctl)[j],old_par_tv)
            for(k in trans_par){
              new_tv <- rbind(new_tv,old_tv[1,])
              old_tv <- old_tv[-1,,drop=FALSE]
              new_par_tv <- rbind(new_par_tv,old_par_tv[1,])
              old_par_tv <- old_par_tv[-1,,drop=FALSE]
            }
          }
          #Also transfer over any deviation vectors for these parameters 
          for(j in dev_existing[dev_existing<current_par]){
            new_par_devs[[(length(new_par_devs)+1)]]<-old_par_devs[[1]]
            names(new_par_devs)[length(new_par_devs)]<-names(old_par_devs)[1]
            old_par_devs<-old_par_devs[-1]
          }
          #If the current parameter to update with future om devs already has block effects copy these over 
          #to the new parameter list and save them for possible use in converting parameter devs if needed
          if(is.element(current_par,block_existing)){
            block_par <- c(grep(paste0(names(temp_ctl)[current_par],"_BLK"),old_par_tv),grep(paste0(names(temp_ctl)[current_par],"_Trend"),old_par_tv))
            temp_block <- old_par_tv[block_par,]
            new_tv <- rbind(new_tv,old_tv[block_par,])
            old_tv <- old_tv[-block_par,,drop=FALSE]
            new_par_tv <- rbind(new_par_tv,old_par_tv[block_par,])
            old_par_tv <- old_par_tv[-block_par,,drop=FALSE]
          }
          #If the current parameter to update with future om devs already has environmental effects copy these over 
          #to the new parameter list and save them for possible use in converting parameter devs if needed
          if(is.element(current_par,env_existing)){
            env_par <- grep(paste0(names(temp_ctl)[current_par],"_ENV"),old_par_tv)
            temp_env <- old_par_tv[env_par,]
            new_tv <- rbind(new_tv,old_tv[env_par,])
            old_tv <- old_tv[-env_par,,drop=FALSE]
            new_par_tv <- rbind(new_par_tv,old_par_tv[env_par,])
            old_par_tv <- old_par_tv[-env_par,,drop=FALSE]
          }
          #If the current parameter to update with future om devs already has dev effects these may need 
          #to be converted to a format compatible with the new MSE period devs
          if(is.element(current_par,dev_existing)){
            #If the existing devs are not of a standard additive form they will need to be converted 
            if(temp_ctl[current_par,c("dev_link")]!=2 & temp_ctl[current_par,c("dev_link")]!=22){
              #Scale the base deviations by their standard deviation parameter 
              scale_devs <- old_par_devs[[1]][,"dev"]*new_par_tv[(length(new_par_tv[,1])-1),"ESTIM"]
              #Identify the years for base devs
              base_dev_years <- old_par_devs[[1]][,"year"]
              #Extract the base parameter value, bounds, range
              base_val <- new_par[current_par,c("ESTIM")]
              base_vals <- rep(base_val,length(old_par_devs[[1]][,"dev"]))
              base_bounds <- temp_ctl[current_par,c("LO","HI")]
              base_range <- base_bounds[2] - base_bounds[1]
              
              #Conversion of exponential deviations to additive will require calculation of the underlying
              #parameter values which may have been modified by other block or environmental time varying inputs
              if(is.element(temp_ctl[current_par,c("dev_link")], c(1,21))){
                
                #Block effects are implemented first so if applicable update the base_vals sequence based on these block effects
                base_vals <- update_basevals_blocks(base_vals,base_dev_years,temp_block,current_par,ctl,dat,temp_ctl,base_range,baseparm,base_bounds)
                
                #Now update base_vals based on environmental effects if applicable
                base_vals <- update_basevals_env(base_vals,base_dev_years,temp_env,current_par,timeseries,temp_ctl,dat,base_range,base_bounds)
                  
                #Now update base_vals based on existing deviations in their original link function format
                final_vals <- update_basevals_dev(base_vals,
                                                  temp_dev=new_par_tv[(length(new_par_tv[,1])-c(0,1)),"ESTIM"],
                                                  dev_seq=old_par_devs[[1]][,"dev"],
                                                  current_par,temp_ctl,base_range,base_bounds)
                
                #Now calculate the coverted additive devs
                converted_devs <- (final_vals-base_vals)
                
              }else if(is.element(temp_ctl[current_par,c("dev_link")], c(3,23))){
                #Now calculate the coverted additive devs
                converted_devs <- cumsum(scale_devs)
              }else if(is.element(temp_ctl[current_par,c("dev_link")], c(4,6,24,26))){
                #Now calculate the coverted additive devs
                converted_devs <- scale_devs
                for(j in 2:length(scale_devs)){converted_devs[j]<-converted_devs[j]+converted_devs[j-1]*new_par_tv[(length(new_par_tv[,1])),"ESTIM"]}
              }else if(is.element(temp_ctl[current_par,c("dev_link")], c(5,25))){
                #Block effects are implemented first so if applicable update the base_vals sequence based on these block effects
                base_vals <- update_basevals_blocks(base_vals,base_dev_years,temp_block,current_par,ctl,dat,temp_ctl,base_range,baseparm,base_bounds)
                
                #Now if update base_vals based on environmental effects if applicable
                base_vals <- update_basevals_env(base_vals,base_dev_years,temp_env,current_par,timeseries,temp_ctl,dat,base_range,base_bounds)
                
                #Now update base_vals based on existing deviations in their original link function format
                final_vals <- update_basevals_dev(base_vals,
                                                  temp_dev=new_par_tv[(length(new_par_tv[,1])-c(0,1)),"ESTIM"],
                                                  dev_seq=old_par_devs[[1]][,"dev"],
                                                  current_par,temp_ctl,base_range,base_bounds)
                
                #Now calculate the coverted additive devs
                converted_devs <- final_vals-base_vals
              }
              converted_devs <- converted_devs/new_par_tv[(length(new_par_tv[,1])-1),"ESTIM"]
            }else{
              converted_devs <- old_par_devs[[1]]
            }
            #If the existing devs didn't run all the way to the last year of the model then filler zero devs will be 
            #needed for the intervening years
            if(temp_ctl[current_par,c("dev_maxyr")]<dat[["endyr"]]){
              filler_devs <- rep(0,(dat[["endyr"]]-temp_ctl[current_par,c("dev_maxyr")]))
            }else{
              filler_devs <- NULL
            }
            
            #Combine the converted existing devs with any needed filler devs and the new om projection devs and update the 
            #relevant parameter values
            add_devs <- data.frame(year=((min(temp_ctl[current_par,c("dev_maxyr")],dat[["endyr"]])+1):(dat[["endyr"]]+length(future_om_dat[,i]))),dev=c(filler_devs,future_om_dat[,i]))
            new_par_devs[[(length(new_par_devs)+1)]] <- rbind(converted_devs[converted_devs[,"year"]<=dat[["endyr"]]],add_devs)
            names(new_par_devs)[length(new_par_devs)] <- names(old_par_devs)[1]
            old_par_devs <- old_par_devs[-1]
            temp_ctl[current_par,c("dev_link","dev_maxyr","dev_PH")] <- c(2,(dat[["endyr"]]+length(future_om_dat[,i])),-1)
            
          }else{
            temp_ctl[current_par,c("dev_link","dev_minyr","dev_maxyr","dev_PH")] <- c(2,(dat[["endyr"]]+1),(dat[["endyr"]]+length(future_om_dat[,i])),-1)
            new_tv <- rbind(new_tv,tv_dummy)
            new_par_tv <- rbind(new_par_tv,tv_par_dummy)
            new_par_devs[[(length(new_par_devs)+1)]] <- data.frame(year=((dat[["endyr"]]+1):(dat[["endyr"]]+length(future_om_dat[,i]))),dev=c(future_om_dat[,i]))
          }
          
          TV_existing <- TV_existing[TV_existing>current_par]
          dev_existing <- dev_existing[dev_existing>current_par]
        }
      }else if(length(dev_existing)>0){
        for(j in dev_existing){
          new_par_devs[[(length(new_par_devs)+1)]]<-old_par_devs[[1]]
          names(new_par_devs)[length(new_par_devs)]<-names(old_par_devs)[1]
          old_par_devs<-old_par_devs[-1]
        }
      }
      
      new_par <- rbind(new_par,new_par_tv,old_par_tv)
      new_tv <- rbind(new_tv,old_tv)
      temp_tv <- new_tv
      temp_par <- new_par
        
      if(s==1){
        ctl[["MG_parms"]] <- temp_ctl
        ctl[["MG_parms_tv"]] <- temp_tv
        parlist[["MG_parms"]] <- temp_par
      }else if(s==2){
        ctl[["SR_parms"]] <- temp_ctl
        ctl[["SR_parms_tv"]] <- temp_tv
        parlist[["SR_parms"]] <- temp_par
      }else if(s==3){
        ctl[["Q_parms"]] <- temp_ctl
        ctl[["Q_parms_tv"]] <- temp_tv
        parlist[["Q_parms"]] <- temp_par
      }else if(s==4){
        ctl[["size_selex_parms"]] <- temp_ctl[1:length(ctl[["size_selex_parms"]][,1]),]
        temp_ctl <- temp_ctl[-(1:length(ctl[["size_selex_parms"]][,1])),,drop=FALSE]
        ctl[["age_selex_parms"]] <- temp_ctl[1:length(ctl[["age_selex_parms"]][,1]),]
        temp_ctl <- temp_ctl[-(1:length(ctl[["age_selex_parms"]][,1])),,drop=FALSE]
        ctl[["dirichlet_parms"]] <- temp_ctl[1:length(ctl[["dirichlet_parms"]][,1]),]
        temp_ctl <- temp_ctl[-(1:length(ctl[["dirichlet_parms"]][,1])),,drop=FALSE]
        if(length(temp_ctl[,1])>0){stop("Something is wrong, the number of temp selection parameters is not correct. This is likely a code bug not user error")}
          
        ctl[["size_selex_parms_tv"]] <- temp_tv[1:length(ctl[["size_selex_parms_tv"]][,1]),]
        temp_tv <- temp_tv[-(1:length(ctl[["size_selex_parms_tv"]][,1])),,drop=FALSE]
        ctl[["age_selex_parms_tv"]] <- temp_tv[1:length(ctl[["age_selex_parms_tv"]][,1]),]
        temp_tv <- temp_tv[-(1:length(ctl[["age_selex_parms_tv"]][,1])),,drop=FALSE]
        ctl[["pars_2D_AR"]] <- temp_tv[1:length(ctl[["pars_2D_AR"]][,1]),]
        temp_tv <- temp_tv[-(1:length(ctl[["pars_2D_AR"]][,1])),,drop=FALSE]
        if(length(temp_tv[,1])>0){stop("Something is wrong, the number of temp time varying selection parameters is not correct. This is likely a code bug not user error")}
          
        parlist[["S_parms"]] <- temp_par 
      }
      
    }
    if(length(old_par_devs)!=0){stop("Something is wrong, all par devs have not been accounted for but they should have been. This is likely a code bug not user error")}
    new_par_devs <- c(new_par_devs,old_par_devs)
    parlist[["parm_devs"]] <- new_par_devs
  }
  
  output_list<-list()
  output_list[["control"]]<-ctl
  output_list[["data"]]<-dat
  output_list[["parameter"]]<-parlist
  
  return(output_list)
}

#' Update a sequence of base parameter annual values to account for a time varying block effects  
#' 
#' @param base_vals A vector of base parameter values that will be updated to include the impact of a time varying block change
#' @param base_years A vector of years for which the base values are needed
#' @param temp_block The timevarying parameter lines for the block effects on the base parameter
#' @param current_par The index of the current parameter being updated
#' @param clt A control file as read in by r4ss::SS_readctl
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param temp_ctl A subset of the control file representing the parameter section of interest (i.e. MG, SR, Q, or Selectivity)
#' @param base_range the difference between the base parameters max and min bounds
#' @param baseparm The value of the base parameter
#' @param base_bounds The min and max bounds of the base parameter 
#'
#' @author Nathan Vaughan
#' @return A modified parameter value series that incorporates the appropriate time varying block effects.
#' 

update_basevals_blocks <- function(base_vals,base_years,temp_block,current_par,ctl,dat,temp_ctl,base_range,baseparm,base_bounds){
  if(temp_ctl[current_par,c("Block")] > 0){
    n_blocks <- ctl[["blocks_per_pattern"]][temp_ctl[current_par,c("Block")]]
    blocks <- ctl[["Block_Design"]][temp_ctl[current_par,c("Block")],,drop=FALSE]
    for(j in 1:n_blocks){
      if(temp_ctl[current_par,c("Block_Fxn")==0]){
        block_dev <- base_vals[(base_dev_years>=blocks[1,((j-1)*2+1)] & base_dev_years<=blocks[1,((j-1)*2+2)])]*exp(temp_block[j,"ESTIM"])   
      }else if(temp_ctl[current_par,c("Block_Fxn")==1]){
        block_dev <- base_vals[(base_dev_years>=blocks[1,((j-1)*2+1)] & base_dev_years<=blocks[1,((j-1)*2+2)])] + temp_block[j,"ESTIM"]   
      }else if(temp_ctl[current_par,c("Block_Fxn")==2]){
        block_dev <- temp_block[j,"ESTIM"]   
      }else if(temp_ctl[current_par,c("Block_Fxn")==3]){
        block_dev <- base_vals[(base_dev_years>=blocks[1,((j-1)*2+1)] & base_dev_years<=blocks[1,((j-1)*2+2)])] + sum(temp_block[1:j,"ESTIM"])
      }else{
        stop(paste0("The block function ",temp_ctl[current_par,c("Block_Fxn")]," selected for ",names(temp_ctl)[current_par]," is not yet implemented in SSMSE"))
      }
      base_vals[(base_dev_years>=blocks[1,((j-1)*2+1)] & base_dev_years<=blocks[1,((j-1)*2+2)])] <- block_dev
    }
  }else if(temp_ctl[current_par,c("Block")] < 0){
    if(temp_ctl[current_par,c("Block")] == -1){
      endtrend <- log((base_range+0.0000002)/(baseparm-base_bounds[1]+0.0000001)-1.)/(-2.)  # // transform the base parameter
      endtrend <- endtrend + temp_block[1,"ESTIM"]  #   //  add the offset  Note that offset value is in the transform space
      endtrend <- base_bounds[1] + (base_range)/(1. + exp(-2.*endtrend))  # // backtransform
      infl_year <- log(0.5)/(-2.) #   // transform the base parameter
      infl_year <- infl_year + temp_block[2,"ESTIM"]  #    //  add the offset  Note that offset value is in the transform space
      infl_year <- dat[["styr"]]+(dat[["endyr"]]-dat[["styr"]])/(1.+exp(-2.*infl_year)) #// backtransform
    }else if(temp_ctl[current_par,c("Block")] == -2){
      endtrend <- temp_block[1,"ESTIM"]
      infl_year <- temp_block[2,"ESTIM"]
    }else if(temp_ctl[current_par,c("Block")] == -3){
      endtrend <- base_bounds[1] + (base_range)*temp_block[1,"ESTIM"]
      infl_year <- dat[["styr"]]+(dat[["endyr"]]-dat[["styr"]])*temp_block[2,"ESTIM"]
    }else{
      stop(paste0("The block function ",temp_ctl[current_par,c("Block")]," selected for ",names(temp_ctl)[current_par]," is not yet implemented in SSMSE"))
    }
    slope <- temp_block[3,"ESTIM"]
    
    norm_styr <- pnorm((dat[["styr"]] -infl_year)/slope)
    norm_endyr <- pnorm((dat[["endyr"]] -infl_year)/slope)
    temp <- (endtrend-baseparm) / (norm_endyr-norm_styr);  # //  delta in cumulative probability between styr and endyr
    
    base_vals <- base_vals + temp*(pnorm((base_dev_years-infl_year)/slope)-norm_styr)
  }
  return(base_vals)
}


#' Update a sequence of base parameter annual values to account for a time varying environmental effects  
#' 
#' @param base_vals A vector of base parameter values that will be updated to include the impact of a time varying environmental effects
#' @param base_years A vector of years for which the base values are needed
#' @param temp_env The time varying parameter lines for the environmental effects on the base parameter
#' @param current_par The index of the current parameter being updated
#' @param timeseries SS output timeseries data
#' @param temp_ctl A subset of the control file representing the parameter section of interest (i.e. MG, SR, Q, or Selectivity)
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param base_range the difference between the base parameters max and min bounds
#' @param base_bounds The min and max bounds of the base parameter 
#' 
#' @author Nathan Vaughan
#' @return A modified parameter series that incorporates the appropriate time varying environmental effects.
#' 

update_basevals_env <- function(base_vals,base_years,temp_env,current_par,timeseries,temp_ctl,dat,base_range,base_bounds){
  if(temp_ctl[current_par,c("env_var&link")] > 0){
    env_link <- floor((temp_ctl[current_par,c("env_var&link")]/100))
    env_index <- floor(temp_ctl[current_par,c("env_var&link")]-100*env_link)
    env_dat <- dat[["envdat"]][is.element(dat[["envdat"]][,"Yr"],base_dev_years) & dat[["envdat"]][,"Variable"]==env_index,]
    
    if(env_link == 1){
      base_vals <- base_vals*exp(temp_env[1,"ESTIM"]*env_dat[,"Value"])
    }else if(env_link == 2){
      base_vals <- base_vals + temp_env[1,"ESTIM"]*env_dat[,"Value"]
    }else if(env_link == 3){
      temp <- log((base_vals-base_bounds[1]+1.0e-7)/(base_bounds[2]-base_vals+1.0e-7))
      temp <- temp + temp_env[1,"ESTIM"]*env_dat[,"Value"]
      base_vals <- base_bounds[1]+base_range/(1.0+exp(-temp))
    }else if(env_link == 4){
      base_vals <- base_vals*2.00000/(1.00000 + exp(-temp_env[2,"ESTIM"]*(env_dat[,"Value"]-temp_env[1,"ESTIM"])))
    }
  }else if(temp_ctl[current_par,c("env_var&link")] < 0){
    env_link <- floor(abs(temp_ctl[current_par,c("env_var&link")]/100))
    env_index <- floor(abs(temp_ctl[current_par,c("env_var&link")])-100*env_link)
    #These estimations of the model based environmental index are effects are accurate to the best of my 
    #knowledge. It is possible that there are slight discrepencies with how SS calculates the values during the
    #run such as the specific timing during the year. More testing will be needed to see if these work perfectly.
    if(env_index == 1){
      SSB_base <- timeseries[2,"SpawnBio"]
      SSB_current <- timeseries[is.element(timeseries[,"Yr"],base_dev_years),"SpawnBio"]
      env_dat <- log(SSB_current/SSB_base)
    }else if(env_index == 2){
      env_dat <- parlist[["recdev_forecast"]][is.element(parlist[["recdev_forecast"]][,"year"],base_dev_years),"recdev"]
    }else if(env_index == 3){
      smrybio_base <- timeseries[2,"Bio_smry"]
      smrybio_current <- timeseries[is.element(timeseries[,"Yr"],base_dev_years),"Bio_smry"]
      env_dat <- log(smrybio_current/smrybio_base)
    }else if(env_index == 4){
      smrynum_cols <- grep("SmryNum",names(timeseries))
      if(length(smrynum_cols)>1){
        smrynum <- apply(timeseries[,smrynum_cols],MARGIN=1,FUN=sum)
        smrynum_base <- smrynum[2]
        smrynum_current <- smrynum[is.element(timeseries[,"Yr"],base_dev_years)]
      }
      env_dat <- log(smrynum_current/smrynum_base)
    }
    
    if(env_link == 1){
      base_vals <- base_vals*exp(temp_env[1,"ESTIM"]*env_dat)
    }else if(env_link == 2){
      base_vals <- base_vals + temp_env[1,"ESTIM"]*env_dat
    }else if(env_link == 3){
      temp <- log((base_vals-base_bounds[1]+1.0e-7)/(base_bounds[2]-base_vals+1.0e-7))
      temp <- temp + temp_env[1,"ESTIM"]*env_dat
      base_vals <- base_bounds[1]+base_range/(1.0+exp(-temp))
    }else if(env_link == 4){
      base_vals <- base_vals*2.00000/(1.00000 + exp(-temp_env[2,"ESTIM"]*(env_dat-temp_env[1,"ESTIM"])))
    }
  }
  return(base_vals)
}

#' Update a sequence of base parameter annual values to account for a time varying deviation effects  
#' 
#' @param base_vals A vector of base parameter values that will be updated to include the impact of a time varying deviations
#' @param temp_dev The time varying parameter lines for the deviations on the base parameter
#' @param dev_seq A vector of the parameter deviations to be applied to the base values
#' @param current_par The index of the current parameter being updated
#' @param temp_ctl A subset of the control file representing the parameter section of interest (i.e. MG, SR, Q, or Selectivity)
#' @param base_range the difference between the base parameters max and min bounds
#' @param base_bounds The min and max bounds of the base parameter 
#' 
#' @author Nathan Vaughan
#' @return A modified parameter series that incorporates the appropriate deviations.
#' 

update_basevals_dev <- function(base_vals,temp_dev,dev_seq,current_par,temp_ctl,base_range,base_bounds){
  final_vals<-base_vals
  if(temp_ctl[current_par,c("dev_link")]>0){
    #Scale the base deviations by their standard deviation parameter 
    scale_devs <- dev_seq*temp_dev[(length(temp_dev[,1])-1),"ESTIM"]
    
    #Now calculate the final parameter values based on the chosen deviation link function
    if(is.element(temp_ctl[current_par,c("dev_link")], c(1,21))){
      final_vals <- base_vals*exp(scale_devs)
    }else if(is.element(temp_ctl[current_par,c("dev_link")], c(3,23))){
      converted_devs <- cumsum(scale_devs)
      final_vals <- base_vals+converted_devs
    }else if(is.element(temp_ctl[current_par,c("dev_link")], c(4,6,24,26))){
      converted_devs <- scale_devs
      for(j in 2:length(scale_devs)){converted_devs[j]<-converted_devs[j]+converted_devs[j-1]*temp_dev[(length(temp_dev[,1])),"ESTIM"]}
      final_vals <- base_vals+converted_devs
    }else if(is.element(temp_ctl[current_par,c("dev_link")], c(5,25))){
          
      final_vals <- scale_devs
      temp <- log((base_vals[1]-base_bounds[1]+1.0e-7)/(base_bounds[2]-base_vals[1]+1.0e-7))
      final_vals[1] <- base_bounds[1]+base_range/(1.0+exp(-temp-scale_devs[1]))
      for (j in 2:length(scale_devs))
      {
        scale_devs[j] <- temp_dev[(length(temp_dev[,1])),"ESTIM"]*scale_devs[j-1]+scale_devs[j]
        temp <- log((base_vals[j]-base_bounds[1]+1.0e-7)/(base_bounds[2]-base_vals[j]+1.0e-7))
        final_vals[j] <- base_bounds[1]+base_range/(1.0+exp(-temp-scale_devs[j]))
      }
    }
  }
  return(final_vals)
}
  
  
  
  