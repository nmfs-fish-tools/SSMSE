
#' Add in future parameter values
#' 
#' @param clt A control file as read in by r4ss::SS_readctl
#' @param dat A datafile as read in by r4ss::SS_readdat
#' @param parlist A parameter file as read in by r4ss::SS_readpar_3.30
#' @param future_om_dat A data frame with random sample data for future parameter
#' 
add_OM_devs <- function(ctl, dat, parlist, future_om_dat) {

  #First check if their is any data to add if not just pass back the original files with end year updated
  if(!is.null(future_om_dat)){
    
    for(i in grep("rec_devs",names(future_om_dat))){
      late_years <- dat[["endyr"]]-dat[["MainRdevYrLast"]]
      late_devs <- parlist[["recdev_forecast"]][0:late_years,"recdev"]
      parlist[["recdev_forecast"]][,"year"] <- (dat[["endyr"]]-late_years+1):(dat[["endyr"]]+length(future_om_dat[,i]))
      parlist[["recdev_forecast"]][,"recdev"] <- c(late_devs,future_om_dat[,i])
    }
    
    for(i in grep("Env_",names(future_om_dat))){
      temp_env<-data.frame(Yr=(dat[["endyr"]]+1):(dat[["endyr"]]+length(future_om_dat[,i])),
                           Variable=rep(as.numeric(strsplit(names(future_om_dat)[i],"Env_")[[1]][2]),length(future_om_dat[,i])),
                           Value=future_om_dat[,i])
      dat[["envdat"]]<-rbind(dat[["envdat"]],temp_env)  
    }
    
    tv_dummy <- data.frame(LO=c(-10,-10), HI=c(10,10), INIT=c(1,1), PRIOR=c(1,1), PR_SD=c(1,1), PR_type=c(0,0), PHASE=c(-1,-1)))
    tv_par_dummy <- data.frame(INIT=1,ESTIM=1)
    old_par_devs <- parlist[["parm_devs"]]
    new_par_devs <- list()
    
    for(s in 1:4){
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
      
      temp_mods <- which(is.element(names(future_om_dat),row.names(temp_ctl)))
      
      MG_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("env_var&link", "dev_link", "Block")] != 0),])),2*length(temp_ctl))
      MG_dev_existing <- c(sort(which(temp_ctl[any(temp_ctl[, c("dev_link")] != 0),])),2*length(temp_ctl))
      old_tv <- temp_tv
      old_par <- temp_par
      old_par_tv <- old_par[(length(old_par[,1])-length(old_tv[,1])+1):length(old_par[,1]),]
        
      new_par <- old_par[-((length(old_par[,1])-length(old_tv[,1])+1):length(old_par[,1])),]
      new_par_tv <- old_par_tv[0, , drop=FALSE]
      new_tv <- old_tv[0, , drop=FALSE]
      new_par_devs <- list()
      
      if(length(temp_mods)>0){  
        for(i in temp_mods){
          current_par <- which(is.element(row.names(temp_ctl),names(future_om_dat)[i]))
          
          for(j in MG_existing[MG_existing<=current_par]){
            trans_par <- grep(names(temp_ctl)[j],old_par_tv)
            for(k in trans_par){
              new_tv <- rbind(new_tv,old_tv[1,])
              old_tv <- old_tv[-1,,drop=FALSE]
              new_par_tv <- rbind(new_par_tv,old_par_tv[1,])
              old_par_tv <- old_par_tv[-1,,drop=FALSE]
            }
          }
          
          for(j in MG_dev_existing[MG_dev_existing<current_par]){
            new_par_devs[[(length(new_par_devs)+1)]]<-old_par_devs[[1]]
            names(new_par_devs)[length(new_par_devs)]<-names(old_par_devs)[1]
            old_par_devs<-old_par_devs[-1]
          }
          
          if(is.element(current_par,MG_dev_existing)){
            if(temp_ctl[current_par,c("dev_link")]!=2){
              stop("Your OM has an existing parameter with devs that are not additive. Please change historic devs to type 2 or don't vary this parameter in the OM.")
              #TODO: Add code to modify the old devs into a simple additive form. This can be done I'm just skipping over it for now.
            }else{
              if(temp_ctl[current_par,c("dev_maxyr")]<dat[["endyr"]]){
                filler_devs <- rep(0,(dat[["endyr"]]-temp_ctl[current_par,c("dev_maxyr")]))
              }else{
                filler_devs <- NULL
              }
              add_devs <- data.frame(year=((temp_ctl[current_par,c("dev_maxyr")]+1):(dat[["endyr"]]+length(future_om_dat[,i]))),dev=c(filler_devs,future_om_dat[,i]))
              new_par_devs[[(length(new_par_devs)+1)]] <- rbind(old_par_devs[[1]],add_devs)
              names(new_par_devs)[length(new_par_devs)] <- names(old_par_devs)[1]
              old_par_devs <- old_par_devs[-1]
              temp_ctl[current_par,c("dev_maxyr","dev_PH")] <- c((dat[["endyr"]]+length(future_om_dat[,i])),-1)
            }
          }else{
            temp_ctl[current_par,c("dev_link","dev_minyr","dev_maxyr","dev_PH")] <- c(2,(dat[["endyr"]]+1),(dat[["endyr"]]+length(future_om_dat[,i])),-1)
            new_tv <- rbind(new_tv,tv_dummy)
            new_par_tv <- rbind(new_par_tv,tv_par_dummy)
            new_par_devs[[(length(new_par_devs)+1)]] <- data.frame(year=((dat[["endyr"]]+1):(dat[["endyr"]]+length(future_om_dat[,i]))),dev=c(future_om_dat[,i]))
          }
        }
      }else if(length(MG_dev_existing)>0){
        for(j in MG_dev_existing){
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
