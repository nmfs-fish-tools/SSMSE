## COPIED FROM SSMSE GITHUB ###
# to use as reference

# RatioBiasEM designed to work with Implement_Catch_Bias SSMSE branch (which allows for mismatch between OM and EM catch scales)
# contains BOTH RatioBiasEM AND BiasEM

#' Get the EM catch data frame
#'
#' Get the data frame of catch for the next iterations when using a Stock
#' Synthesis Estimation model from the Report.sso file.
#' @param EM_dir Path to the EM files
#' @param dat A SS datfile read into R using \code{r4ss::SS_readdat()}
#' @author Kathryn Doering
#' @return A data frame of future catch
get_RatioEM_catch_df<-function(EM_dir, dat, dat_yrs, 
                               #EM_dir = EM_out_dir; dat = new_EM_dat
                               changeup=0.2, changedown=0.2,# add inputs, min & max allowable annual percent change in catch
                               c1=0.75,# add inputs, multiplicative constant 
                               option="default", ...)
{
  # B_ratio_denominator
  SSout<-r4ss::SS_output(EM_dir)
  endyr<-dat_yrs[1]-1
  startyr<-SSout$startyr
  
  #depletion basis
  # 1 = X*SSB0. Relative to virgin spawning biomass
  # 2 = X*SSBMSY. Relative to spawning biomass that achieves MSY
  # 3 = X*SBstyr. Relative to model start year spawning biomass. 
  # 4 = X*SBendyr. Relative to spawning biomass in the model end year. 
  # use tens digit (1-9) to invoke multi-year (up to 9 yrs)
  # use 1 as hundreds digit to invoke log(ratio)
  # where X= fraction for depletion denominator. Value for use in the calculation of the ratio for SSBy/(X*SSB0)
  if(SSout$depletion_basis == 3 | SSout$depletion_basis==4 ){simpleError("ERROR -- depletion basis 1 or 2")}
  if(SSout$depletion_basis == 1){ # relative to X*SSB0
    # SSout$depletion_basis # 1
    # SSout$depletion_multiplier # 1
    TargBratio<-SSout$derived_quants["B_MSY/SSB_unfished",]$Value
    CurBratio<-SSout$derived_quants[paste0("Bratio_",endyr),"Value"] * SSout$depletion_multiplier
    ratio<- CurBratio/TargBratio
  }
  
  if(SSout$depletion_basis == 2){ # relative to X*SSBMSY
    TargBratio<-1  #SSout$derived_quants["SSB_MSY",]$Value
    CurBratio<-SSout$derived_quants[paste0("Bratio_",endyr),"Value"] * SSout$depletion_multiplier
    ratio<- CurBratio/TargBratio
  }
  
  
  # sratio<-min(1+changeup, ratio); sratio<-max(1-changedown, sratio)
  
  
  rpt <- readLines(file.path(EM_dir, "Report.sso"))
  start <- grep("TIME_SERIES", rpt)
  start <- start[length(start)] + 1
  end <- grep("SPR_series", rpt, ignore.case = TRUE)
  end <- end[length(end)] - 1
  # sanity checks to catch false assumptions about number of times the
  # expressions occur
  assertive.properties::assert_is_of_length(start, 1)
  assertive.properties::assert_is_of_length(end, 1)
  # make into a data frame.
  future_catch <- rpt[start:end]
  hdr <- strsplit(future_catch[1], split = " ", fixed = TRUE)[[1]]
  hdr <- hdr[-grep("^$", hdr)]
  catch_dat <- strsplit(future_catch[-1], split = " ", fixed = TRUE)
  catch_dat <- lapply(catch_dat, function(x) x[x != ""])
  catch_df <- do.call("rbind", catch_dat)
  colnames(catch_df) <- hdr
  catch_df <- utils::type.convert(as.data.frame(catch_df), as.is = TRUE)
  fcast_catch_df <- catch_df[catch_df[["Era"]] == "FORE", ]
  
  catch_df[catch_df[["Era"]]=="TIME","Yr"] # added for ratio EM
  LYr_catch_df <-catch_df[catch_df[["Yr"]]==endyr,] # Added for RatioEM
  
  
  # get the fleets and the units on catch
  units <- dat[["fleetinfo"]]
  units[["survey_number"]] <- seq_len(nrow(units))
  flt_units <- units[units[["type"]] %in% c(1, 2), c("survey_number", "units")]
  # for multi-area models, need to summarize across areas (note a fleet
  # only operates in 1 area)
  # may also need to consider if the catch multiplier is used.
  # match catch with the units
  unit_key <- data.frame(unit_name = c("B", "N"), units = 1:2)
  flt_units <- merge(flt_units, unit_key, all.x = TRUE, all.y = FALSE)
  # get the se
  se <- SSMSE:::get_input_value(dat[["catch"]],
                                method = "most_common_value", ################### takes most common value -- does that consider multiple fleets? 
                                colname = "catch_se", group = "fleet"
  )
  df_list <- vector(mode = "list", length = nrow(flt_units))
  bio_df_list <- vector(mode = "list", length = nrow(flt_units))
  F_df_list <- vector(mode = "list", length = nrow(flt_units))
  for (fl in seq_len(nrow(flt_units))) {
    # note for multi-area models, there is a row for each area and each fleet.
    # will need to summarize  across areas (because fleets only operate in 1
    # area , so this approach is fine for the quantities of interest)
    
    # find which row to get fleet unit catch from. Right now, assume selected =
    # retained,
    # i.e., no discards.
    tmp_col_lab <- paste0(
      "retain(", flt_units[["unit_name"]][fl], "):_",
      flt_units[["survey_number"]][fl]
    )
    # Get the retained biomass for all fleets regardless of units to allow checking with population Biomass
    tmp_col_lab_bio <- paste0(
      "retain(B):_",
      flt_units[["survey_number"]][fl]
    )
    # Get the fleet apical F's to allow identification of unrealistically high fishing effort which may be a
    # better check for MSE than just single year catch larger than the population.
    tmp_col_lab_F <- paste0(
      "F:_",
      flt_units[["survey_number"]][fl]
    )
    # find the se that matches for the fleet
    tmp_catch_se <- se[se[["fleet"]] == flt_units[["survey_number"]][fl], "catch_se"]
    if (length(tmp_catch_se) == 0) {
      if (all(fcast_catch_df[, tmp_col_lab] == 0)) {
        tmp_catch_se <- 0.1 # assign an arbitrary value, b/c no catch
      } else {
        stop(
          "Problem finding catch se to add to future catch df for fleet ",
          flt_units[["survey_number"]][fl], "."
        )
      }
    } # end if length(tmp_catch_se)==0 
    
    ## ADDED FOR RATIO EM
    LYr_catch<-LYr_catch_df[,tmp_col_lab]
    upb<-LYr_catch*(1+changeup)
    lob<-LYr_catch*(1-changedown)
    New_catch<-LYr_catch * ratio
    
    if(option=="default"){TAC <- ( (New_catch - LYr_catch) * c1 ) + LYr_catch}
    if(option=="damping"){TAC <- LYr_catch + exp(log(New_catch - LYr_catch)*c1)}
    
    TAC= min(TAC, upb); TAC = max(TAC, lob)
    ##
    
    
    df_list[[fl]] <- data.frame(
      area = fcast_catch_df[["Area"]],
      year = fcast_catch_df[["Yr"]],
      seas = fcast_catch_df[["Seas"]],
      fleet = flt_units[["survey_number"]][fl],
      catch = rep(TAC, nrow(fcast_catch_df) ),
      catch_se = tmp_catch_se
    )
    
    ## added
    LYr_catch_bio<-LYr_catch_df[,tmp_col_lab_bio]
    upb_bio<-LYr_catch_bio*(1+changeup)
    lob_bio<-LYr_catch_bio*(1-changedown)
    New_catch_bio<-LYr_catch_bio * ratio
    if(option=="default"){TAC_bio <- ( (New_catch_bio - LYr_catch_bio) * c1 ) + LYr_catch_bio}
    if(option=="damping"){TAC_bio <- LYr_catch_bio + exp(log(New_catch_bio+LYr_catch_bio)*c1)}
    TAC_bio= min(TAC_bio, upb_bio); TAC = max(TAC_bio, lob_bio)
    
    bio_df_list[[fl]] <- data.frame(
      area = fcast_catch_df[["Area"]],
      year = fcast_catch_df[["Yr"]],
      seas = fcast_catch_df[["Seas"]],
      fleet = flt_units[["survey_number"]][fl],
      catch = rep(TAC_bio, nrow(fcast_catch_df)),
      catch_se = tmp_catch_se
    )
    
    F_df_list[[fl]] <- data.frame(
      area = fcast_catch_df[["Area"]],
      year = fcast_catch_df[["Yr"]],
      seas = fcast_catch_df[["Seas"]],
      fleet = flt_units[["survey_number"]][fl],
      catch = fcast_catch_df[, tmp_col_lab_F],
      catch_se = tmp_catch_se
    )
  }
  catch_df <- do.call("rbind", df_list)
  catch_bio_df <- do.call("rbind", bio_df_list)
  catch_F_df <- do.call("rbind", F_df_list)
  
  # sum across area - this is necessary fo a multiarea model
  catch_df <- catch_df %>%
    dplyr::group_by(.data[["year"]], .data[["seas"]], .data[["fleet"]]) %>%
    dplyr::summarise(catch = sum(.data[["catch"]])) %>%
    merge(se, all.x = TRUE, all.y = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[["year"]], .data[["seas"]], .data[["fleet"]], .data[["catch"]], .data[["catch_se"]])
  catch_bio_df <- catch_bio_df %>%
    dplyr::group_by(.data[["year"]], .data[["seas"]], .data[["fleet"]]) %>%
    dplyr::summarise(catch = sum(.data[["catch"]])) %>%
    merge(se, all.x = TRUE, all.y = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[["year"]], .data[["seas"]], .data[["fleet"]], .data[["catch"]], .data[["catch_se"]])
  catch_F_df <- catch_F_df %>%
    dplyr::group_by(.data[["year"]], .data[["seas"]], .data[["fleet"]]) %>%
    dplyr::summarise(catch = sum(.data[["catch"]])) %>%
    merge(se, all.x = TRUE, all.y = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[["year"]], .data[["seas"]], .data[["fleet"]], .data[["catch"]], .data[["catch_se"]])
  
  catch_df <- as.data.frame(catch_df)
  catch_bio_df <- as.data.frame(catch_bio_df)
  catch_F_df <- as.data.frame(catch_F_df)
  
  # get discard, if necessary
  if (dat[["N_discard_fleets"]] > 0) {
    # discard units: 1, biomass/number according to set in catch
    # 2, value are fraction (biomass/numbers ) of total catch discarded
    # 3, values are in numbers(thousands)
    se_dis <- get_input_value(dat[["discard_data"]],
                              method = "most_common_value",
                              colname = "Std_in", group = "Flt"
    )
    dis_df_list <- vector(
      mode = "list",
      length = nrow(dat[["discard_fleet_info"]])
    )
    for (i in seq_along(dat[["discard_fleet_info"]][, "Fleet"])) {
      tmp_flt <- dat[["discard_fleet_info"]][i, "Fleet"]
      # tmp_units_code can be 1, 2, or 3.
      tmp_units_code <- dat[["discard_fleet_info"]][i, "units"]
      # get the discard units
      tmp_discard_units <- ifelse(dat[["fleetinfo"]][tmp_flt, "units"] == 1, "B", "N")
      if (tmp_units_code == 3) tmp_discard_units <- "N"
      tmp_cols <- paste0(
        c("retain(", "sel("), tmp_discard_units, "):_",
        tmp_flt
      )
      tmp_discard_amount <- fcast_catch_df[, tmp_cols[2]] -
        fcast_catch_df[, tmp_cols[1]]
      if (tmp_units_code == 2) { # get discard as a fractional value.
        tmp_discard_amount <- tmp_discard_amount / fcast_catch_df[, tmp_cols[2]]
        tmp_discard_amount[is.na(tmp_discard_amount)] <- 0 # replace any NAs with 0s
      }
      if (sum(tmp_discard_amount) == 0) {
        dis_df_list[[i]] <- NULL
      } else {
        # check that an se was created for that fleet (a sanity check)
        if (length(se_dis[se_dis[["Flt"]] == tmp_flt, "Std_in"]) == 0) {
          stop(
            "A standard error value for fleet ", tmp_flt, "could not be ",
            "determined because there was no discard data for that fleet in ",
            "the data file input to the EM. Please add discarding data for ",
            "the fleet to the OM data file or contact the developers for ",
            "assistance with this problem."
          )
        }
        if(dat$discard_data[dat$discard_data$Flt==tmp_flt,"Seas"][1]<0){ # replace dis_df_list[[i]] - 5/16/2024
          dis_df_list[[i]] <- data.frame(
            Yr = -(abs(fcast_catch_df[["Yr"]])),
            Seas = rep(abs(dat$discard_data[dat$discard_data$Flt==tmp_flt,"Seas"][1]),length(fcast_catch_df[["Yr"]])),
            Flt = abs(tmp_flt),
            Discard = tmp_discard_amount,
            Std_in = se_dis[se_dis[["Flt"]] == tmp_flt, "Std_in"]
          )
        }else{
          dis_df_list[[i]] <- data.frame(
            Yr = fcast_catch_df[["Yr"]],
            Seas = rep(dat$discard_data[dat$discard_data$Flt==tmp_flt,"Seas"][1],length(fcast_catch_df[["Yr"]])),
            Flt = tmp_flt,
            Discard = tmp_discard_amount,
            Std_in = se_dis[se_dis[["Flt"]] == tmp_flt, "Std_in"]
          )
        }# end ifelse
      }
    }
    dis_df <- do.call("rbind", dis_df_list)
  } else {
    dis_df <- NULL
  }
  new_dat_list <- list(
    catch = catch_df,
    discards = dis_df,
    catch_bio = catch_bio_df,
    catch_F = catch_F_df
  )
}

#' Use EM as the management strategy option.
#' @template EM_out_dir
#' @param init_loop Logical. If this is the first initialization loop of the
#'   MSE, \code{init_loop} should be TRUE. If it is in further loops, it should
#'   be FALSE.
#' @param OM_dat An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @template verbose
#' @param nyrs_assess The number of years between assessments. E.g., if an
#'   assessment is conducted every 3 years, put 3 here. A single integer value.
#' @param dat_yrs Which years should be added to the new model? Ignored if
#'  init_loop is TRUE.
#' @template OM_out_dir
#' @template sample_struct
#' @template seed
#' @param ... Any additional parameters

#For example, if EM has a positive bias (e.g., EM catch = 1 when true OM catch = 0.5), the EM2OM multiplier should be less than 1 (EM2OM = 0.5)


RatioBiasEM <- function(EM_out_dir = NULL, init_loop = TRUE, OM_dat, verbose = FALSE,
                   nyrs_assess, dat_yrs, sample_struct = NULL, seed = NULL, OM_out_dir, ...) { 
  SSMSE:::check_dir(EM_out_dir)
  # TODO: change this name to make it less ambiguous
  new_datfile_name <- "init_dat.ss"
  # change the name of data file.
  start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                          verbose = FALSE
  )
  
  if (init_loop) {
    
    # copy over raw data file from the OM to EM folder
    SS_writedat(OM_dat,
                file.path(EM_out_dir, new_datfile_name),
                overwrite = TRUE,
                verbose = FALSE
    )
    orig_datfile_name <- start[["datfile"]] # save the original data file name.
    start[["datfile"]] <- new_datfile_name
    start[["seed"]] <- seed
    SS_writestarter(start, file.path(EM_out_dir),
                    verbose = FALSE,
                    overwrite = TRUE
    )
    ### NOTE:: BY THIS POINT, THE EM DATAFILE HAS BEEN UPDATED TO INCLUDE FORECAST YRS. 
    
    # make sure the data file has the correct formatting (use existing data
    # file in the EM directory to make sure)??
    # TODO: is this necessary, given we have sample structures?
    new_EM_dat <- SSMSE:::change_dat(
      OM_datfile = new_datfile_name,
      EM_datfile = orig_datfile_name,
      EM_dir = EM_out_dir,
      do_checks = TRUE,
      verbose = verbose
    )
    ctl <- SS_readctl(file.path(EM_out_dir, start[["ctlfile"]]),
                      datlist = new_EM_dat
    )
    if (ctl[["EmpiricalWAA"]] == 1) {
      message(
        "EM uses weight at age, so copying over wtatage file from OM.",
        "\nNote wtatage data is not sampled."
      )
      file.copy(
        from = file.path(OM_out_dir, "wtatage.ss"),
        to = file.path(EM_out_dir, "wtatage.ss"),
        overwrite = TRUE
      )
    }
    if (!all(ctl[["time_vary_auto_generation"]] == 1)) {
      warning("Turning off autogeneration of time varying lines in the control file of the EM")
      ctl[["time_vary_auto_generation"]] <- rep(1, times = 5)
      r4ss::SS_writectl(ctl, file.path(EM_out_dir, start[["ctlfile"]]),
                        overwrite = TRUE
      )
    }
    
  } else {
    if (!is.null(sample_struct)) {
      sample_struct_sub <- lapply(sample_struct,
                                  function(df, y) df[df[, 1] %in% y, ],
                                  y = dat_yrs - nyrs_assess
      )
    } else {
      sample_struct_sub <- NULL
    }
    
    new_EM_dat <- add_new_dat_BIAS( ######## NEW FUNCTION TO BUILD IN CONVERSION FACTOR
      OM_dat = OM_dat,
      EM_datfile = new_datfile_name,
      sample_struct = sample_struct_sub,
      EM_dir = EM_out_dir,
      nyrs_assess = nyrs_assess,
      do_checks = TRUE,
      new_datfile_name = new_datfile_name,
      verbose = verbose
    )
    
  } # end else not first iteration
  
  # Update SS random seed
  start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                          verbose = FALSE
  )
  start[["seed"]] <- seed
  SS_writestarter(start, file.path(EM_out_dir),
                  verbose = FALSE,
                  overwrite = TRUE
  )
  # manipulate the forecasting file.
  # make sure enough yrs can be forecasted.
  
  fcast <- SS_readforecast(file.path(EM_out_dir, "forecast.ss"),
                           readAll = TRUE,
                           verbose = FALSE
  )
  # check that it can be used in the EM. fleets shoul
  SSMSE:::check_EM_forecast(fcast,
                            n_flts_catch = length(which(new_EM_dat[["fleetinfo"]][, "type"] %in%
                                                          c(1, 2)))
  )
  fcast <- SSMSE:::change_yrs_fcast(fcast,
                                    make_yrs_rel = (init_loop == TRUE),
                                    nyrs_fore = nyrs_assess,
                                    mod_styr = new_EM_dat[["styr"]],
                                    mod_endyr = new_EM_dat[["endyr"]]
  )
  SS_writeforecast(fcast,
                   dir = EM_out_dir, writeAll = TRUE, overwrite = TRUE,
                   verbose = FALSE
  )
  # given all checks are good, run the EM
  # check convergence (figure out way to error if need convergence)
  # get the future catch using the management strategy used in the SS model.
  run_EM(EM_dir = EM_out_dir, verbose = verbose, check_converged = TRUE)
  # get the forecasted catch.
  new_EM_catch_list <- get_RatioEM_catch_df(EM_dir = EM_out_dir, dat = new_EM_dat, dat_yrs=dat_yrs)
  ## COnSIDER MAKING A get_OM_catch_df if structure of OM =/= EM. 
  # For the simple approach, we can just apply a series of scalars from the EM catch list to create an OM catch list
  new_OM_catch_list = new_EM_catch_list
  
  
  if(!is.null(new_OM_catch_list$catch)){
    tmp_catch <- base::merge(new_OM_catch_list$catch, sample_struct$EM2OMcatch_bias, all.x=TRUE, all.y=FALSE)
    new_OM_catch_list$catch$catch <- new_OM_catch_list$catch$catch * tmp_catch$bias 
  }
  if(!is.null(new_OM_catch_list$catch_bio)){
    tmp_catch_bio <- base::merge(new_OM_catch_list$catch_bio, sample_struct$EM2OMcatch_bias, all.x=TRUE)
    new_OM_catch_list$catch_bio$catch <- new_OM_catch_list$catch_bio$catch * tmp_catch_bio$bias
  }
  # new_OM_catch_list$catch_bio <- NULL
  
  if(!is.null(new_OM_catch_list$discards)){
    tmp_discards <- base::merge(new_OM_catch_list$discards, sample_struct$EM2OMdiscard_bias, all.x=TRUE)
    new_OM_catch_list$discards$Discard <- new_OM_catch_list$discards$Discard * tmp_discards$bias 
  }
  
  if(2 %in% OM_dat$fleetinfo$type){ # if bycatch fleet -- remove from catch list and keep only bycatch fleets in catch_F list
    byc_f <- as.numeric(row.names(OM_dat$fleetinfo[which(OM_dat$fleetinfo$type==2),]))
    new_OM_catch_list$catch<- new_OM_catch_list$catch[new_OM_catch_list$catch$fleet!=byc_f,]
    new_OM_catch_list$catch_bio<- new_OM_catch_list$catch_bio[new_OM_catch_list$catch$fleet!=byc_f,]
    new_OM_catch_list$catch_F<- new_OM_catch_list$catch_F[new_OM_catch_list$catch_F$fleet==byc_f,]
  } else{
    new_OM_catch_list$catch_F <- NULL
  }
  
  
  new_catch_list<-new_OM_catch_list
  new_catch_list
  
}



#### add_new_dat Function Line-By-Line ####
add_new_dat_BIAS<- function (OM_dat, EM_datfile, sample_struct, EM_dir, nyrs_assess, 
                             do_checks = TRUE, new_datfile_name = NULL, verbose = FALSE) 
{
  if (do_checks) {
    if (OM_dat[["type"]] != "Stock_Synthesis_data_file") {
      r4ss_obj_err("OM_dat", "data list")
    }
  }
  EM_dat <- SS_readdat(file.path(EM_dir, EM_datfile), verbose = FALSE)
  new_EM_dat <- EM_dat
  new_EM_dat[["endyr"]] <- new_EM_dat[["endyr"]] + nyrs_assess
  extracted_dat <- mapply(function(df, df_name, OM_dat) {
    OM_df <- OM_dat[[df_name]]
    if (is.integer(OM_df[1, 3]) | is.numeric(OM_df[1, 3])) {
      OM_df[, 3] <- abs(OM_df[, 3])
    } else if (is.character(OM_df[1, 3])) {
      OM_df[, 3] <- as.character(abs(as.integer(OM_df[, 
                                                      3])))
    } # end else if
    by_val <- switch(df_name, catch = c("year", "seas", "fleet"),
                     CPUE = c("year", "seas", "index"),
                     discard_data = c("Yr", "Seas","Flt"), 
                     lencomp = c("Yr", "Seas", "FltSvy", "Gender", "Part"), 
                     agecomp = c("Yr","Seas", "FltSvy", "Gender", "Part", 
                                 "Ageerr", "Lbin_lo", "Lbin_hi"), 
                     meanbodywt = c("Year", "Seas", "Fleet", "Partition", "Type"),
                     MeanSize_at_Age_obs = c("Yr", "Seas", "FltSvy", "Gender", "Part", "AgeErr"))
    new_dat <- merge(df, OM_df, by = by_val, all.x = TRUE, all.y = FALSE)
    if ("catch_se.y" %in% colnames(new_dat)) {
      new_dat[["catch_se.x"]] <- NULL
      colnames(new_dat)[which(colnames(new_dat) == "catch_se.y")] <- "catch_se"
    }
    if ("se_log.y" %in% colnames(new_dat)) {
      new_dat[["se_log.x"]] <- NULL
      colnames(new_dat)[which(colnames(new_dat) == "se_log.y")] <- "se_log"
    }
    if ("Nsamp.y" %in% colnames(new_dat)) {
      new_dat[["Nsamp.x"]] <- NULL
      colnames(new_dat)[which(colnames(new_dat) == "Nsamp.y")] <- "Nsamp"
    }
    if ("Std_in.y" %in% colnames(new_dat)) {
      new_dat[["Std_in.x"]] <- NULL
      colnames(new_dat)[which(colnames(new_dat) == "Std_in.y")] <- "Std_in"
    }
    if ("Ignore.y" %in% colnames(new_dat)) {
      new_dat[["Ignore.y"]] <- NULL
      colnames(new_dat)[which(colnames(new_dat) == "Ignore.x")] <- "Ignore"
    }
    if ("N_" %in% colnames(new_dat)) {
      n_col <- which(colnames(new_dat) == "N_")
      new_dat <- new_dat[, -n_col]
    }
    if (any(is.na(new_dat))) {
      warning("Some values specified in sample_struct (list component ", 
              df_name, ") were not found in OM_dat, so they will not be added to ", 
              "the EM_dat.")
      new_dat <- na.omit(new_dat)
    }
    new_dat
  }, df = sample_struct, df_name = names(sample_struct), MoreArgs = list(OM_dat = OM_dat), 
  SIMPLIFY = FALSE, USE.NAMES = TRUE)
  
  #extracted_dat takes observations from the OM; then below we use the EM2OM multiplier to put catch back into EM units and remove the EM2OMcatch_basis element so that it doesn't get added to the datafile. 
  
  if(!is.null(extracted_dat$catch)){
    tmp_catch<- merge(extracted_dat$catch, sample_struct$EM2OMcatch_bias)
    extracted_dat$catch$catch<- tmp_catch$catch / tmp_catch$bias ### EM2OM edits to get EM catch back to OM
  }
  
  if(!is.null(extracted_dat$discard_data)){
    tmp_discard<- merge(extracted_dat$discard_data, sample_struct$EM2OMdiscard_bias)
    extracted_dat$discard_data$Discard<- tmp_discard$Discard / tmp_discard$bias
  }
  
  extracted_dat[["EM2OMcatch_bias"]]<-NULL
  
  for (n in names(extracted_dat)) {
    new_EM_dat[[n]] <- rbind(new_EM_dat[[n]], extracted_dat[[n]])
  }
  if (!is.null(new_datfile_name)) {
    SS_writedat(new_EM_dat, file.path(EM_dir, new_datfile_name), 
                overwrite = TRUE, verbose = FALSE)
  }
  new_EM_dat
}
# assignInNamespace("add_new_dat_BIAS", add_new_dat_BIAS, "SSMSE") ## ERROR





## COPIED FROM SSMSE GITHUB ###
# to use as reference

# contains the specific MS functions available in the SSMSE package

#' Use EM as the management strategy option.
#' @template EM_out_dir
#' @param init_loop Logical. If this is the first initialization loop of the
#'   MSE, \code{init_loop} should be TRUE. If it is in further loops, it should
#'   be FALSE.
#' @param OM_dat An valid SS data file read in using r4ss. In particular,
#'   this should be sampled data.
#' @template verbose
#' @param nyrs_assess The number of years between assessments. E.g., if an
#'   assessment is conducted every 3 years, put 3 here. A single integer value.
#' @param dat_yrs Which years should be added to the new model? Ignored if
#'  init_loop is TRUE.
#' @template OM_out_dir
#' @template sample_struct
#' @template seed
#' @param ... Any additional parameters

#For example, if EM has a positive bias (e.g., EM catch = 1 when true OM catch = 0.5), the EM2OM multiplier should be less than 1 (EM2OM = 0.5)


BiasEM <- function(EM_out_dir = NULL, init_loop = TRUE, OM_dat, verbose = FALSE,
                   nyrs_assess, dat_yrs, sample_struct = NULL, seed = NULL, OM_out_dir, ...) { 
  SSMSE:::check_dir(EM_out_dir)
  # TODO: change this name to make it less ambiguous
  new_datfile_name <- "init_dat.ss"
  # change the name of data file.
  start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                          verbose = FALSE
  )
  
  if (init_loop) {
    
    # copy over raw data file from the OM to EM folder
    SS_writedat(OM_dat,
                file.path(EM_out_dir, new_datfile_name),
                overwrite = TRUE,
                verbose = FALSE
    )
    orig_datfile_name <- start[["datfile"]] # save the original data file name.
    start[["datfile"]] <- new_datfile_name
    start[["seed"]] <- seed
    SS_writestarter(start, file.path(EM_out_dir),
                    verbose = FALSE,
                    overwrite = TRUE
    )
    # make sure the data file has the correct formatting (use existing data
    # file in the EM directory to make sure)??
    # TODO: is this necessary, given we have sample structures?
    new_EM_dat <- SSMSE:::change_dat(
      OM_datfile = new_datfile_name,
      EM_datfile = orig_datfile_name,
      EM_dir = EM_out_dir,
      do_checks = TRUE,
      verbose = verbose
    )
    ctl <- SS_readctl(file.path(EM_out_dir, start[["ctlfile"]]),
                      datlist = new_EM_dat
    )
    if (ctl[["EmpiricalWAA"]] == 1) {
      message(
        "EM uses weight at age, so copying over wtatage file from OM.",
        "\nNote wtatage data is not sampled."
      )
      file.copy(
        from = file.path(OM_out_dir, "wtatage.ss"),
        to = file.path(EM_out_dir, "wtatage.ss"),
        overwrite = TRUE
      )
    }
    if (!all(ctl[["time_vary_auto_generation"]] == 1)) {
      warning("Turning off autogeneration of time varying lines in the control file of the EM")
      ctl[["time_vary_auto_generation"]] <- rep(1, times = 5)
      r4ss::SS_writectl(ctl, file.path(EM_out_dir, start[["ctlfile"]]),
                        overwrite = TRUE
      )
    }
    
    
  } else {
    
    if (!is.null(sample_struct)) {
      sample_struct_sub <- lapply(sample_struct,
                                  function(df, y) df[df[, 1] %in% y, ],
                                  y = dat_yrs - nyrs_assess
      )
    } else {
      sample_struct_sub <- NULL
    }
    
    new_EM_dat <- add_new_dat_BIAS( ######## NEW FUNCTION TO BUILD IN CONVERSION FACTOR
      OM_dat = OM_dat,
      EM_datfile = new_datfile_name,
      sample_struct = sample_struct_sub,
      EM_dir = EM_out_dir,
      nyrs_assess = nyrs_assess,
      do_checks = TRUE,
      new_datfile_name = new_datfile_name,
      verbose = verbose
    )
    
  } # end else not first iteration
  
  # Update SS random seed
  start <- SS_readstarter(file.path(EM_out_dir, "starter.ss"),
                          verbose = FALSE
  )
  start[["seed"]] <- seed
  SS_writestarter(start, file.path(EM_out_dir),
                  verbose = FALSE,
                  overwrite = TRUE
  )
  # manipulate the forecasting file.
  # make sure enough yrs can be forecasted.
  
  fcast <- SS_readforecast(file.path(EM_out_dir, "forecast.ss"),
                           readAll = TRUE,
                           verbose = FALSE
  )
  # check that it can be used in the EM. fleets shoul
  SSMSE:::check_EM_forecast(fcast,
                            n_flts_catch = length(which(new_EM_dat[["fleetinfo"]][, "type"] %in%
                                                          c(1, 2)))
  )
  fcast <- SSMSE:::change_yrs_fcast(fcast,
                                    make_yrs_rel = (init_loop == TRUE),
                                    nyrs_fore = nyrs_assess,
                                    mod_styr = new_EM_dat[["styr"]],
                                    mod_endyr = new_EM_dat[["endyr"]]
  )
  SS_writeforecast(fcast,
                   dir = EM_out_dir, writeAll = TRUE, overwrite = TRUE,
                   verbose = FALSE
  )
  # given all checks are good, run the EM
  # check convergence (figure out way to error if need convergence)
  # get the future catch using the management strategy used in the SS model.
  run_EM(EM_dir = EM_out_dir, verbose = verbose, check_converged = TRUE)
  
  # get the forecasted catch.
  new_EM_catch_list <- get_EM_catch_df(EM_dir = EM_out_dir, dat = new_EM_dat) 
  ## COnSIDER MAKING A get_OM_catch_df if structure of OM =/= EM. 
  # For the simple approach, we can just apply a series of scalars from the EM catch list to create an OM catch list
  new_OM_catch_list = new_EM_catch_list
  
  
  if(!is.null(new_OM_catch_list$catch)){
    tmp_catch <- base::merge(new_OM_catch_list$catch, sample_struct$EM2OMcatch_bias, all.x=TRUE, all.y=FALSE)
    new_OM_catch_list$catch$catch <- new_OM_catch_list$catch$catch * tmp_catch$bias 
  }
  if(!is.null(new_OM_catch_list$catch_bio)){
    tmp_catch_bio <- base::merge(new_OM_catch_list$catch_bio, sample_struct$EM2OMcatch_bias, all.x=TRUE)
    new_OM_catch_list$catch_bio$catch <- new_OM_catch_list$catch_bio$catch * tmp_catch_bio$bias
  }
  # new_OM_catch_list$catch_bio <- NULL
  
  if(!is.null(new_OM_catch_list$discards)){
    tmp_discards <- base::merge(new_OM_catch_list$discards, sample_struct$EM2OMdiscard_bias, all.x=TRUE)
    new_OM_catch_list$discards$Discard <- new_OM_catch_list$discards$Discard * tmp_discards$bias 
  }
  
  if(2 %in% OM_dat$fleetinfo$type){ # if bycatch fleet -- remove from catch list and keep only bycatch fleets in catch_F list
    byc_f <- as.numeric(row.names(OM_dat$fleetinfo[which(OM_dat$fleetinfo$type==2),]))
    new_OM_catch_list$catch<- new_OM_catch_list$catch[new_OM_catch_list$catch$fleet!=byc_f,]
    new_OM_catch_list$catch_bio<- new_OM_catch_list$catch_bio[new_OM_catch_list$catch$fleet!=byc_f,]
    new_OM_catch_list$catch_F<- new_OM_catch_list$catch_F[new_OM_catch_list$catch_F$fleet==byc_f,]
  } else{
    new_OM_catch_list$catch_F <- NULL
  }
  
  new_catch_list<-new_OM_catch_list
  new_catch_list
}



#' Create the sample_struct list for biased catch and discard data
#'
#' Create a sampling structure list using the pattern in a data file and a year
#' range. NAs are added if no pattern is found (and rm_NAs = FALSE). The types
#' of structure that are added to this list (given their presence in the dat file)
#' with their names as called in the list object in parentheses are:
#'  catch (catch), relative indices (CPUE), length composition (lencomp),
#' age composition (agecomp), mean body weight (meanbodywt), and mean size at
#' age (MeanSize_at_Age_obs). Details for creating the sample structure list are
#' available in the [sampling options section of the SSMSE user manual](https://nmfs-fish-tools.github.io/SSMSE/manual/SSMSE.html#sampling-options).
#'
#' @param dat An r4ss list object read in using r4ss::SS_readdat() or the path
#'  (relative or absolute) to an SS data file to read in.
#' @param nyrs Number of years beyond the years included in the dat file to run
#'  the MSE. A single integer value.
#' @param rm_NAs Should all NAs be removed from dataframes? Defaults to FALSE.
#' @export
#' @return A sample_struct list object, where each list element is a dataframe
#'   containing sampling values. If there were no data for the type, NA is
#'   returned for the element.
#' @author Kathryn Doering
#' @examples
#' OM_path <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
#' # note there is a warning for lencomp because it does not have a consistent pattern
#' sample_struct <- create_sample_struct(OM_path, nyrs = 20)
#' print(sample_struct)
#' 
create_sample_struct_biased <- function(dat, nyrs, rm_NAs = FALSE) { ### edited to include EM2OMcatch_bias
  assertive.types::assert_is_a_number(nyrs)
  if (length(dat) == 1 & is.character(dat)) {
    dat <- SS_readdat(dat, verbose = FALSE)
  }
  
  list_name <- c(
    "catch", "EM2OMcatch_bias", "CPUE", "discard_data", "EM2OMdiscard_bias",
    "lencomp", "agecomp", "meanbodywt", "MeanSize_at_Age_obs"
  )
  sample_struct <- lapply(list_name,
                          function(name, dat) {
                            df <- dat[[name]]
                            if (is.null(df)) {
                              return(NA)
                            }
                            # get year, seas, fleet combo, ignoring -999 values.
                            yr_col <- grep("year|yr|Yr", colnames(df), ignore.case = TRUE, value = TRUE)
                            seas_col <- grep("seas|season|Seas", colnames(df), ignore.case = TRUE, value = TRUE)
                            flt_col <- grep("FltSvy|fleet|index|Flt", colnames(df),
                                            ignore.case = TRUE,
                                            value = TRUE
                            )
                            input_SE_col <- grep("_se|se_|Std_in", colnames(df),
                                                 ignore.case = TRUE,
                                                 value = TRUE
                            ) # catch sample size
                            Nsamp_col <- grep("Nsamp", colnames(df),
                                              ignore.case = TRUE,
                                              value = TRUE
                            ) # input sample size
                            # sanity checks. should match with 1 (or 0 in some cases) cols. Failing these
                            # checks indicate a bug in the code (invalid assuptions of how to match the
                            # cols.)
                            assertive.properties::assert_is_of_length(yr_col, 1)
                            assertive.properties::assert_is_of_length(seas_col, 1)
                            assertive.properties::assert_is_of_length(flt_col, 1)
                            # b/c only Nsamp or SE should exist for a df
                            assertive.base::assert_is_identical_to_true(
                              (length(input_SE_col) == 0 & length(Nsamp_col) == 1) |
                                (length(input_SE_col) == 1 & length(Nsamp_col) == 0) |
                                (length(input_SE_col) == 0 & length(Nsamp_col) == 0)
                            )
                            # remove equilibrium catch
                            df <- df[df[[yr_col]] != -999, ]
                            # find combinations of season and fleet in the df.
                            df_combo <- unique(df[, c(seas_col, flt_col), drop = FALSE])
                            fill_vec <- vector(mode = "list", length = nrow(df_combo))
                            for (i in seq_len(nrow(df_combo))) {
                              tmp_seas <- df_combo[i, seas_col]
                              tmp_flt <- df_combo[i, flt_col]
                              tmp_yrs <- df[df[[seas_col]] == tmp_seas &
                                              df[[flt_col]] == tmp_flt, yr_col]
                              tmp_yrs <- as.numeric(unique(tmp_yrs))
                              tmp_yrs <- tmp_yrs[order(tmp_yrs)]
                              
                              
                              # figure out diff between first and second yr.
                              tmp_diff <- tmp_yrs[2] - tmp_yrs[1]
                              # reconstruct the pattern
                              pat <- seq(tmp_yrs[1], by = tmp_diff, length.out = length(tmp_yrs))
                              if (all(!is.na(pat)) && all(pat == tmp_yrs)) { # a pattern was found
                                future_pat <- seq(pat[length(pat)], dat[["endyr"]] + nyrs, by = tmp_diff)
                                future_pat <- future_pat[future_pat > dat[["endyr"]]]
                                if (length(future_pat) > 0) {
                                  future_pat <- data.frame(
                                    Yr = future_pat,
                                    Seas = tmp_seas,
                                    FltSvy = tmp_flt,
                                    stringsAsFactors = FALSE
                                  )
                                } else {
                                  message(
                                    "Pattern found for ", name, ": FltSvy ", tmp_flt,
                                    ", Seas ", tmp_seas, ", but no data to add for the ",
                                    "timeframe specified. Returning NA for Yr in this ",
                                    "dataframe."
                                  )
                                  future_pat <- data.frame(
                                    Yr = NA,
                                    Seas = tmp_seas,
                                    FltSvy = tmp_flt,
                                    stringsAsFactors = FALSE
                                  )
                                }
                              } else {
                                # the pattern was not found
                                warning(
                                  "Pattern not found for ", name, ": FltSvy ", tmp_flt,
                                  ", Seas ", tmp_seas, ". Returning NA for Yr in this dataframe."
                                )
                                future_pat <- data.frame(
                                  Yr = NA,
                                  Seas = tmp_seas,
                                  FltSvy = tmp_flt,
                                  stringsAsFactors = FALSE
                                )
                              }
                              if (name %in% c("lencomp", "agecomp", "MeanSize_at_Age_obs")) {
                                # Sex
                                sex_col <- grep("Sex|Gender", colnames(df),
                                                ignore.case = TRUE,
                                                value = TRUE
                                )
                                tmp_sex <- unique(df[df[[seas_col]] == tmp_seas &
                                                       df[[flt_col]] == tmp_flt, sex_col])
                                if (length(tmp_sex) == 1) {
                                  future_pat[["Sex"]] <- tmp_sex
                                } else {
                                  future_pat[["Sex"]] <- NA
                                }
                              }
                              if (name %in% c("lencomp", "agecomp", "meanbodywt", "MeanSize_at_Age_obs")) {
                                # partition
                                part_col <- grep("part", colnames(df),
                                                 ignore.case = TRUE,
                                                 value = TRUE
                                )
                                tmp_part <- unique(df[df[[seas_col]] == tmp_seas &
                                                        df[[flt_col]] == tmp_flt, part_col])
                                if (length(tmp_part) == 1) {
                                  future_pat[["Part"]] <- tmp_part
                                } else {
                                  future_pat[["Part"]] <- NA
                                }
                              }
                              if (name %in% c("agecomp", "MeanSize_at_Age_obs")) {
                                # Ageerr
                                ageerr_col <- grep("ageerr", colnames(df),
                                                   ignore.case = TRUE,
                                                   value = TRUE
                                )
                                tmp_err <- unique(df[df[[seas_col]] == tmp_seas &
                                                       df[[flt_col]] == tmp_flt, ageerr_col])
                                
                                if (length(tmp_err) == 1) {
                                  future_pat[["Ageerr"]] <- tmp_err
                                } else {
                                  future_pat[["Ageerr"]] <- NA
                                }
                              }
                              if (name == "agecomp") {
                                # Lbin_lo (expect should be -1)
                                tmp_lo <- unique(df[df[[seas_col]] == tmp_seas &
                                                      df[[flt_col]] == tmp_flt, "Lbin_lo"])
                                if (length(tmp_lo) == 1) {
                                  future_pat[["Lbin_lo"]] <- tmp_lo
                                } else {
                                  future_pat[["Lbin_lo"]] <- NA
                                }
                                # Lbin_hi (expect should be -1)
                                tmp_hi <- unique(df[df[[seas_col]] == tmp_seas &
                                                      df[[flt_col]] == tmp_flt, "Lbin_hi"])
                                if (length(tmp_hi) == 1) {
                                  future_pat[["Lbin_hi"]] <- tmp_hi
                                } else {
                                  future_pat[["Lbin_hi"]] <- NA
                                }
                              }
                              if (name == "meanbodywt") {
                                tmp_type <- unique(df[df[[seas_col]] == tmp_seas &
                                                        df[[flt_col]] == tmp_flt, "Type"])
                                if (length(tmp_type) == 1) {
                                  future_pat[["Type"]] <- tmp_type
                                } else {
                                  future_pat[["Type"]] <- NA
                                }
                              }
                              # add sample size, if possible
                              # see if se or Nsamp is the same across years for the seas/flt. If so,
                              # add to the df. If not, add NA's.
                              if (length(input_SE_col) == 1) {
                                tmp_SE <- unique(df[df[[seas_col]] == tmp_seas &
                                                      df[[flt_col]] == tmp_flt &
                                                      df[[yr_col]] != -999, input_SE_col])
                                if (length(tmp_SE) == 1) {
                                  future_pat[["SE"]] <- tmp_SE
                                } else {
                                  future_pat[["SE"]] <- NA
                                  warning("NA included in column SE for ", name, ".")
                                }
                              }
                              if (length(Nsamp_col) == 1) {
                                tmp_Nsamp <- unique(df[df[[seas_col]] == tmp_seas &
                                                         df[[flt_col]] == tmp_flt &
                                                         df[[yr_col]] != -999, Nsamp_col])
                                if (length(tmp_Nsamp) == 1) {
                                  future_pat[["Nsamp"]] <- tmp_Nsamp
                                } else {
                                  future_pat[["Nsamp"]] <- NA
                                  warning("NA included in column Nsamp for ", name, ".")
                                }
                              }
                              if (name == "MeanSize_at_Age_obs") {
                                # Ageerr
                                n_col <- grep("N_", colnames(df),
                                              ignore.case = FALSE,
                                              value = TRUE
                                )
                                tmp_n <- unique(df[df[[seas_col]] == tmp_seas &
                                                     df[[flt_col]] == tmp_flt, n_col])
                                tmp_n <- unlist(tmp_n, use.names = FALSE)
                                tmp_n <- unique(as.numeric(tmp_n))
                                if (length(tmp_n) == 1) {
                                  future_pat[["N_"]] <- tmp_n
                                } else {
                                  future_pat[["N_"]] <- NA
                                }
                              }
                              fill_vec[[i]] <- future_pat
                            }
                            future_pat_all <- do.call("rbind", fill_vec)
                          },
                          dat = dat
  )
  sample_struct <- lapply(
    sample_struct,
    function(x) utils::type.convert(x, as.is = TRUE)
  )
  if (rm_NAs == TRUE) {
    sample_struct <- lapply(
      sample_struct,
      function(x) {
        x <- na.omit(x)
        if (!is.data.frame(x)) {
          x <- NA
        }
        x
      }
    )
  }
  names(sample_struct) <- list_name
  
  ## ADD EM2OMcatch_bias
  sample_struct$EM2OMcatch_bias<- sample_struct$catch
  names(sample_struct$EM2OMcatch_bias)[4] = "bias"
  sample_struct$EM2OMcatch_bias$bias= rep(1, length=nrow(sample_struct$catch))
  
  ## ADD EM2OMdiscard_bias
  if(!is.null(ncol(sample_struct$discard_data))){
    sample_struct$EM2OMdiscard_bias<- sample_struct$discard_data
    names(sample_struct$EM2OMdiscard_bias)[4] = "bias"
    sample_struct$EM2OMdiscard_bias$bias= rep(1, length=nrow(sample_struct$discard_data))
  } else{
    sample_struct$EM2OMdiscard_bias<-NA
  }
  
  
  sample_struct
}

