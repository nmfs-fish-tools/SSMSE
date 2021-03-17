# Functions to extend the OM beyond the initial years.

#' Extend the OM forward using next years' catch
#'
#' Add in the catch values for the next years, extend the OM forward for the
#' appropriate number of years.
#' @author Kathryn Doering & Nathan Vaughan
#' @param catch A dataframe of catch values and its associated information to
#'  add to the OM. The column names are the same as in an SS data file (e.g.,
#'  year,	season, fleet,	catch,	catch_se).
#' length of the number of years (only works when catch is for 1 fleet)
#' @param discards A dataframe of discard values and associated information to
#'  add to the OM. The column names are as in an SS datafile.
#' @param harvest_rate A dataframe of harvest rate (F) values and associated information to
#'  add to the OM. The column names are as in an SS datafile.
#' @param OM_dir The full path to the OM directory.
#' @param sample_struct The sample structure dataframe.
#' @param future_om_list The changes to make to the future OM list.
#' @param nyrs_extend An integer value of years to extend the model forward. Defaults
#'  to an arbitrary value of 3.
#' # SINGLE_RUN_MODS: param dat_yrs Which years should parameter values be updated for. 
#' @param write_dat Should the datafile be overwritten? Defaults to TRUE.
#' @param rec_devs The recruitment deviations
#' @param impl_error The implementation error
#' @param seed A random initialization seed for SS to allow reproducibility
#' @template verbose
#' @return A new dat list object (format as created by r4ss::SS_readdat) that
#'  has been extended forward  as if read in by r4ss function SS_readdat
#' @importFrom r4ss SS_readdat SS_readstarter SS_writestarter
extend_OM <- function(catch,
                      discards,
                      harvest_rate = NULL,
                      # SINGLE_RUN_MODS: EM_pars = NULL, This will let EM specified pars be passed in directly 
                      # SINGLE_RUN_MODS: Linked_pars = NULL, We may need to update some pars based on linkages such as SSB?? if so may be easier to pass in a reference to this?
                      OM_dir,
                      sample_struct = NULL, # SINGLE_RUN_MODS: delete
                      future_om_list = NULL, # SINGLE_RUN_MODS: delete may be better to have the EM directly pass the appropriate pars if needed and given the single run no other pars need updating
                      nyrs_extend = 3, # SINGLE_RUN_MODS: delete
                      # SINGLE_RUN_MODS: dat_yrs = NULL,
                      write_dat = TRUE,
                      rec_devs = NULL, # SINGLE_RUN_MODS: delete
                      impl_error = NULL, # SINGLE_RUN_MODS: delete
                      verbose = FALSE,
                      seed = NULL) {
  #TODO: implement using the future_om_list in this function.

  # input checks

  check_catch_df(catch)
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
  # read in forecast file
  # SINGLE_RUN_MODS: don't need to read in forecast once we move to directly updating values in the model period
  forelist <- r4ss::SS_readforecast( # SINGLE_RUN_MODS: delete
    file = file.path(OM_dir, "forecast.ss"), # SINGLE_RUN_MODS: delete
    readAll = TRUE, verbose = FALSE # SINGLE_RUN_MODS: delete
  ) # SINGLE_RUN_MODS: delete

  # SINGLE_RUN_MODS: No real need for this check as we are working in Fs that cap at 1.5 better just to give a 
  # warning that the fishery bounded at an F limit. Even better would be to require that users put in an F limit
  # for their fleets.
  if (max(catch[["year"]]) > (dat[["endyr"]] + nyrs_extend)) { # SINGLE_RUN_MODS: delete
    stop( # SINGLE_RUN_MODS: delete
      "The maximum year input for catch is ", max(catch[["year"]]), ", but the ", # SINGLE_RUN_MODS: delete
      " nyrs_extend used in function extend_OM only extends the model to the year ", # SINGLE_RUN_MODS: delete
      (dat[["endyr"]] + nyrs_extend), ". Please either remove years of catch data or ", # SINGLE_RUN_MODS: delete
      "the end year of the model longer." # SINGLE_RUN_MODS: delete
    ) # SINGLE_RUN_MODS: delete
  } # SINGLE_RUN_MODS: delete
  
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
  forelist[["Nforecastyrs"]] <- nyrs_extend # should already have this value, but just in case. # SINGLE_RUN_MODS: delete
  forelist[["InputBasis"]] <- -1 # SINGLE_RUN_MODS: delete

  temp_catch <- catch[, c("year", "seas", "fleet", "catch")]
  temp_catch <- cbind(temp_catch, rep(3, length(temp_catch[, 1]))) 

  temp_catch[, "catch"] <- temp_catch[, "catch"] *
    impl_error[seq_along(NROW(temp_catch[, "catch"]))] # SINGLE_RUN_MODS: will need to update with whatever the new impl_error format becomes
  temp_comb <- temp_catch
  mod_catch <- catch

  # SINGLE_RUN_MODS: The logic of creating a combined catch and F target table may still be useful for implementing search in the model period
  if (!is.null(harvest_rate)) {
    temp_F <- harvest_rate[, c("year", "seas", "fleet", "catch")]
    temp_F <- cbind(temp_F, rep(99, length(temp_F[, 1])))

    temp_comb[temp_catch[["catch"]] == 0, ] <- temp_F[temp_catch[["catch"]] == 0, ]
    mod_catch[mod_catch[["catch"]] == 0 & harvest_rate[["catch"]] != 0, "catch"] <- 0.1
  }
  
  achieved_Catch <- FALSE
  colnames(temp_comb) <- c("year", "seas", "fleet", "catch", "basis")
  catch_intended <- temp_comb

  # SINGLE_RUN_MODS: This adds a reference pointer for what row of the par file Fs the input catch target represents
  # F_ref<-rep(1,length(catch_intended[,1]))
  # for(i in 1:length(F_ref)){
  #   F_ref[i] <- which(parlist$F_rate[,c("year")]==catch_intended[i,c("year")] & 
  #                     parlist$F_rate[,c("seas")]==catch_intended[i,c("seas")] &
  #                     parlist$F_rate[,c("fleet")]==catch_intended[i,c("fleet")]) 
  # }
  
  
  
  # modify par file ----
  # SINGLE_RUN_MODS: This whole recdev section can go I think we will not be moving or changing length of recdevs 
  # but may have to update there values if they are a function of SSB or a user specified EM model  
  old_recs <- parlist[["recdev_forecast"]] # SINGLE_RUN_MODS: delete
  old_recs <- old_recs[old_recs[, "year"] <= dat[["endyr"]], , drop = FALSE] # SINGLE_RUN_MODS: delete

  new_recs <- get_rec_devs_matrix( # SINGLE_RUN_MODS: delete
    yrs = (dat[["endyr"]] + 1):(dat[["endyr"]] + forelist[["Nforecastyrs"]]), # SINGLE_RUN_MODS: delete
    rec_devs = rec_devs # SINGLE_RUN_MODS: delete
  ) # SINGLE_RUN_MODS: delete

  
  parlist[["recdev_forecast"]] <- rbind(old_recs, new_recs) # SINGLE_RUN_MODS: update this to modify the relevant years rec devs if needed i.e. parlist[["recdev_forecast"]][is.element(parlist[["recdev_forecast"]][,"year"],dat_yrs),"recdev"]<-EM_recs[,"recdev"]

  # implementation error should always be 0 in the OM
  parlist[["Fcast_impl_error"]] <- # SINGLE_RUN_MODS: delete \
    get_impl_error_matrix(yrs = (dat[["endyr"]] + 1):(dat[["endyr"]] + forelist[["Nforecastyrs"]])) # SINGLE_RUN_MODS: delete

  # SINGLE_RUN_MODS: add in function to update other parameter devs as required 
  # parlist <- update_pars(parlist,future_om_list,dat_yrs)
  
  update_pars<-function(parlist,future_om_list,dat_yrs){
    #Thinking about the update function here it is maybe easier to cull the future_om_list each time it is passed down to a 
    #scenario or iteration rather than passing down 
    
  }
  
  
  
  
  
  
  # SINGLE_RUN_MODS: also need to update the F's in Par file to good starting guesses??
  # maybe don't need too just let it run with whatever the base values are to start??
  
  # write new par file
  r4ss::SS_writepar_3.30( # SINGLE_RUN_MODS: delete
    parlist = parlist, outfile = file.path(OM_dir, "ss.par"), # SINGLE_RUN_MODS: delete
    overwrite = TRUE, verbose = FALSE # SINGLE_RUN_MODS: delete
  ) # SINGLE_RUN_MODS: delete

  
  Fleet_scale <- catch_intended
  Fleet_scale[, "catch"] <- 1
  Fleet_scale[, "basis"] <- 0
  Fleet_scale <- Fleet_scale[Fleet_scale[, "year"] >= (dat[["endyr"]] + 1) & Fleet_scale[, "year"] <= (dat[["endyr"]] + nyrs_extend), ] # SINGLE_RUN_MODS: Fleet_scale <- Fleet_scale[is.element(Fleet_scale[, "year"], dat_yrs), ]
  search_loops <- 0
  while (achieved_Catch == FALSE) {
    search_loops <- search_loops + 1
    forelist[["ForeCatch"]] <- temp_comb # SINGLE_RUN_MODS: delete
    
    # write out the changed forecast file ---- # SINGLE_RUN_MODS: delete
    r4ss::SS_writeforecast( # SINGLE_RUN_MODS: delete
      mylist = forelist, dir = OM_dir, writeAll = TRUE, # SINGLE_RUN_MODS: delete
      overwrite = TRUE, verbose = FALSE # SINGLE_RUN_MODS: delete
    ) # SINGLE_RUN_MODS: delete

    # SINGLE_RUN_MODS: Add code here to update the par F's with adjusted target F's until the achieve target catch
    # SINGLE_RUN_MODS: r4ss::SS_writepar_3.30(
    # SINGLE_RUN_MODS:   parlist = parlist, outfile = file.path(OM_dir, "ss.par"),
    # SINGLE_RUN_MODS:   overwrite = TRUE, verbose = FALSE
    # SINGLE_RUN_MODS:  )
    
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
    
    # SINGLE_RUN_MODS: 
    # read in parameter file
    # parlist <- r4ss::SS_readpar_3.30(
    #   parfile = file.path(OM_dir, "ss.par"),
    #   datsource = dat, ctlsource = ctl,
    #   verbose = FALSE
    # )
    
    # F_achieved <- parlist[["F_rate"]][F_ref,]
    
    # Extract the achieved F and Catch for the forecast period
    F_list <- get_F( # SINGLE_RUN_MODS: delete
      timeseries = outlist[["timeseries"]],# SINGLE_RUN_MODS: delete
      fleetnames = dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "fleetname"]# SINGLE_RUN_MODS: delete
    )# SINGLE_RUN_MODS: delete

    units_of_catch <- dat[["fleetinfo"]][dat[["fleetinfo"]][["type"]] %in% c(1, 2), "units"]

    names(units_of_catch) <- as.character(which(dat[["fleetinfo"]][["type"]] %in% c(1, 2)))

    ret_catch <- get_retained_catch(
      timeseries = outlist[["timeseries"]],
      units_of_catch = units_of_catch
    )

    F_achieved <- F_list[["F_df"]][F_list[["F_df"]][, "Era"] == "FORE", c("Yr", "Seas", "Fleet", "F")]  # SINGLE_RUN_MODS: delete
    colnames(F_achieved) <- c("year", "seas", "fleet", "F")# SINGLE_RUN_MODS: delete
    # Check that SS created projections with the intended catches before updating model
    # make sure retained catch is close to the same as the input catch.
    for (i in 1:length(Fleet_scale[, "catch"])) {
      ratio <- 1

      intended_val <- catch_intended[catch_intended[, "year"] == Fleet_scale[i, "year"] &
        catch_intended[, "seas"] == Fleet_scale[i, "seas"] &
        catch_intended[, "fleet"] == Fleet_scale[i, "fleet"], "catch"]

      intended_basis <- catch_intended[catch_intended[, "year"] == Fleet_scale[i, "year"] &
        catch_intended[, "seas"] == Fleet_scale[i, "seas"] &
        catch_intended[, "fleet"] == Fleet_scale[i, "fleet"], "basis"]

      achieved_ret <- ret_catch[ret_catch[, "Yr"] == Fleet_scale[i, "year"] &
        ret_catch[, "Seas"] == Fleet_scale[i, "seas"] &
        ret_catch[, "Fleet"] == Fleet_scale[i, "fleet"], "retained_catch"]

      achieved_F <- F_achieved[F_achieved[, "year"] == Fleet_scale[i, "year"] &
        F_achieved[, "seas"] == Fleet_scale[i, "seas"] &
        F_achieved[, "fleet"] == Fleet_scale[i, "fleet"], "F"]
      
      # TODO: Add a dead catch option if we think it is beneficial? not hard to implement I don't think.
      
      if (length(achieved_F) == 0) {
        achieved_F <- 0
      }

      if (length(intended_val) == 1) {
        if (intended_basis == 3) {
          if (intended_val == 0 & achieved_ret == 0) {
            ratio <- 1
          } else if (achieved_ret == 0) {
            if (temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] & # SINGLE_RUN_MODS: if(parlist[["F_rate"]][F_ref[i],"F"] > 0){ stop("This fleet must be 100% discard you will never be able to achieve target catch")
              temp_comb[, "seas"] == Fleet_scale[i, "seas"] & # SINGLE_RUN_MODS: delete
              temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "basis"] == 99) { # SINGLE_RUN_MODS: delete
              stop("For some reason SS refuses to apply catch to this fleet SOMETHING IS VERY WRONG! :(") # SINGLE_RUN_MODS: delete
              # TODO: we could possibly add in capacity for total dead rather than retained to allow biomass based projection of discard fleets?? 
              # catch_intended[catch_intended[, "year"] == Fleet_scale[i, "year"] &
              #                  catch_intended[, "seas"] == Fleet_scale[i, "seas"] &
              #                  catch_intended[, "fleet"] == Fleet_scale[i, "fleet"], "basis"] <- 2
              
            } else {
              # SINGLE_RUN_MODS: parlist[["F_rate"]][F_ref[i],"F"] <- 0.4
              # SINGLE_RUN_MODS: ratio <- 1
              temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] &  # SINGLE_RUN_MODS: delete
                temp_comb[, "seas"] == Fleet_scale[i, "seas"] &  # SINGLE_RUN_MODS: delete
                temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] <- 0.4  # SINGLE_RUN_MODS: delete
              temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] &  # SINGLE_RUN_MODS: delete
                temp_comb[, "seas"] == Fleet_scale[i, "seas"] &  # SINGLE_RUN_MODS: delete
                temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "basis"] <- 99  # SINGLE_RUN_MODS: delete
            }
          } else {
            ratio <- intended_val / achieved_ret
          }
        } else if (intended_basis == 99) {
          if (intended_val == 0 & achieved_ret == 0) {
            ratio <- 1
          } else if (length(achieved_F) == 0) {
            stop("For some reason SS refuses to apply F to this fleet SOMETHING IS VERY WRONG! :(") # SINGLE_RUN_MODS: probably don't need this any more as we are pulling directly from the par Fs
          } else {
            ratio <- intended_val / achieved_F
          }
        } else {# TODO: We could add in dead catch here as a target option
          # replicate the inner loops of intended basis = 3 here
          stop("Catch testing is only set up to compare retained catch or F at the moment")
        }
      } else {
        ratio <- 1
      }
      if (is.na(ratio)) {
        ratio <- 0.5
      }
      if (is.null(ratio)) {
        ratio <- 0.5
      }
      if (is.nan(ratio)) {
        ratio <- 0.5
      }
      ratio <- min(ratio, 2)

      # SINGLE_RUN_MODS: These need to be adjusted to modify the par file F's instead of the forecast file dataframe
      if (ratio < 0) {  # SINGLE_RUN_MODS: We will need to replace this but still probably have a way to limit the upper bound of F (maybe a default of 2 times the historic max?? I think F=1.5 is a default cap for SS)
        # SINGLE_RUN_MODS: The scenario of negative Fs/catch (required to create ratio<0) is likely not possible any more now that we are fitting Fs directly. This only happened when SS tried to implement impossibly 
        # large fixed catches.
        
        # SINGLE_RUN_MODS: parlist[["F_rate"]][F_ref[i],"F"] <- 1.5
        # SINGLE_RUN_MODS: ratio <- 1
        # SINGLE_RUN_MODS: Fleet_scale[i, "catch"] <- ratio
        # SINGLE_RUN_MODS: Fleet_scale[i, "basis"] <- abs(1 - ratio)
        
        temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] &  # SINGLE_RUN_MODS: delete
          temp_comb[, "seas"] == Fleet_scale[i, "seas"] &  # SINGLE_RUN_MODS: delete
          temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] <- 1.5  # SINGLE_RUN_MODS: delete
        temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] &  # SINGLE_RUN_MODS: delete
          temp_comb[, "seas"] == Fleet_scale[i, "seas"] &  # SINGLE_RUN_MODS: delete
          temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "basis"] <- 99  # SINGLE_RUN_MODS: delete

        catch_intended[catch_intended[, "year"] == Fleet_scale[i, "year"] &
          catch_intended[, "seas"] == Fleet_scale[i, "seas"] &
          catch_intended[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] <- 1.5
        catch_intended[catch_intended[, "year"] == Fleet_scale[i, "year"] &
          catch_intended[, "seas"] == Fleet_scale[i, "seas"] &
          catch_intended[, "fleet"] == Fleet_scale[i, "fleet"], "basis"] <- 99
      } else {
        if (ratio < 1) {
          # SINGLE_RUN_MODS: Fleet_scale[i, "catch"] <- ratio
          # SINGLE_RUN_MODS: Fleet_scale[i, "basis"] <- abs(1 - ratio)
          ratio <- (0.5 * (ratio - 1) + 1) # SINGLE_RUN_MODS: replace 0.5 with runif(1,0.5,1) so that the model can step quickly while also avoiding oscillating
          ratio <- (ratio + Fleet_scale[i, "catch"]) / 2 # SINGLE_RUN_MODS: delete I think, I don't think averaging with the previous step was my best idea
          temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "seas"] == Fleet_scale[i, "seas"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] <- temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "seas"] == Fleet_scale[i, "seas"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] * ratio # SINGLE_RUN_MODS: delete
          # SINGLE_RUN_MODS: parlist[["F_rate"]][F_ref[i],"F"] <- parlist[["F_rate"]][F_ref[i],"F"] * ratio
        } else if (achieved_F >= 1.5) {
          # SINGLE_RUN_MODS: parlist[["F_rate"]][F_ref[i],"F"] <- 1.5
          # SINGLE_RUN_MODS: Fleet_scale[i, "catch"] <- ratio
          # SINGLE_RUN_MODS: Fleet_scale[i, "basis"] <- abs(1 - ratio)
          ratio <- 1
        } else {
          # SINGLE_RUN_MODS: Fleet_scale[i, "catch"] <- ratio
          # SINGLE_RUN_MODS: Fleet_scale[i, "basis"] <- abs(1 - ratio)
          ratio <- (0.5 * (ratio - 1) + 1) # SINGLE_RUN_MODS: replace 0.5 with runif(1,.5,1) so that the model can step quickly while also avoiding oscillating 
          ratio <- (ratio + Fleet_scale[i, "catch"]) / 2 # SINGLE_RUN_MODS: delete I think, I don't think averaging with the previous step was my best idea
          temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "seas"] == Fleet_scale[i, "seas"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] <- temp_comb[temp_comb[, "year"] == Fleet_scale[i, "year"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "seas"] == Fleet_scale[i, "seas"] & # SINGLE_RUN_MODS: delete
            temp_comb[, "fleet"] == Fleet_scale[i, "fleet"], "catch"] * ratio # SINGLE_RUN_MODS: delete
        }
      }

      Fleet_scale[i, "catch"] <- ratio # SINGLE_RUN_MODS: delete
      Fleet_scale[i, "basis"] <- abs(1 - ratio) # SINGLE_RUN_MODS: delete
    }

    if (max(Fleet_scale[, "basis"]) > 0.001 & search_loops < 20) {
      achieved_Catch <- FALSE
    } else {
      achieved_Catch <- TRUE
    }
    if (search_loops == 20) {
      utils::write.csv("test to see how long the loop runs",
        file = file.path(OM_dir, "search_took_too_long.csv")
      )
    }
  }
  temp_1 <- catch_intended[, 1:4]
  colnames(temp_1) <- c("year", "seas", "fleet", "catch")
  temp_2 <- ret_catch[ret_catch[, "Yr"] == (dat[["endyr"]] + 1), c(1, 3, 5, 6)]
  colnames(temp_2) <- c("year", "seas", "fleet", "catch")
  temp_3 <- F_achieved[, 1:4]
  colnames(temp_3) <- c("year", "seas", "fleet", "catch")
  result_fit <- rbind(temp_1, temp_2, temp_3)
  utils::write.csv(result_fit,
    file = file.path(
      OM_dir,
      paste0("intended_catch_search_took_", search_loops, "_loops.csv")
    )
  )
  # extend the number of yrs in the model and add in catch ----
  # modify forecast file - do this to make the forecasting elements simpler for
  # the second run of the OM.
  forelist[["Nforecastyrs"]] <- 1  # SINGLE_RUN_MODS: delete 
  forelist[["ForeCatch"]] <- NULL  # SINGLE_RUN_MODS: delete 
  # change parfile
  # recdevs
  # recdev1 is created when using do_rec_devs method 1; recdev2 is created when
  # using do_rec_devs method 2,3,or 4;
  dummy_rec <- get_rec_devs_matrix(  # SINGLE_RUN_MODS: delete 
    yrs = (dat[["endyr"]] + nyrs_extend + 1),  # SINGLE_RUN_MODS: delete 
    rec_devs = 0  # SINGLE_RUN_MODS: delete 
  )  # SINGLE_RUN_MODS: delete 
  parlist[["recdev_forecast"]] <- rbind(parlist[["recdev_forecast"]], dummy_rec)  # SINGLE_RUN_MODS: delete 
  #
  # if (!is.null(parlist[["recdev1"]])) {
  #   parlist[["recdev1"]] <- rbind(parlist[["recdev1"]],
  #                            parlist[["recdev_forecast"]][seq_len(nyrs_extend+length(temp_recs[,1])), ])
  # } else if (!is.null(parlist[["recdev2"]])) {
  #   parlist[["recdev2"]] <- rbind(parlist[["recdev2"]],
  #                            parlist[["recdev_forecast"]][seq_len(nyrs_extend), ])
  # }
  # implementation error
  parlist[["Fcast_impl_error"]] <- get_impl_error_matrix(  # SINGLE_RUN_MODS: delete 
    yrs = (dat[["endyr"]] + nyrs_extend + 1)  # SINGLE_RUN_MODS: delete 
  )  # SINGLE_RUN_MODS: delete 
  # recdevs
  # parlist[["recdev_forecast"]] <-
  #   get_rec_devs_matrix(yrs = (dat[["endyr"]] + nyrs_extend + 1),
  #                       rec_devs = 0)

  # F values
  # SINGLE_RUN_MODS: need to modify this and move it back to the loop to modify a portion of F_rate rather than appending to the end
  add_F_rate <- F_list[["F_rate_fcast"]][  # SINGLE_RUN_MODS: delete
    , # SINGLE_RUN_MODS: delete
    setdiff(colnames(F_list[["F_rate_fcast"]]), "name") # SINGLE_RUN_MODS: delete
  ] # SINGLE_RUN_MODS: delete
  add_F_rate <- add_F_rate[add_F_rate[["year"]] %in% # SINGLE_RUN_MODS: delete
    (dat[["endyr"]] + 1):(dat[["endyr"]] + nyrs_extend), ] # SINGLE_RUN_MODS: delete
  parlist[["F_rate"]] <- rbind(parlist[["F_rate"]], add_F_rate) # SINGLE_RUN_MODS: delete

  parlist[["F_rate"]] <- parlist[["F_rate"]][order(parlist[["F_rate"]][["fleet"]], parlist[["F_rate"]][["year"]], parlist[["F_rate"]][["seas"]]), ] # SINGLE_RUN_MODS: delete
  # change data file

  # SINGLE_RUN_MODS: I don't think this matters because we are not fitting to the catches/discards, we could always just replace the input catch/discards with the expected values if we want for neatness??
  dat[["catch"]] <- rbind(dat[["catch"]], mod_catch)  # SINGLE_RUN_MODS: delete
  if (dat[["N_discard_fleets"]] > 0) { # SINGLE_RUN_MODS: delete
    dat[["discard_data"]] <- rbind(dat[["discard_data"]], discards) # SINGLE_RUN_MODS: delete
  } # SINGLE_RUN_MODS: delete
  
  
  # add in future years data that the EM will need.
    dat <- add_sample_struct(sample_struct = sample_struct, dat = dat, # SINGLE_RUN_MODS: delete 
                      nyrs_extend = nyrs_extend)# SINGLE_RUN_MODS: delete 
  dat[["endyr"]] <- dat[["endyr"]] + nyrs_extend# SINGLE_RUN_MODS: delete 
  # write the new data file
  # SINGLE_RUN_MODS: only need to do this if we are updating the values from the expected
  if (write_dat) {
    r4ss::SS_writedat(dat,
                      outfile = file.path(OM_dir, start[["datfile"]]),
                      overwrite = TRUE,
                      verbose = FALSE
    )
  }
  # modify ctl file ----
  # ctl[["MainRdevYrLast"]] <- dat[["endyr"]]
  # ctl[["last_yr_fullbias_adj"]] <- dat[["endyr"]]
  # ctl[["first_recent_yr_nobias_adj"]] <- dat[["endyr"]]

  # write out the changed files, except for dat.
  r4ss::SS_writectl(
    ctllist = ctl, outfile = file.path(OM_dir, start[["ctlfile"]]),
    overwrite = TRUE, verbose = FALSE
  )
  r4ss::SS_writeforecast(
    mylist = forelist, dir = OM_dir, writeAll = TRUE,
    overwrite = TRUE, verbose = FALSE
  )
  r4ss::SS_writepar_3.30(
    parlist = parlist,
    outfile = file.path(OM_dir, "ss.par"), overwrite = TRUE
  )
  
  # SINGLE_RUN_MODS: run_ss_model(OM_dir, "-maxfn 0 -phase 50 -nohess",
  # SINGLE_RUN_MODS:              verbose = verbose,
  # SINGLE_RUN_MODS:              debug_par_run = TRUE
  # SINGLE_RUN_MODS: )
  
  invisible(dat)
  # maybe should just move the final run of the OM here, since we can no longer
  # isolate this function to prevent it from running SS?
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
