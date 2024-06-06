#' Create the devs dataframe for a scenario and iteration from user input
#'
#' This function parses user inputs to convert it into a dataframe of deviations.
#'
#' @template future_om_list
#' @param scen_name The scenario name
#' @param niter The iteration number
#' @param om_mod_path Path to the OM files. Used to reference parameter names.
#' @param nyrs The total number of years that the model will be extended forward.
#' @param global_seed A global seed to set, then pull new seeds from. Defaults
#'   to 123.
#' @author Kathryn Doering
#' @return A list including 3 dataframes and one list: devs_df, the additive deviations
#'  relative to the base values; base_df, the base values of the parameter with
#'  deviations; abs_df, the absolute future values by year (first col) and
#'  parameter (parameters in different cols). Also includes a modified version
#'  of the future_om_list which includes the seed applied to each list component
#'  (note that this is not the ultimate seed used for sampling, as additional)
#'  seeds are generated from this seed based on the scenario, iteration, and
#'  option for randomness (replicate across scenarios or randomize across scenarios).
#'  Note that no OM files are modified or created as part of this function
#'  (i.e., it does not have side effects).
convert_future_om_list_to_devs_df <- function(future_om_list, scen_name,
                                              niter, om_mod_path, nyrs,
                                              global_seed = 123) {
  if (isTRUE(is.null(future_om_list))) { # in case future_om_list is NULL to start with.
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

  # add seeds (alternative to this would be adding the seeds to this list earlier)
  future_om_list <- mapply(function(fut_list, seed) {
    fut_list[["seed"]] <- seed
    fut_list
  }, fut_list = future_om_list, seed = seeds_to_use, SIMPLIFY = FALSE)

  # get rid of the list elements that don't apply to this scenario
  future_om_list <- lapply(future_om_list, function(fut_list, scen) {
    scenarios <- fut_list[["scen"]][-1]
    if (scen %in% scenarios) {
      return(fut_list)
    } else {
      return(NULL)
    }
  }, scen = scen_name)
  if (isTRUE(is.null(future_om_list))) { # in case no changes apply to future_om_list
    dev_vals_df <- NULL
    return(dev_vals_df)
  }
  # construct a dataframe of values (not devs yet). Initialize it using the base
  # params in the model.

  pars_to_change <- unique(unlist(lapply(future_om_list, function(x) x[["pars"]])))

  start <- r4ss::SS_readstarter(
    file = file.path(om_mod_path, "starter.ss"),
    verbose = FALSE
  )
  dat <- r4ss::SS_readdat(file.path(om_mod_path, start[["datfile"]]),
    verbose = FALSE
  )
  ctl <- r4ss::SS_readctl(file.path(om_mod_path, start[["ctlfile"]]),
    datlist = dat
  )
  par <- r4ss::SS_readpar_3.30(
    parfile = file.path(om_mod_path, "ss.par"),
    datsource = file.path(om_mod_path, start[["datfile"]]),
    ctlsource = file.path(om_mod_path, start[["ctlfile"]]),
    verbose = FALSE
  )
  where_par <- match_parname(list_pars = pars_to_change, parlist = par)
  vals_df <- data.frame(yrs = seq_len(nyrs) + dat[["endyr"]])
  tmp_vals <- stats::setNames(
    data.frame(
      matrix(NA, nrow = nrow(vals_df), ncol = length(where_par[["pars"]]))
    ),
    where_par[["pars"]]
  )
  vals_df <- cbind(vals_df, tmp_vals)
  # Get the base values for these parameters.
  for (p in where_par[["pars"]]) {
    vals_df[[p]] <- where_par[where_par[["pars"]] == p, "est"]
  }
  base_vals <- vals_df
  # now, add changes on top of this. Note this is the TOTAL value.
  for (i in seq_len(length(future_om_list))) {
    vals_df <- add_dev_changes(
      fut_list = future_om_list[[i]], scen = scen_name, iter = niter,
      parlist = par, dat = dat, vals_df = vals_df, nyrs = nyrs, ctl = ctl
    )
  }
  # remove base values from pars? I think soo...
  vals_df
  dev_vals_df <- vals_df - base_vals
  if (!all(dev_vals_df[["yrs"]] == 0)) {
    stop(
      "Incorrect assumption made regarding dataframe addition, please open ",
      "an issue in the SSMSE repository."
    )
  }
  # correct the years
  dev_vals_df[["yrs"]] <- vals_df[["yrs"]]

  # return the base vals, deviations, and absolute vals, also the future_om_list values
  return_list <- list(
    base_vals = base_vals,
    dev_vals = dev_vals_df,
    abs_vals = vals_df,
    future_om_list = future_om_list
  )
  return_list
}

#' Match parameter name to parameter names in the par file
#'
#' @param list_pars the parameter names to find
#' @template parlist
#' @author Kathryn Doering
#' @return A dataframe containing the parameter name and which object it is in
#'  in the par object.
match_parname <- function(list_pars, parlist) {
  # make table of values
  par_name_tbl <- data.frame(
    pars = rownames(parlist[["MG_parms"]]),
    obj = "MG_parms"
  )
  par_name_tbl <- rbind(
    par_name_tbl,
    data.frame(
      pars = rownames(parlist[["SR_parms"]]),
      obj = "SR_parms"
    )
  )
  par_name_tbl <- rbind(
    par_name_tbl,
    data.frame(
      pars = rownames(parlist[["Q_parms"]]),
      obj = "Q_parms"
    )
  )
  par_name_tbl <- rbind(
    par_name_tbl,
    data.frame(
      pars = rownames(parlist[["S_parms"]]),
      obj = "S_parms"
    )
  )
  ## Edit to allow for recdevs labeled recdev2 not recdev1
  rdnme <- names(parlist)[grepl("recdev", names(parlist))][1]
  par_name_tbl <- rbind(par_name_tbl, data.frame(
    pars = "rec_devs",
    obj = rdnme
  ))
  par_name_tbl <- rbind(par_name_tbl, data.frame(pars = "impl_error", obj = NA))
  if (isTRUE(list_pars == "all")) {
    # TODO: consider which parameters should be included in the "all" option.
    # note that impl_error is not included in all for now; not sure if it should be?
    # return(par_name_tbl)
    list_pars <- par_name_tbl[["pars"]]
    # remove SR params we don't want to be time varying
    #  SR_sigmaR, SR_regime, SR_autocorr
    pars_to_rm <- c("SR_sigmaR", "SR_regime", "SR_autocorr", "impl_error")
    tmp_keep <- which(!list_pars %in% pars_to_rm)
    list_pars <- list_pars[tmp_keep]
  }

  where_pars <- match(list_pars, par_name_tbl[["pars"]])
  subset_parname_tbl <- par_name_tbl[where_pars, ]
  est <- vector(mode = "numeric", length = nrow(subset_parname_tbl))
  for (r in seq_len(nrow(subset_parname_tbl))) {
    if (subset_parname_tbl[r, "pars"] == "rec_devs") {
      est[r] <- 0 # Is this the right assumption? or pull some val from recdevs1?
    } else if (subset_parname_tbl[r, "pars"] == "impl_error") {
      est[r] <- 0 # is this the right assumption? or pull some val from Fcast_impl_error?
    } else {
      tmp_tbl <- parlist[[subset_parname_tbl[r, "obj"]]]
      tmp_est <- tmp_tbl[rownames(tmp_tbl) == subset_parname_tbl[r, "pars"], "ESTIM"]
      est[r] <- tmp_est
    }
  }
  subset_parname_tbl <- cbind(subset_parname_tbl, data.frame(est = est))
  subset_parname_tbl
}

#' Sample vals from normal random, lognormal random, or modified AR-1
#' process.
#'
#' @param mean A single value or vector of mean parameters
#' @param sd A single value or vector of sd parameter
#' @param ar_1_phi The phi (coefficient) value for an ar 1 process. Should be
#'  between -1 and 1. 0 means an AR 1 process will NOT be used. 1 indicates a
#'  random walk. A single value or vector.
#' @param ndevs The number of sampled values to expect
#' @param dist The distribution to sample from.
#' @author Kathryn Doering
sample_vals <- function(mean,
                        sd,
                        ar_1_phi = 0,
                        ndevs,
                        dist = c("normal", "lognormal")) {
  # checks
  dist <- match.arg(dist, several.ok = FALSE)
  # note: arima.sim(list(order = c(1,0,0), ar = phi), n = nyrs)+mean, if mean, sd fixed.

  if (length(mean) == 1) {
    mean <- rep(mean, length = ndevs)
  }
  assertive.properties::assert_is_of_length(mean, ndevs)

  if (length(sd) == 1) {
    sd <- rep(sd, length = ndevs)
  }
  assertive.properties::assert_is_of_length(sd, ndevs)
  # sample
  # set.seed(seed)
  if (isTRUE(is.na(ar_1_phi) | unique(ar_1_phi) == 0)) {
    samps <- switch(dist,
      normal = stats::rnorm(n = ndevs, mean = mean, sd = sd),
      # other way of writing: obs * exp(rnorm(n = 1, mean = 0, sd = sd) - sd^2/2)
      # note term in front is the bias correction to make E(x) = exp(mu)
      # instead of med(x) = exp(mu)
      lognormal = exp(-sd^2 / 2) * stats::rlnorm(n = ndevs, meanlog = log(mean), sdlog = sd)
    )
  } else {
    if (dist == "lognormal") {
      stop(
        "Lognormal sampling cannot be use with autocorrelation. Please use",
        "normal distribution."
      )
    }
    if (length(ar_1_phi) == 1) {
      ar_1_phi <- rep(ar_1_phi, length = ndevs)
    }
    assertive.properties::is_of_length(ar_1_phi, ndevs)
    # custom function to sample, becasue sd and phi have to be fixed for the
    # arima_sim function.
    # need to think more about if this is correct or  not. this isn't exactly
    # an ar 1 process as described....because the variance isn't constant.
    samps <- vector(mode = "numeric", length = ndevs)
    for (d in seq_len(ndevs)) {
      if (d == 1) {
        past_samp <- 0 # I think
      } else {
        past_samp <- samps[d - 1]
      }
      if (abs(ar_1_phi[d]) < 1) {
        samps[d] <- mean[d] * (1 - ar_1_phi[d]) + ar_1_phi[d] * past_samp +
          stats::rnorm(1, mean = 0, sd = sd[d] / sqrt(1 / (1 - ar_1_phi[d]^2)))
      } else {
        samps[d] <- mean[d] + ar_1_phi[d] * past_samp + stats::rnorm(1, mean = 0, sd = sd[d])
        warning("An AR1 process with phi >= 1 was called, therefore will be nonstationary.")
      }
    }
  }
  samps
}

#' Add the deviation changes from the list obj to an existing df
#'
#' @param fut_list A single change input
#' @param scen The scenario name
#' @param iter The iteration name
#' @template parlist
#' @template ctl_dat
#' @param vals_df The dataframe with future om values
#' @param nyrs The number of years to extend the model forward
#' @author Kathryn Doering
#' @return A modified version of vals_df with the new changes applied.
add_dev_changes <- function(fut_list, scen, iter, parlist, dat, vals_df, nyrs, ctl) {
  if (is.null(fut_list)) {
    return(vals_df)
  }
  where_par <- match_parname(list_pars = fut_list[["pars"]], parlist = parlist)
  # model_change sampling -------
  # do sampling for things that require sampling
  if (fut_list[["pattern"]][1] == "model_change") {
    # create a dataframe of deviations; but may don't need to do? May be easier to
    # create the values we want, then back calc the deviations later?
    # dev_vals_df <- vals_df
    # dev_vals_df[,seq_len(ncol(dev_vals_df))[-1]] <- 0 # Fill with 0s b/c just want the devs.
    # now, parse the input part (and pattern[2])

    if (fut_list[["scen"]][1] == "randomize") {
      # need a different seed for each scenario and iteration
      set.seed(fut_list[["seed"]])
      tmp_scen_seed <- stats::runif(length(fut_list[["scen"]]), 0, 999999)[which(fut_list[["scen"]] == scen)]
      tmp_iter_seed <- tmp_scen_seed + iter
      fut_list[["seed"]] <- tmp_iter_seed
      fut_list[["seed"]] <- fut_list[["seed"]] + which(fut_list[["scen"]] == scen)
    } else if (fut_list[["scen"]][1] == "replicate") {
      # use the same seed for the same number iteration of different scenarios.
      fut_list[["seed"]] <- fut_list[["seed"]] + iter
    }

    # if there are no tv devs in the original model, than taking a historical
    # average is unnecessary.
    # Users are not allowed to input both sd and cv, so add a check for this.
    if (length(which(fut_list[["input"]][["ts_param"]] == "sd")) == 1 &
      length(which(fut_list[["input"]][["ts_param"]] == "cv")) == 1) {
      stop(
        "sd and cv both specified as a ts_param in the input dataframe. ",
        "Please specify only one or the other. "
      )
    }
    # Here we set the random number generator seed to the base iteration value
    set.seed(seed = fut_list[["seed"]])
    # We then save the full random number generator state which is a 626 value long
    # integer vector created from the above random seed. The second value in this vector
    # specifies the location in the psuedorandom sequence.
    seed_loc <- .Random.seed
    for (i in where_par[["pars"]]) {
      # Adjusting the location in the sequence rather than the seed allows for a much
      # greater number of random values to be drawn as only 2^32 seed values are possible
      # while the random number sequence has a period of 2^19937 - 1 values.
      seed_loc[2] <- as.integer(10000 * (as.integer(row.names(where_par)[which(where_par[["pars"]] == i)]) - 1) + 1) # The ten thousand value step assumes no MSE is ever going to run for that many years which is a safe bet even for a months as years model
      # Here we directly replace the random state (rather than adjusting the seed)
      .Random.seed <- seed_loc
      # find the mean: 1) is a mean specified? If not, use the most recent
      # value.
      # do some calcs to figure out the mean value
      # this holds if the parameter isn't time varying
      # calculate the mean. The  below holds with or without a trend
      mean <- calc_par_trend(
        val_info = fut_list[["input"]],
        val_line = "mean",
        parlist = parlist,
        ref_parm_value = where_par[where_par[["pars"]] == i, "est"],
        vals_df = vals_df, # use to potentially get trend start value.
        parname = i,
        ctl = ctl,
        par_section = where_par[where_par[["pars"]] == i, "obj"],
        dat = dat
      )
      sd <- calc_par_trend(
        val_info = fut_list[["input"]],
        val_line = "sd",
        parlist = parlist,
        ref_parm_value = 0, # may not always be correct?
        vals_df = vals_df, # use to get trend start value, and last yr.
        parname = i,
        ctl = ctl,
        par_section = where_par[where_par[["pars"]] == i, "obj"],
        dat = dat
      )
      cv <- calc_par_trend(
        val_info = fut_list[["input"]],
        val_line = "cv",
        parlist = parlist,
        ref_parm_value = 0, # may not always be correct?
        vals_df = vals_df, # use to get trend start value, and last yr.
        parname = i,
        ctl = ctl,
        par_section = where_par[where_par[["pars"]] == i, "obj"],
        dat = dat
      )
      if (any(cv != 0)) {
        # calculate the sd to use
        # sanity check for developers.
        if (length(mean) != length(cv)) {
          stop("Incorrect assumption in SSMSE. Please contact the developers")
        }
        if (any(mean <= 0)) {
          warning(
            "Parameter ", i, " has negative or 0 values and cv is used.",
            "The cv is still used by calculating sd as abs(cv*mean), ",
            "which may or may not be reasonable for this variable."
          )
        }
        sd <- abs(cv * mean)
      }

      ar_1_phi <- calc_par_trend(
        val_info = fut_list[["input"]],
        val_line = "ar_1_phi",
        parlist = parlist,
        ref_parm_value = 0, # may not always be correct?
        vals_df = vals_df, # use to get trend start value, and last yr.
        parname = i
      )

      # if it is specified: then parse this input to figure out what the
      # sampled mean should be.
      # find the sd: 2) is the sd specified? If not, use sd of 1 to sample (or historical sd?)
      samp_vals <- sample_vals(
        mean = mean, sd = sd, ar_1_phi = ar_1_phi,
        ndevs = nyrs,
        dist = fut_list[["pattern"]][2]
      )

      vals_df[[i]] <- samp_vals
    }
  } else if (fut_list[["pattern"]][1] == "custom") {
    # custom ----
    for (i in where_par[["pars"]]) {
      tmp_base <- where_par[where_par[["pars"]] == i, "est"]
      # custom will just replace any values for the pattern.
      # filter out the columns needed for the given scenario and iteration
      custom_vals <- fut_list[["input"]][fut_list[["input"]][["scen"]] %in% c("all", scen) &
        fut_list[["input"]][["par"]] == i &
        fut_list[["input"]][["iter"]] == iter, ]
      custom_vals <- dplyr::select(custom_vals, dplyr::all_of(c("yr", "value"))) %>%
        dplyr::rename(yrs = "yr")
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
#' @param val_info The line in the input df containing info about the parameter.
#' @param val_line Which line in val info to use.
#' @param ref_parm_value This is the historic parameter that the end trend value.
#'  Can be NA if the there is no line in val_info for the given parameter
#' @param vals_df The dataframe of the parameter values by year. Use to get
#'  start val and last year
#' @param parname Name of the parameter with devs from the SS3 model.
#'  will reference, if using a relative method.
#' @template parlist
#' @template ctl_dat
#' @param par_section Which parameter section should this variable be in?
#' @author Kathryn Doering
#' @return A vector of values with length ncol(vals_df), the number of future
#'  years.
calc_par_trend <- function(val_info,
                           val_line = c("mean", "sd", "cv", "ar_1_phi"),
                           ref_parm_value, vals_df, parname, parlist, ctl, par_section, dat) {
  val_line <- match.arg(val_line, several.ok = FALSE)
  # determine historical value, if necessary. This will replace the ref_parm_value
  # determine the ref_parm_value

  if (isTRUE(!is.na(val_info[val_info[["ts_param"]] == val_line, "first_yr_averaging"]) &
    !is.na(val_info[val_info[["ts_param"]] == val_line, "last_yr_averaging"]))) {
    if (parname == "rec_devs") {
      # EDITED bc par file labeled "recdev2" NOT "recdev1"
      rdnme <- names(parlist)[grepl("recdev", names(parlist))][1]
      tmp_vals <- data.frame(
        yrs = parlist[[rdnme]][, 1],
        rec_devs = parlist[[rdnme]][, 2]
      )
      tmp_vals_2 <- data.frame(yrs = vals_df[["yrs"]], rec_devs = vals_df[["rec_devs"]])
      tmp_vals <- rbind(tmp_vals, tmp_vals_2)
      to_include <- which(tmp_vals[["yrs"]] >= val_info[val_info[["ts_param"]] == val_line, "first_yr_averaging"] &
        tmp_vals[["yrs"]] <= val_info[val_info[["ts_param"]] == val_line, "last_yr_averaging"])
      tmp_vals <- tmp_vals[to_include, "rec_devs"]
      ref_parm_value <- switch(val_line,
        mean = mean(tmp_vals),
        sd = stats::sd(tmp_vals),
        cv = stats::sd(tmp_vals) / mean(tmp_vals),
        ar_1_phi = stats::arima(tmp_vals, order = c(1, 0, 0))$coef[1]
      )
    } else { # for all other parameters
      # check for tv devs
      ctl[["S_parms"]] <- rbind(
        ctl[["size_selex_parms"]], ctl[["age_selex_parms"]],
        ctl[["dirichlet_parms"]]
      )
      if (!is.null(ctl[["age_selex_parms_tv"]]) | !is.null(ctl[["size_selex_parms_tv"]])) {
        ctl[["S_parms_tv"]] <- rbind(ctl[["size_selex_parms_tv"]], ctl[["age_selex_parms_tv"]])
      }
      tv_par_def <- ctl[[par_section]][
        rownames(ctl[[par_section]]) == parname,
        c("env_var&link", "dev_link", "Block")
      ]
      if (any(tv_par_def != 0)) {
        # TODO: need to figure out in this case what the historical values are
        # and figure out the base value to use.
        # blocks/parameter trend
        # get the starting base values
        first_tv <- TRUE # switch to determine if this is the first tv defined or not.
        temp_ctl <- ctl[[par_section]]
        current_par <- which(row.names(temp_ctl) == parname)
        if (tv_par_def[["Block"]] != 0) {
          # pull out the block and/or trend parameters

          parfile_par_block_loc <-
            c(
              grep(paste0(parname, "_BLK"), rownames(parlist[[par_section]]),
                fixed = TRUE
              ),
              grep(paste0(parname, "_Trend"), rownames(parlist[[par_section]]),
                fixed = TRUE
              )
            )
          parfile_par_block <- parlist[[par_section]][parfile_par_block_loc, ]
          parfile_base_val <- parlist[[par_section]][parname, "ESTIM"]
          if (first_tv == TRUE) {
            vals <- rep(parfile_base_val,
              length.out = length(val_info[["first_yr_averaging"]]:val_info[["last_yr_averaging"]])
            )
          } # note that otherwise, vals is already defined.

          vals <- update_basevals_blocks(
            base_vals = vals,
            base_years = val_info[["first_yr_averaging"]]:val_info[["last_yr_averaging"]],
            temp_block = parfile_par_block,
            current_par = current_par,
            dat = dat,
            ctl = ctl,
            temp_ctl = temp_ctl,
            base_range = ctl[[par_section]][parname, "HI"] - ctl[[par_section]][parname, "LO"],
            baseparm = parfile_base_val,
            base_bounds = c(ctl[[par_section]][parname, "LO"], ctl[[par_section]][parname, "HI"])
          )
          first_tv <- FALSE
        }
        # env
        if (tv_par_def[["env_var&link"]] != 0) {
          parfile_base_val <- parlist[[par_section]][parname, "ESTIM"]
          if (first_tv == TRUE) {
            vals <- rep(parfile_base_val, length.out = length(val_info[["first_yr_averaging"]]:val_info[["last_yr_averaging"]]))
          } # note that otherwise, vals is already defined.
          temp_env <- parlist[[par_section]][grep(paste0(parname, "_ENV"),
            rownames(parlist[[par_section]]),
            fixed = TRUE
          ), ]
          if (ctl[[par_section]][current_par, "env_var&link"] < 0) {
            out <- r4ss::SS_output(dirname(dat[["sourcefile"]]), verbose = FALSE)
            timeseries <- out[["timeseries"]]
          } else {
            timeseries <- NA # shouldn't need unless using biology.
          }
          vals <- update_basevals_env(
            base_vals = vals,
            base_years = val_info[["first_yr_averaging"]]:val_info[["last_yr_averaging"]],
            temp_env = temp_env,
            current_par = current_par,
            timeseries = timeseries,
            temp_ctl = temp_ctl,
            dat = dat,
            base_range = ctl[[par_section]][parname, "HI"] - ctl[[par_section]][parname, "LO"],
            base_bounds = c(ctl[[par_section]][parname, "LO"], ctl[[par_section]][parname, "HI"]),
            parlist = parlist
          )
          first_tv <- FALSE
        }
        # dev
        if (tv_par_def[["dev_link"]] != 0) {
          # TODO: need to check on how to update these if not additive?
          parfile_base_val <- parlist[[par_section]][parname, "ESTIM"]
          if (first_tv == TRUE) {
            vals <- rep(parfile_base_val,
              length.out = length(val_info[["first_yr_averaging"]]:val_info[["last_yr_averaging"]])
            )
          } # note that otherwise, vals is already defined.
          dev_par <- grep(paste0(parname, "_dev"),
            row.names(parlist[[par_section]]),
            fixed = TRUE
          )
          temp_dev <- parlist[[par_section]][dev_par, ]
          # currently not sure of the difference between temp_dev and dev_seq.

          vals <- update_basevals_dev(
            base_vals = vals,
            temp_dev = temp_dev,
            dev_seq = rep(0, length.out = length(vals)),
            current_par = current_par,
            temp_ctl = temp_ctl,
            base_range = ctl[[par_section]][parname, "HI"] - ctl[[par_section]][parname, "LO"],
            base_bounds = c(ctl[[par_section]][parname, "LO"], ctl[[par_section]][parname, "HI"])
          )
          first_tv <- FALSE
        }
        # now, calculate the historical value to use as reference
        ref_parm_value <- switch(val_line,
          mean = mean(vals),
          sd = stats::sd(vals),
          cv = stats::sd(vals) / mean(vals),
          ar_1_phi = stats::arima(vals, order = c(1, 0, 0))$coef[1]
        )
      } else {
        ref_parm_value <- ref_parm_value # just keep using the default if no TV
      }

      # the base parm value should just be used as the reference in this case.
      ref_parm_value <- ref_parm_value # keeep using the default.
    }
  }

  # determine start val
  if (isTRUE(length(val_info[val_info[["ts_param"]] == val_line, "last_yr_orig_val"]) == 1 &
    val_line == "mean")) {
    start_val_yr <- val_info[val_info[["ts_param"]] == val_line, "last_yr_orig_val"]
    if (start_val_yr %in% vals_df[["yrs"]]) {
      start_val <- vals_df[vals_df[["yrs"]] == start_val_yr, parname]
    } else {
      # This should only be used when the start value is 1 year before the
      # devs start.
      assert_is_identical_to_true(start_val_yr == (vals_df[["yrs"]][1]) - 1)
      start_val <- ref_parm_value # Note: this may not be true if orig. val is time varying?
    }
  } else { # just use the base value
    start_val <- ref_parm_value # I'm not sure this is right or not for the sd???
  }
  last_yr <- vals_df[nrow(vals_df), "yrs"]
  if (isFALSE(val_line %in% val_info[["ts_param"]])) {
    # use the start value
    return(start_val)
  }
  yrs_out <- val_info[val_info[["ts_param"]] == val_line, "first_yr_final_val"] -
    val_info[val_info[["ts_param"]] == val_line, "last_yr_orig_val"] + 1
  if (is.na((yrs_out))) yrs_out <- 2 # this is just a ste
  # get end value
  # I think this should catch any NA values or NULL values for the value param value which should avoid crashes in the end value calculation below
  if (is.na(val_info[val_info[["ts_param"]] == val_line, "value"])) {
    val_info[val_info[["ts_param"]] == val_line, "value"] <- switch(val_info[val_info[["ts_param"]] == val_line, "method"],
      absolute = ref_parm_value,
      additive = 0,
      multiplier = 1
    )
  }
  if (is.null(val_info[val_info[["ts_param"]] == val_line, "value"])) {
    val_info[val_info[["ts_param"]] == val_line, "value"] <- switch(val_info[val_info[["ts_param"]] == val_line, "method"],
      absolute = ref_parm_value,
      additive = 0,
      multiplier = 1
    )
  }
  end <- switch(val_info[val_info[["ts_param"]] == val_line, "method"],
    absolute = val_info[val_info[["ts_param"]] == val_line, "value"],
    additive = val_info[val_info[["ts_param"]] == val_line, "value"] + ref_parm_value,
    multiplier = val_info[val_info[["ts_param"]] == val_line, "value"] * ref_parm_value
  )
  # Calculate a linear trend in between them
  trend <- seq(from = start_val, to = end, length.out = yrs_out)
  trend <- trend[-1] # because don't want to keep the year with the original value
  # after, want to keep the trend at the same value.
  end_val <- rep(end, length.out = last_yr -
    val_info[val_info[["ts_param"]] == val_line, "first_yr_final_val"])
  trend <- c(trend, end_val)
  # before, want to keep the trend at the original value. This is not always needed.
  if (val_info[val_info[["ts_param"]] == val_line, "last_yr_orig_val"] >=
    vals_df[["yrs"]][1]) {
    beg_val <- rep(start_val,
      length.out = val_info[
        val_info[["ts_param"]] == val_line,
        "last_yr_orig_val"
      ] - vals_df[["yrs"]][1] + 1
    )
    trend <- c(beg_val, trend)
  }
  trend
}
