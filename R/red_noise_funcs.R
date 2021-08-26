#' Calculate uncertainty and biases in historic composition data
#'
#' @param data_obs A data frame of observed composition data extracted from SS .dat file
#' @param data_exp A data frame of the expected composition data as estimated by an SS assessment model
#' @param bins A vector object including the composition bins
#' @param merge_fleets TRUE/FALSE should fleets be merged to calculate variance and biases (Defaults to FALSE)
#' @param merge_seasons TRUE/FALSE should seasons be merged to calculate variance and biases (Defaults to TRUE)
#' @param merge_genders TRUE/FALSE should genders be merged to calculate variance and biases (Defaults to TRUE)
#' @param fleets A vector of the fleet numbers to analyze composition uncertainty for (Default is all fleets if NULL)
#' @param years A vector of the years to include when calculating composition uncertainty (Default is all years if NULL)
#' @param seasons A vector of the seasons to include when calculating composition uncertainty (Default is all years if NULL)
#' @param genders A vector of the genders to analyze composition uncertainty for (Default is all genders if NULL)
#'
#'
#' @author Nathan R. Vaughan
#'
#' @return A list object with uncertainty and bias characteristics to inform data simulation.
#'
calc_comp_var <- function(data_obs, data_exp, bins, fleets = NULL, years = NULL, seasons = NULL, merge_genders = TRUE, genders = NULL, merge_seasons = TRUE, merge_fleets = FALSE) {

  # Create a list to hold the calculated uncertainty results
  Comp_uncert <- list()

  # Check if a range of years is specified if not select all years in the input composition data frame
  if (!is.null(years)) {
    years <- years[is.element(years, unique(data_obs[["Yr"]]))]
  } else {
    years <- unique(data_obs[["Yr"]])
  }

  # Check if select seasons are specified if not select all seasons in the input composition data frame
  if (!is.null(seasons)) {
    seasons <- seasons[is.element(seasons, unique(data_obs[["Seas"]]))]
  } else {
    seasons <- unique(data_obs[["Seas"]])
  }

  # Check if select genders are specified if not select all genders in the input composition data frame
  if (!is.null(genders)) {
    genders <- genders[is.element(genders, unique(data_obs[["Gender"]]))]
  } else {
    genders <- unique(data_obs[["Gender"]])
  }

  # Check if select fleets are specified if not select all fleets in the input composition data frame
  if (!is.null(fleets)) {
    fleets <- fleets[is.element(fleets, unique(data_obs[["FltSvy"]]))]
  } else {
    fleets <- unique(data_obs[["FltSvy"]])
  }

  # Subset the input observed and selected data to retain only the year, season, gender, and fleet
  # elements specified above
  data_obs <- data_obs[is.element(data_obs[["Yr"]], years) & is.element(data_obs[["FltSvy"]], fleets) & is.element(data_obs[["Seas"]], seasons) & is.element(data_obs[["Gender"]], genders) & data_obs[["Yr"]] > 0, ]
  data_exp <- data_exp[is.element(data_exp[["Yr"]], years) & is.element(data_exp[["FltSvy"]], fleets) & is.element(data_exp[["Seas"]], seasons) & is.element(data_exp[["Gender"]], genders) & data_exp[["Yr"]] > 0, ]

  # If merge fleets was set as true change all fleets to 0 so that data will be aggregated across them all
  if (merge_fleets == TRUE) {
    fleets <- 0
    data_obs[["FltSvy"]] <- 0
    data_exp[["FltSvy"]] <- 0
  }

  # If merge genders was set as true change all genders to 0 so that data will be aggregated across them all
  if (merge_genders == TRUE) {
    genders <- 0
    data_obs[["Gender"]] <- 0
    data_exp[["Gender"]] <- 0
  }

  # If merge seasons was set as true change all seasons to 0 so that data will be aggregated across them all
  if (merge_seasons == TRUE) {
    seasons <- 0
    data_obs[["Seas"]] <- 0
    data_exp[["Seas"]] <- 0
  }

  # Add the target fleets, seasons, and genders to the output list object
  # data is always aggregated over years hence they are not included
  Comp_uncert[[1]] <- list()
  Comp_uncert[[1]][[1]] <- fleets
  Comp_uncert[[1]][[2]] <- seasons
  Comp_uncert[[1]][[3]] <- genders
  Comp_uncert[[2]] <- list()
  # Loop over all fleets, genders and seasons and calculate bias and variance effects relative to
  # composition category (e.g. size/age bin) and expected sample size respectively
  for (i in 1:length(fleets)) {
    Comp_uncert[[2]][[i]] <- list()
    for (j in 1:length(genders)) {
      Comp_uncert[[2]][[i]][[j]] <- list()
      for (l in 1:length(seasons)) {
        Comp_uncert[[2]][[i]][[j]][[l]] <- list()
        # For each fleet, gender, and season combination subset the data for analysis
        sub_dat_obs <- data_obs[data_obs[["FltSvy"]] == fleets[i] & data_obs[["Gender"]] == genders[j] & data_obs[["Seas"]] == seasons[l], ((length(data_obs[1, ]) - length(bins) + 1):length(data_obs[1, ]))]
        sub_dat_exp <- data_exp[data_exp[["FltSvy"]] == fleets[i] & data_exp[["Gender"]] == genders[j] & data_obs[["Seas"]] == seasons[l], ((length(data_exp[1, ]) - length(bins) + 1):length(data_exp[1, ]))]

        # Calculate the total observed and expected samples to allow scaling and unlisting of matrix data
        total_obs <- apply(sub_dat_obs, 1, sum)
        total_exp <- apply(sub_dat_exp, 1, sum)
        max_obs_samps <- max(sub_dat_obs[, ])

        # Initialize and data array to store square difference, sample size, and other data needed
        # to quantify expected variance relative to expected sample size
        dat_array <- matrix(NA, nrow = (length(sub_dat_obs[1, ]) * length(sub_dat_obs[, 1])), ncol = 9, dimnames = list(NULL, c("obs", "exp", "bin", "samp_size", "sq_diff", "bw", "var", "steps", "diff")))
        # Unwrap composition matrices into a long vector format and add to dat_array
        for (k in 1:length(total_obs)) {
          sub_dat_exp[k, ] <- sub_dat_exp[k, ] * total_obs[k] / total_exp[k]
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 1] <- unlist(c(sub_dat_obs[k, ]))
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 2] <- unlist(c(sub_dat_exp[k, ]))
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 3] <- bins
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 4] <- total_obs[k]
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 5] <- (dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 1] - dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 2])^2
          dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 9] <- (dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 1] - dat_array[(((k - 1) * length(bins) + 1):(k * length(bins))), 2])
        }

        # Sort the dat_array by expected sample size
        dat_array <- dat_array[order(dat_array[, 2]), ]
        # Define a smooth sequence of values from e-9 to 1.5*max expected sample size in the data
        # separated by 1% changes. This provides a manageable length but close enough values to
        # allow interpolation without too much error
        out_seq <- 1.01^(-2000:ceiling(logb(1.5 * max(dat_array[, 2]), base = 1.01)))
        output_array <- matrix(NA, nrow = (length(out_seq)), ncol = 3, dimnames = list(NULL, c("exp", "var", "steps")))
        output_array[, 1] <- out_seq

        # This is a two phase process of kernel smoothing in order to convert sample square
        # residuals into a non-parameteric model of variance relative expected sample size

        # The first phase uses a simple box kernel to extrapolate above the maximum
        # expected sample size in the data set without wild artifacts or a fixed
        # assumption of variance proportional to the mean that didn't seem to hold

        # The multiplier of one allows the box to still capture some data when centered at
        # up to 1.5*the max observation
        mult <- 1
        # The buffer of 0.1 helps smooth out the huge impact of rare observations at very
        # low probability points, which dominate the sample (i.e. two observations when the
        # expected value was 1e-5)
        buff <- 0.1
        for (k in 1:length(output_array[, 1])) {
          output_array[k, 2] <- stats::ksmooth(x = dat_array[, 2], y = dat_array[, 5], kernel = "box", bandwidth = mult * output_array[k, 1] + buff, x.points = output_array[k, 1])$y
        }

        # The second phase uses a normal distribution kernel to smooth out a lot of the jitter
        # and step artifacts that occur in the first phase at points that large deviation samples
        # start or stop being incorporated into the kernel bandwidth

        # The multiplier and buffer are reduced here to 0.25 and 0 respectively to prevent smoothing
        # any of the broader pattern in the data, this is just to locally smooth artifacts that
        # may have projected through to the MSE results.
        mult <- 0.25
        buff <- 0
        # Here we calculate the kernel interpolation at the smooth range of the output array for
        # future use calculating simulated values
        for (k in 1:length(output_array[, 1])) {
          output_array[k, 3] <- stats::ksmooth(x = output_array[, 1], y = output_array[, 2], kernel = "normal", bandwidth = mult * output_array[k, 1] + buff, x.points = output_array[k, 1])$y
        }
        # Here we calculate the kernel interpolation at all the points of the original data
        # this will be used for weighting the data to calculate mean biases of different sample bins
        # these biases should show artifacts of real sampling such as observers favoring round
        # multiples of 10 or some other bias.
        for (k in 1:length(dat_array[, 1])) {
          dat_array[k, 7] <- stats::ksmooth(x = output_array[, 1], y = output_array[, 2], kernel = "normal", bandwidth = mult * dat_array[k, 2] + buff, x.points = dat_array[k, 2])$y
        }
        output_array[, 2] <- output_array[, 3]

        # These calculations fill in the relevant extra parameter value other than expectation and
        # variance needed to simulate a random sample size. One of three sample distributions may
        # be needed depending on the ratio of expected sample size to variance.

        # Poisson distribution (if variance=expectation)
        # Value at zero as Poisson only requires a single input which is the mean and/or variance as they are equal
        output_array[output_array[, 2] == output_array[, 1], 3] <- 0
        # binomial distribution (if variance<expectation)
        # This calculates the n trials parameter which combined with the mean allows probability p of observation to be calculated
        # given that mean=np and var=npq where q=1-p
        output_array[output_array[, 2] < output_array[, 1], 3] <- output_array[output_array[, 2] < output_array[, 1], 1] / (1 - (output_array[output_array[, 2] < output_array[, 1], 2] / output_array[output_array[, 2] < output_array[, 1], 1]))
        # Negative binomial distribution (if variance>expectation)
        # This calculates the steps till sampling ended parameter which is a required input along with mean expected sample size
        output_array[output_array[, 2] > output_array[, 1], 3] <- output_array[output_array[, 2] > output_array[, 1], 1] / ((output_array[output_array[, 2] > output_array[, 1], 2] / output_array[output_array[, 2] > output_array[, 1], 1]) - 1)

        # Build an output matrix to hold data on bias in the mean sample size based on composition bin
        output_array2 <- matrix(NA, nrow = (length(bins)), ncol = 3, dimnames = list(NULL, c("bin", "Bias_exp", "Bias_se")))
        output_array2[, 1] <- bins
        # For each bin calculate the mean bias in units of standard deviation in order to account for
        # the fact that observations in a single bin could have all had different expected mean values and
        # expected variances based on those means
        for (k in 1:length(bins)) {
          output_array2[k, 2] <- sum(dat_array[dat_array[, 3] == bins[k], 9] * dat_array[dat_array[, 3] == bins[k], 4] / dat_array[dat_array[, 3] == bins[k], 7]) / sum(dat_array[dat_array[, 3] == bins[k], 4])
          output_array2[k, 3] <- (sum(dat_array[dat_array[, 3] == bins[k], 5] * dat_array[dat_array[, 3] == bins[k], 4] / dat_array[dat_array[, 3] == bins[k], 7]) / sum(dat_array[dat_array[, 3] == bins[k], 4])) / sqrt(length(dat_array[dat_array[, 3] == bins[k], 4]))
        }

        # Add the data, uncertainty estimates, and bias estimates to the output list object.
        Comp_uncert[[2]][[i]][[j]][[l]][[1]] <- dat_array
        Comp_uncert[[2]][[i]][[j]][[l]][[2]] <- output_array
        Comp_uncert[[2]][[i]][[j]][[l]][[3]] <- output_array2
      }
    }
  }
}

#' Calculate uncertainty and biases in historic composition data
#'
#' @param Comp_uncert A list object representing the output from the calc_comp_var function
#' @param data_exp A vector representing the expected composition values for which to draw a random observation dataset
#' @param bins A vector object including the composition bins
#' @param years A vector of the years to simulate data for. default is all years in data_exp if NULL.
#' @param seasons A vector of the seasons to simulate data for. default is all seasons in data_exp  if NULL.
#' @param fleets A vector of the fleets to simulate data for. default is all fleets in data_exp  if NULL.
#' @param genders A vector of the genders to simulate data for. default is all genders in data_exp  if NULL.
#'
#' @author Nathan R. Vaughan
#'
#' @return A list object with uncertainty and bias characteristics to inform data simulation.
#'
Sim_comp <- function(Comp_uncert, data_exp, bins, years = NULL, seasons = NULL, fleets = NULL, genders = NULL) {

  # For each of years, seasons, genders, and fleets check if a fix simulation range has been set and
  # if not use the full range present in the expected data frame
  if (!is.null(years)) {
    years <- years[is.element(years, unique(data_exp[["Yr"]]))]
  } else {
    years <- unique(data_exp[["Yr"]])
  }

  if (!is.null(seasons)) {
    seasons <- seasons[is.element(seasons, unique(data_exp[["Seas"]]))]
  } else {
    seasons <- unique(data_exp[["Seas"]])
  }

  if (!is.null(genders)) {
    genders <- genders[is.element(genders, unique(data_exp[["Gender"]]))]
  } else {
    genders <- unique(data_exp[["Gender"]])
  }

  if (!is.null(fleets)) {
    fleets <- fleets[is.element(fleets, unique(data_exp[["FltSvy"]]))]
  } else {
    fleets <- unique(data_exp[["FltSvy"]])
  }

  # Subset the expected data to only include the selected fleet, year, season, gender range and the replicate this as the
  # observed data object to be filled with new random observations
  data_exp <- data_exp[is.element(data_exp[["Yr"]], years) & is.element(data_exp[["FltSvy"]], fleets) & is.element(data_exp[["Seas"]], seasons) & is.element(data_exp[["Gender"]], genders) & data_exp[["Yr"]] > 0, ]
  data_obs <- data_exp

  # offset assumes that the observed composition bins are represented buy the final columns of the data frame
  # and therefore excludes any columns as the start longer than the number of bins
  offset <- length(data_obs[1, ]) - length(bins)
  # Now loop over first all rows of the expected data dataframe
  for (i in 1:length(data_obs[, 1])) {
    # Each row represents a year, fleet, season, gender which may have been estimated to have unique composition uncertainty
    # For each of fleet, season, and gender select the corresponding reference link to the uncertainty list object
    fleet_ref <- which(Comp_uncert[[1]][[1]] == data_obs[["FltSvy"]][i])
    season_ref <- which(Comp_uncert[[1]][[2]] == data_obs[["Seas"]][i])
    gender_ref <- which(Comp_uncert[[1]][[3]] == data_obs[["Gender"]][i])
    # Extract the variance and bias information for the referenced fleet, gender, season
    sub_var <- Comp_uncert[[2]][[fleet_ref]][[gender_ref]][[season_ref]][[2]]
    sub_bias <- Comp_uncert[[2]][[fleet_ref]][[gender_ref]][[season_ref]][[3]]

    # Now loop over all composition bins to calculate a new sample observation
    for (j in 1:length(bins)) {

      # Find the bounding variance estimates for the expected sample size
      ref_var_bound <- sub_var[c(max(which(sub_var[, 1] <= data_obs[i, (j + offset)])), min(which(sub_var[, 1] >= data_obs[i, (j + offset)]))), ]
      # Calculate weights to perform a linear interpolation of the variance for the exact expected sample size
      if (ref_var_bound[1, 1] != ref_var_bound[2, 1]) {
        w1 <- (data_obs[i, (j + offset)] - ref_var_bound[1, 1]) / (ref_var_bound[2, 1] - ref_var_bound[1, 1])
        w2 <- (ref_var_bound[2, 1] - data_obs[i, (j + offset)]) / (ref_var_bound[2, 1] - ref_var_bound[1, 1])
        ref_var <- (w1 * ref_var_bound[1, , drop = FALSE] + w2 * ref_var_bound[2, , drop = FALSE]) / (w1 + w2)
      } else {
        ref_var <- ref_var[1, , drop = FALSE]
      }
      # Now calculate an observed sample size using either a Poisson, binomial, or negative binomial function depending on the
      # size of the variance relative to the mean
      if (ref_var[1, 2] == ref_var[1, 1]) {
        # If mean and variance are equal sample from a Poisson with a random mean/variance bias draw based on the bias distribution for the bin
        data_obs[i, (j + offset)] <- stats::rpois(1, (ref_var[1, 1] + stats::rlnorm(1, sub_bias[j, 2], sub_bias[j, 3]) * sqrt(ref_var[1, 2])))
      } else if (ref_var[1, 2] < ref_var[1, 1]) {
        # If mean is greater than variance sample from a binomial distribution with a random mean bias draw based on the bias distribution for the bin
        data_obs[i, (j + offset)] <- stats::rbinom(1, size = ref_var[1, 3], prob = ((ref_var[1, 1] + stats::rlnorm(1, sub_bias[j, 2], sub_bias[j, 3]) * sqrt(ref_var[1, 2])) / ref_var[1, 3]))
      } else if (ref_var[1, 2] > ref_var[1, 1]) {
        # If mean is less than variance sample from a negative binomial distribution with a random mean bias draw based on the bias distribution for the bin
        data_obs[i, (j + offset)] <- stats::rnbinom(1, size = ref_var[1, 3], mu = (ref_var[1, 1] + stats::rlnorm(1, sub_bias[j, 2], sub_bias[j, 3]) * sqrt(ref_var[1, 2])))
      }
    }
  }
}
