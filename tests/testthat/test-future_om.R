# create a test object of user input that can change the OM's future structure
# assume this could be compatible with cod and that there are 3 scenarios: scen1, scen2, and scen3

# Object 1 description ----
# Start by specifying 2  changes to make:
# 1. Make M vary randomly around its historic mean for scen2 and scen 3
# 2. Change 1 parameter of the selectivity curve at a certain time point from
#  5.1 to 4.5 for scen2 only

future_om_list <- vector(mode = "list", length = 2)
future_om_list <- lapply(future_om_list, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for M
future_om_list[[1]][["pars"]] <- "NatM_p_1_Fem_GP_1"
future_om_list[[1]][["scen"]] <- c("replicate", "scen2", "scen3")
future_om_list[[1]][["pattern"]] <- c("model_change","norm")
future_om_list[[1]][["input"]] <- data.frame(first_yr_averaging = 1,
                                             last_yr_averageing = 100,
                                             last_yr_orig_val = 100, # or maybe this should be 100, the last yr of the model? Yes I think it should be 100.
                                             first_yr_final_val = 101, 
                                             ts_param = "sd", 
                                             method = "multiplier", 
                                             value = 1)
  # jitter with a standard deviation equal to the historic standard deviation
  # NOTE: The way this is set up it would jitter with standard deviation equal to the standard deviation in year 100 not an average over the 
  # whole series. If we use the logic that year 100 represents the historic model and therefore a single year for averaging. Setting start_yr = 1 (or 0?)
  # would do an average of the whole historic period.

# add values for selectivity curve param. step change occuring in year 103
future_om_list[[2]][["pars"]] <- "SizeSel_P_3_Fishery(1)" # had to figure this out from reading in the par file.
future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
future_om_list[[2]][["pattern"]] <- "model_change"
future_om_list[[2]][["input"]] <- data.frame(start_yr = 102, # or should this be 102 for a step change? yes so the step starts at the end of 102 and is completed by the start of 103 so instant
                                             end_yr = 103, 
                                             ts_param = "mean",
                                             method = "absolute", 
                                             value = 4.5)

# Object 2, apply change to all params ----
# Start by specifying 2  changes to make:
# 1. Apply a random jitter to all parameters change to all paramters
#NOTE: This is only applying the jitter in scenarios 2 and 3

future_om_list_2 <- vector(mode = "list", length = 1)
future_om_list_2 <- lapply(future_om_list_2, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list_2[[1]]) <- c("pars", "scen", "pattern", "input")

# add jittered values using mean from historic time series to all future parameters.
future_om_list_2[[1]][["pars"]] <- "all"
future_om_list_2[[1]][["scen"]] <- c("randomize", "scen2", "scen3")
future_om_list_2[[1]][["pattern"]] <- "model_change"
# should cvs be used to specify the random sampling inputs instead of standard dev?
future_om_list_2[[1]][["input"]] <- data.frame(start_yr = c(1, 1),
                                               end_yr = c(101, 101),
                                               ts_param = c("sd", "mean"), # how to specify if this uses historic mean or mean in the last model year?
                                               method = c("absolute", "multiplier"), 
                                               value = c(0.1, 1))
                # As mentioned above the choice of start_yr=1 vs 100 would differentiate between final year value vs whole model average.
                # i.e. I changed it to start_yr=1 above which would calculate an average over the 100 historic series. Setting it at start_yr=100 would just use the 
                # value in year 100 and make an instant step change in year 101.
                # We will have to think more about how to handle the rec_dev and environmental vals as they have both variability in their deviations and 
                # specified st_dev in each year??? Not sure what the best way to handle is. NOTE: Having thought about it more I think that we 
                # should largely ignore the annual model uncertainties on rec_devs/implementation error/environmental indicies and just calculate sd/variance/autocorrelation from the
                # time-series of their values, I think this makes more sense as that is the pattern we are trying to replicate.

# Object 3, use custom ---
# set future values for von bert k and and for survey q


future_om_list_3 <- vector(mode = "list", length = 2)
future_om_list_3 <- lapply(future_om_list_3, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list_3[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list_3[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for list element 1
future_om_list_3[[1]][["pars"]] <- "VonBert_K_Fem_GP_1"
future_om_list_3[[1]][["scen"]] <- c("replicate", "all")
future_om_list_3[[1]][["pattern"]] <- "custom"
# is parameter name really necessary? should each param just be put in separate 
# lists instead if there are multiple, as shown in this example? 
# I don't think it is really any easier if you have it have it split out into lists,
# and their is a chance you could want to apply the same custom series to a number of 
# parameters at once like mirrored fleets maybe? in which case the current method should be simpler?
# the following list has von bertK set as 0.2 for all years in scen1 and scen2,
# but set at 0.3 in scen3
future_om_list_3[[1]][["input"]] <- data.frame(
  parameter = "VonBert_K_Fem_GP_1",
  scenario = rep("all", times = 6*5), 
  iteration = rep(1:5, times = 6),
  year =  rep(101:106, times = rep(5, times = 6)),
  value = rep(0.2, times = 6*5))

# add in vals for list element 2 - This parameter has a positive, deterministic
# trend over time that ends at 1.5 of the original mean value.
future_om_list_3[[2]][["pars"]] <- "LnQ_base_Survey(2)"
future_om_list_3[[2]][["scen"]] <- c("replicate", "all")
future_om_list_3[[2]][["pattern"]] <- c("model_change","norm")
future_om_list_3[[2]][["input"]] <- data.frame(start_yr = 100, 
                                               end_yr = 106,
                                               ts_param = "mean",
                                               method = c("multiplier"), 
                                               value = 1.5)

# Object 4 ----
# Specify recdevs and implementation error
future_om_list_4 <- vector(mode = "list", length = 2)
future_om_list_4 <- lapply(future_om_list_4, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list_4[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list_4[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for list element 1 - use recdevs with the same sd as the historic ones, 
# but with autocorrelation?
future_om_list_4[[1]][["pars"]] <- "rec_devs" # or recdevs, not sure which way to spell is better
future_om_list_4[[1]][["scen"]] <- c("replicate", "all")
future_om_list_4[[1]][["pattern"]] <- c("model_change","norm")
future_om_list_4[[1]][["input"]] <- data.frame(start_yr = c(101, 101),
                                               end_yr = c(106, 106),
                                               ts_param = c("sd", "ar_1_param"),
                                               method = c("multiplier", "absolute"),
                                               value = c(1, .5)) # NOTE: Auto correlation has to be a value less than 1 and greater than -1 to be stationary
                                                                # a value of 1 would be a perfect random walk. Negative values can have weird stationary distributions that 
                                                                 # oscillate positive to negative, kind of like a sound wave where the amplitude is the random walk. 

# add in vals for list element 2
future_om_list_4[[2]][["pars"]] <- "impl_error" 
future_om_list_4[[2]][["scen"]] <- c("randomize", "all")
future_om_list_4[[2]][["pattern"]] <- "custom"
future_om_list_4[[2]][["input"]] <- data.frame(
  parameter = "impl_error",
  scenario = rep(c("scen1","scen2", "scen3"), times = rep(6*5, times = 3)), 
  iteration = rep(1:5, times = 3*6),
  year = rep(rep(101:106, times = rep(5, times = 6)), times = 3),
  value = c(rep(1.05, times = 6*5), rep(1.10, times = 6*5*2)) # this specifies catch as 5% or 10% greater than the catch from the MS
  
  #NOTE: While testing the timeseries sim I got to thinking, should we offer an option for the user to specify the random distribution function? 
  #maybe just a couple of options like normal, lognormal, and uniform? not sure if others would be needed? just a thought. YES, for the model change option.
)
