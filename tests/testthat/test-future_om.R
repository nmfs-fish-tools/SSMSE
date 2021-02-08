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
future_om_list[[1]][["pattern"]] <- "historic"
future_om_list[[1]][["input"]] <- 0.1 # jitter with a standard deviation of 0.1 times the historic standard deviation

# add values for selectivity curve param
future_om_list[[2]][["pars"]] <- "SizeSel_P_3_Fishery(1)" # had to figure this out from reading in the par file.
future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
future_om_list[[2]][["pattern"]] <- "fixed"
future_om_list[[2]][["input"]] <- data.frame(start_yr = 103, 
                                             end_yr = 104, 
                                             mod_method = 
                                               "absolute", 
                                             value = 4.5) # step change? or does this gradually change up to 4.5? The way I envisaged it your definition would cause a slope
                                                          # change where 103 was exactly the old value, 106 would be exactly 4.5,  104 would have transitioned a third of the way 
                                                          # and 105 would have transitioned two thirds. if you set start_yr=103 and end_yr=104 it would be a step with 103 at the 
                                                          # original value and 104 at 4.5.

# Object 2, apply change to all params ----
# Start by specifying 2  changes to make:
# 1. Apply change to all paramters

future_om_list_2 <- vector(mode = "list", length = 1)
future_om_list_2 <- lapply(future_om_list_2, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list_2[[1]]) <- c("pars", "scen", "pattern", "input")

# add in vals for all
future_om_list_2[[1]][["pars"]] <- "all"
future_om_list_2[[1]][["scen"]] <- c("randomize", "scen2", "scen3")
future_om_list_2[[1]][["pattern"]] <- "random"
# need clarification on the mean and error parameter type options in the input section?
# is there a way to apply the below data frame to all paramters? And better description of the cols? Not sure what the mean should be...
# should cvs be used to specify the random sampling inputs instead of standard dev?
future_om_list_2[[1]][["input"]] <- data.frame(random_type = "error", 
                                               type_inputs = "normal",
                                               type_weights = NA, 
                                               type_init_1_or_mean = "existing_mean", 
                                               type_init_2_or_sd = 0.01)

# Object 3, use custom ---
# set future values for von bert k and and for survey q

future_om_list_3 <- vector(mode = "list", length = 1)
future_om_list_3 <- lapply(future_om_list_3, function (x) x <- vector(mode = "list", length = 4))
names(future_om_list_3[[1]]) <- c("pars", "scen", "pattern", "input")
names(future_om_list_3[[2]]) <- c("pars", "scen", "pattern", "input")

# add in vals for list element 1
future_om_list_3[[1]][["pars"]] <- "VonBert_K_Fem_GP_1"
future_om_list_3[[1]][["scen"]] <- c("replicate", "all")
future_om_list_3[[1]][["pattern"]] <- "custom"
# is parameter name really necessary? should each param just be put in separate 
# lists instead if there are multiple, as shown in this example? 
# the following list has von bertK set as 0.2 for all years in scen1 and scen2,
# but set at 0.3 in scen3
future_om_list_3[[1]][["input"]] <- data.frame(
  parameter = "VonBert_K_Fem_GP_1", 
  scenario = rep(c("scen1","scen2", "scen3"), times = rep(6, times = 3)), 
  year = rep(101:106, times = 3),
  value = c(rep(0.2, times = 6*2), rep(0.3, times = 6)))

# add in vals for list element 2
future_om_list_3[[1]][["pars"]] <- "LnQ_base_Survey(2)"
future_om_list_3[[1]][["scen"]] <- c("replicate", "all")
future_om_list_3[[1]][["pattern"]] <- "custom"
# the following list has LnQ_base_Survey(2)  set as 0.02 for all years in scen1
# but set at 0.04 in scen2 and scen3
future_om_list_3[[1]][["input"]] <- data.frame(
  parameter = "LnQ_base_Survey(2)", 
  scenario = rep(c("scen1","scen2", "scen3"), times = rep(6, times = 3)), 
  year = rep(101:106, times = 3),
  value = c(rep(0.02, times = 6), rep(0.04, times = 6*2)))

# Object 4 ----
# 1. Rec devs? should be specified here? or with separate input? What about
# implementation error?
