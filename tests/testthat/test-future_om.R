# create a test object of user input that can change the OM's future structure
# assume this could be compatible with cod and that there are 3 scenarios: scen1, scen2, and scen3

# Object description ----
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
future_om_list[[1]][["input"]] <- 0.1 # not clear exactly what this value is. So in this case this would jitter with a standard deviation of 0.1 times the historic standard deviation

# add values for selectivity curve param
future_om_list[[2]][["pars"]] <- "SizeSel_P_3_Fishery(1)" # had to figure this out from reading in the par file.
future_om_list[[2]][["scen"]] <- c("replicate", "scen2")
future_om_list[[2]][["pattern"]] <- "fixed"
future_om_list[[2]][["input"]] <- data.frame(start_yr = 103, 
                                             end_yr = 106, 
                                             mod_method = 
                                               "absolute", 
                                             value = 4.5) # step change? or does this gradually change up to 4.5? The way I envisaged it your definition would cause a slope
                                                          # change where 103 was exactly the old value, 106 would be exactly 4.5,  104 would have transitioned a third of the way 
                                                          # and 105 would have transitioned two thirds. if you set start_yr=103 and end_yr=104 it would be a step with 103 at the 
                                                          # original value and 104 at 4.5.

