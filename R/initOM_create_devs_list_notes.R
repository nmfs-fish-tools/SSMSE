# different pseudocode flow, b/c may need to replicate vals across scenarios.

# similar flow to how the recdevs or imp error is constructed but better parsed
# out into small functions. These values can be passed down to the lower level
# functions. In theory, we could allow people to just pass their own values in
# a list of list format also. 

# I'm not sure if the current way seeds are written will work or not... Because
# there probably won't be a good way to loop that allows to only set the seed 
# 1 x and continue to get the necessary values. Better to just set the seed once, 
# perhaps, and get the values.

# from top level function, translate the user's input to the devs list.
# create a list of devs for each year which is layered on top of each other.
# 1 list for each scenario and nested in that for each iteration.
# for each list element, look through and figure out which scenario/iters to 
# apply it to
# do the sampling
# add onto current dev vals (modify them) or create new columns in each list

# note: to replicate values, the nyrs needs to be the same across the iterations, perhaps? Or 
# just truncate the values? Does it make sense to match up the values of years 
# or just add them as relative years?

