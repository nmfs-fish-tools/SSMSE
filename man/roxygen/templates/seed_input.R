# When seed is many possible types as input by the user.
#' @param seed Input a fixed seed to replicate previous simulation runs. Seed
#'  can be a single value for a global seed, n_scenarios+1 length vector for 
#'  scenario specific and a global seed, n_iterations+n_scenarios+1 length 
#'  vector for iteration scenario and global seeds. Can also be a list object
#'  with a single value under \code{seed[["global"]]}, a vector under 
#'  \code{seed[["scenario"]]}, and a multiple vectors for iteration specific seeds 
#'  under \code{seed[["iter"]][[1:n_scenarios]]}.
