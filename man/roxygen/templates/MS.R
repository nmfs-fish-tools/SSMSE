#' @param MS The management strategy to use. Current options
#'   are:  \code{"last_yr_catch"} which uses the previous year's catch; 
#'   \code{"no_catch"} which uses 0 catch; \code{"EM"} which uses an stock 
#'   synthesis model as the estimation method and the management strategy as 
#'   defined in the forecast file of the stock synthesis estimation method;
#'   \code{"Interim"} to modify catch based on survey predictions between
#'   assessments; and \code{"custom"} to define a custom management stratey.
