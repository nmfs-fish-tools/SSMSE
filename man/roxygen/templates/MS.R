#' @param MS The management strategy to use. Current options
#'   are:  \code{"last_yr_catch"} which uses the previous year's catch; 
#'   \code{"no_catch"} which uses 0 catch; \code{"EM"} which uses an stock 
#'   synthesis model as the estimation method and the management strategy as 
#'   defined in the forecast file of the stock synthesis estimation method;
#'   \code{"Interim"} to modify catch based on survey predictions between
#'   assessments. Users can also specify their own management strategies as a
#'   function. For example, if the function is
#'   called "my_ms" then the user should specify MS = "my_ms" and specify the 
#'   path to the file containing the function in `custom_MS_source`.
