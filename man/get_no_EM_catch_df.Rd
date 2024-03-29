% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MS_functions.R
\name{get_no_EM_catch_df}
\alias{get_no_EM_catch_df}
\title{Get the data frame of catch for the next iterations when not using an
estimation model.}
\usage{
get_no_EM_catch_df(OM_dir, yrs, MS = "last_yr_catch")
}
\arguments{
\item{OM_dir}{The full path to the OM directory.}

\item{yrs}{A vector of years}

\item{MS}{The management strategy to use. Current options
are:  \code{"last_yr_catch"} which uses the previous year's catch;
\code{"no_catch"} which uses 0 catch; \code{"EM"} which uses an stock
synthesis model as the estimation method and the management strategy as
defined in the forecast file of the stock synthesis estimation method;
\code{"Interim"} to modify catch based on survey predictions between
assessments. Users can also specify their own management strategies as a
function. For example, if the function is
called "my_ms" then the user should specify MS = "my_ms" and specify the
path to the file containing the function in \code{custom_MS_source}.}
}
\value{
A dataframe of future catch.
}
\description{
Get the data frame of catch for the next iterations when not using an
estimation model.
}
\author{
Kathryn Doering
}
