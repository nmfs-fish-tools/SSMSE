% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_init_mod_files}
\alias{clean_init_mod_files}
\title{clean the initial model files}
\usage{
clean_init_mod_files(
  OM_out_dir,
  EM_out_dir = NULL,
  MS = "EM",
  overwrite = FALSE
)
}
\arguments{
\item{OM_out_dir}{The full path to the directory in which the OM is run.}

\item{EM_out_dir}{Relative or absolute path to the estimation model, if using a
model outside of the SSMSE package.}

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

\item{overwrite}{Allow existing files to be overwritten?}
}
\description{
clean the initial model files
}
