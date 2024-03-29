% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/single_extend_OM.R
\name{add_OM_devs}
\alias{add_OM_devs}
\title{Add in future parameter values}
\usage{
add_OM_devs(ctl, dat, parlist, timeseries, future_om_dat)
}
\arguments{
\item{ctl}{A control file as read in by \code{r4ss::SS_readctl}.}

\item{dat}{A data file as read in by \code{r4ss::SS_readdat}.}

\item{parlist}{A parameter file as read in by \code{r4ss::SS_readpar_3.30}}

\item{timeseries}{The timeseries table from \code{r4ss::SS_output()}.}

\item{future_om_dat}{A data frame with random sample data for future parameter}
}
\description{
Add in future parameter values
}
\author{
Nathan Vaughan
}
