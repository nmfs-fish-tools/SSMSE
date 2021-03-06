% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recdevs.R
\name{calc_rec_devs}
\alias{calc_rec_devs}
\title{calculate a vector of recruitment deviations}
\usage{
calc_rec_devs(breaks, yrs, target_mean = 1, stddev = 0)
}
\arguments{
\item{breaks}{a vector of break-points between year groups rescaled to a mean of zero}

\item{yrs}{the number of years to simulate recruitment deviations for}

\item{target_mean}{the target mean of the final achieved recruitment deviations (default=0)}

\item{stddev}{the standard deviation of simulated log recruitment deviations}
}
\value{
a vector of recruitment deviations.
}
\description{
calculate a vector of recruitment deviations
}
