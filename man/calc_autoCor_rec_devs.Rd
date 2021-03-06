% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recdevs.R
\name{calc_autoCor_rec_devs}
\alias{calc_autoCor_rec_devs}
\title{calculate a vector of auto-correlated recruitment deviations}
\usage{
calc_autoCor_rec_devs(
  breaks,
  yrs,
  rec_autocorr_mean,
  rec_autocorr_sd,
  target_mean = 1,
  stddev = 0
)
}
\arguments{
\item{breaks}{a vector of break-points between year groups rescaled to a mean of zero}

\item{yrs}{the number of years to simulate recruitment deviations for}

\item{rec_autocorr_mean}{a vector of the mean estimated autocorrelation parameters with length equal to the order of the timelag}

\item{rec_autocorr_sd}{a vector of the standard deviation of estimated autocorrelation parameters with length equal to the order of the timelag}

\item{target_mean}{the target mean of the final achieved recruitment deviations (default=0)}

\item{stddev}{the standard deviation of simulated recruitment deviations}
}
\value{
a vector of recruitment deviations.
}
\description{
calculate a vector of auto-correlated recruitment deviations
}
