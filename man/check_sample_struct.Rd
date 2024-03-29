% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkinput.R
\name{check_sample_struct}
\alias{check_sample_struct}
\title{Check sample_struct_list}
\usage{
check_sample_struct(
  sample_struct,
  valid_names = list(catch = c("Yr", "Seas", "FltSvy", "SE"), CPUE = c("Yr", "Seas",
    "FltSvy", "SE"), lencomp = c("Yr", "Seas", "FltSvy", "Sex", "Part", "Nsamp"), agecomp
    = c("Yr", "Seas", "FltSvy", "Sex", "Part", "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp"),
    meanbodywt = c("Yr", "Seas", "FltSvy", "Part", "Type", "Std_in"), MeanSize_at_Age_obs
    = c("Yr", "Seas", "FltSvy", "Sex", "Part", "Ageerr", "N_"))
)
}
\arguments{
\item{sample_struct}{A optional list including which years, seasons, and fleets
should be  added from the OM into the EM for different types of data.
If NULL, the data structure will try to be inferred from the pattern found
for each of the datatypes within the EM datafiles. Include this structure
for the number of years to extend the model out. Note that the data should
be specified using the list component names and column names as in would be
used in \code{r4ss::SS_readdat()}. The \code{run_SSMSE_iter} function
examples give an example of what this structure should be. Running the
function \code{create_sample_struct()} will also produce a sample_struct
object in the correct form. Can be NULL only when MS is not EM.}

\item{valid_names}{The list to compare sample_struct to.}
}
\description{
Check that list object sample_struct_list has the expected form, including the
correct names, correct column names (as in r4ss), and that all values in the
dataframes are integer or numeric. This does not check for if numeric or
interger values make sense given the model used.
}
\author{
Kathryn Doering
}
