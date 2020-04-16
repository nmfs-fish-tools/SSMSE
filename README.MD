# README

# (SSMSE) Management Strategy Evaluation for Stock Synthesis (SS)

master:
[![Build Status](https://travis-ci.org/nmfs-fish-tools/SSMSE.svg?branch=master)](https://travis-ci.org/nmfs-fish-tools/SSMSE)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/nmfs-fish-tools/SSMSE?branch=master&svg=true)](https://ci.appveyor.com/project/nmfs-fish-tools/SSMSE)
[![codecov](https://codecov.io/gh/nmfs-fish-tools/SSMSE/branch/master/graph/badge.svg)](https://codecov.io/gh/nmfs-fish-tools/SSMSE)

development:
[![Build Status](https://travis-ci.org/nmfs-fish-tools/SSMSE.svg?branch=development)](https://travis-ci.org/nmfs-fish-tools/SSMSE)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/nmfs-fish-tools/SSMSE?branch=development&svg=true)](https://ci.appveyor.com/project/nmfs-fish-tools/SSMSE)
[![codecov](https://codecov.io/gh/nmfs-fish-tools/SSMSE/branch/development/graph/badge.svg)](https://codecov.io/gh/nmfs-fish-tools/SSMSE)


**************

https://nmfs-fish-tools.github.io/SSMSE/


**************

## This is a repository for the Stock Assessment Tool: SSMSE
- Supported by the NOAA Fisheries Integrated Toolbox


## Disclaimer

“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”

<!-- - This project code is made available through GitHub but is managed by NOAA at
 https://vlab.ncep.noaa.gov/redmine/projects/integrated-fisheries-toolbox/files -->

***** *******

Installing the SSMSE R package
---------

Note that the SSMSE is a work in progress and not yet a minimum viable product.

To install SSMSE from github and load it:
``` r
library("devtools") # note that library("remotes") can also be used. Both can access `install_github`. 
install_github("nmfs-fish-tools/SSMSE")
library(SSMSE)
```
You can read the help files with

``` r
?SSMSE
```

An SSMSE toy example
---------

This comes from the documentation examples for function `run_SSMSE_iter`. See `?SSMSE::run_SSMSE_iter` to view this and an additional example.

This code runs 1 iteration of an MSE feedback loop. The code specifies the operating model (OM) as "cod", which is an example model included in the external data for the SSMSE package. Because this is a toy example and not a true MSE, the cod model is also used as the estimation model(EM) to get future catch. The MSE loop runs for 6 years, with a stock assessment occuring every 3 years. The cod model's last year is 100, so the OM is initially conditioned through year 100. Then, after conditioning the operating model through year 100, assessments will occur in years 100, 103, and 106. Note that the assessment run in year 106 will generate future catch for years 107, 108, and 109, but these are not feed back into the operating model because the MSE loop is specified to only run through year 106). The argument `dat_str` specifies the data structure to add to the estimation model. In this case, we add catch every year from fleet to the estimation model and an index of abundance (CPUE) in years 102 and 105 for fleet 2. Because they are not specified, no length or age composition data is added.

```r
# Create a temporary folder for the output
temp_path <- file.path(tempdir(), "run_SSMSE_iter-ex")
dir.create(temp_path)
# run 1 iteration and 1 scenario of SSMSE using an EM.
run_SSMSE_iter(OM_name = "cod",
               MS = "EM",
               out_dir = temp_path,
               EM_name = "cod",
               rec_dev_iter = rep(0, 6),
               impl_error = rep(1, 6*2),
               nyrs = 6,
               nyrs_assess = 3,
               dat_str = list(
                 catch = data.frame(year = 101:106, seas = 1, fleet = 1),
                 CPUE = data.frame(year = c(102, 105), seas = 7, index = 2)
               )
)
# if you want to delete the files:
#unlink(temp_path, recursive = TRUE)

```
