README
================

<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->

# (SSMSE) Management Strategy Evaluation for Stock Synthesis (SS)

master: [![Build
Status](https://travis-ci.org/nmfs-fish-tools/SSMSE.svg?branch=master)](https://travis-ci.org/nmfs-fish-tools/SSMSE)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/nmfs-fish-tools/SSMSE?branch=master&svg=true)](https://ci.appveyor.com/project/nmfs-fish-tools/SSMSE)
[![codecov](https://codecov.io/gh/nmfs-fish-tools/SSMSE/branch/master/graph/badge.svg)](https://codecov.io/gh/nmfs-fish-tools/SSMSE)

-----

<https://nmfs-fish-tools.github.io/SSMSE/>

-----

## This is a repository for the Stock Assessment Tool: SSMSE

  - Supported by the NOAA Fisheries Integrated Toolbox

## Disclaimer

“The United States Department of Commerce (DOC) GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its
use. DOC has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed by
all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.”

<!-- - This project code is made available through GitHub but is managed by NOAA at
 https://vlab.ncep.noaa.gov/redmine/projects/integrated-fisheries-toolbox/files -->

-----

## Installing the SSMSE R package

Note that the SSMSE is a work in progress and not yet a minimum viable
product.

To install SSMSE from github:

``` r
remotes::install_github("nmfs-fish-tools/SSMSE")
```

You can read the help files with

``` r
?SSMSE
```

## An SSMSE toy example

Suppose we want to look at 2 scenarios, one where Steepness (H) is
specified correctly and one where it is specified incorrectly in an
estimation model (EM): 1. **H-ctl**: Cod operating model (H = 0.65) with
correctly specified cod model EM (fixed H = 0.65) 2. **H-1**: Cod
operating model (OM; H = 1) with misspecified cod model EM (fixed H =
0.65)

Note that this is a toy example and not a true MSE, so there is not
significantly different structure between the cod EM and OM, except for
different steepness. We will assume we want to run the MSE loop for 6
years, with a stock assessment occuring every 3 years. The cod model’s
last year is 100, so the OM is initially conditioned through year 100.
Then, after conditioning the operating model through year 100,
assessments will occur in years 100, 103, and 106. Note that the
assessment run in year 106 will generate future catch for years 107,
108, and 109, but these are not feed back into the operating model
because the MSE loop is specified to only run through year 106).

First, we will load the `SSMSE` package and create a folder in which to
run the example:

    ## Loading SSMSE

``` r
library(SSMSE) #load the package
library(r4ss) #install using remotes::install_github("r4ss/r4ss@development)
```

``` r
# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("run_SSMSE-ex")
dir.create(run_SSMSE_dir)
```

The cod model with H = 0.65 is included as external package data.
However, we will need to modify it to use as an operating model with H =
1.

``` r
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
# copy to a new location:
file.copy(from = cod_mod_path, to = run_SSMSE_dir, recursive = TRUE)
## [1] TRUE
file.rename(from = file.path(run_SSMSE_dir, "cod"), to = file.path(run_SSMSE_dir, "cod-1"))
## [1] TRUE
cod_1_path <- file.path(run_SSMSE_dir, "cod-1")
# make model read initial values from control file and not ss.par
start <- r4ss::SS_readstarter(file = file.path(cod_1_path, "starter.ss"), verbose = FALSE)
start$init_values_src # verify reading from the control file
## [1] 0
# change the natural mortality paramter from 0.2 to 0.1 in the control files
r4ss::SS_changepars(dir = cod_1_path, ctlfile = "control.ss_new", 
              newctlfile = "control_modified.ss", strings = "SR_BH_steep", newvals = 1)
## parameter names in control file matching input vector 'strings' (n=1):
## [1] "SR_BH_steep"
## These are the ctl file lines as they currently exist:
##      LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr
## 107 0.2  1 0.65   0.7  0.05       0    -4            0        0         0
##     dev_maxyr dev_PH Block Block_Fxn       Label Linenum
## 107         0      0     0         0 SR_BH_steep     107
## line numbers in control file (n=1):
## 107
## wrote new file to control_modified.ss with the following changes:
##   oldvals newvals oldphase newphase oldlos newlos oldhis newhis oldprior
## 1    0.65       1       -4       -4    0.2    0.2      1      1      0.7
##   newprior oldprsd newprsd oldprtype newprtype       comment
## 1      0.7    0.05    0.05         0         0 # SR_BH_steep
# remove files with M = 0.2
file.remove(file.path(cod_1_path, "control.ss_new"))
## [1] TRUE
file.remove(file.path(cod_1_path, "control.ss"))
## [1] TRUE
file.remove(file.path(cod_1_path, "ss.par")) # delete control file because no longer need.
## [1] TRUE
# rename file with M = 0.1 to control.ss_new () and make a copy as the control file
file.rename(from = file.path(cod_1_path, "control_modified.ss"),
            to = file.path(cod_1_path, "control.ss"))
## [1] TRUE
```

Rerun this model with no estimation to get valid ss.par and
control.ss\_new files:

``` r
# run SS with no estimateion
SSMSE:::run_ss_model(dir = cod_1_path, 
                     admb_options = "-maxfn 0 -phase 50 -nohess",
                     verbose = FALSE)
```

The argument `sample_struct` specifies the structure for sampling from
the OM (and passing to the EM). The function `create_sample_struct` can
be used to construct a simple sampling structure consistent with an
input data file:

``` r
EM_datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
sample_struct <- create_sample_struct(dat = EM_datfile, nyrs = 6) # note warning
## Warning in FUN(X[[i]], ...): Pattern not found for lencomp: FltSvy 1, Seas 1.
## Returning NA for Yr in this dataframe.
sample_struct
## $catch
##    Yr Seas FltSvy    SE
## 1 101    1      1 0.005
## 2 102    1      1 0.005
## 3 103    1      1 0.005
## 4 104    1      1 0.005
## 5 105    1      1 0.005
## 6 106    1      1 0.005
## 
## $CPUE
##    Yr Seas FltSvy  SE
## 1 105    7      2 0.2
## 
## $lencomp
##   Yr Seas FltSvy Sex Part Nsamp
## 1 NA    1      1   0    0   125
## 
## $agecomp
##    Yr Seas FltSvy Sex Part Ageerr Lbin_lo Lbin_hi Nsamp
## 1 105    1      2   0    0      1      -1      -1   500
```

This sample\_structure suggest that catch will be added to the
estimation model every year (years 101 to 106), but an index of
abundance (i.e., CPUE) and age composition (i.e., agecomp) will only be
added in year 105. We can modify this sampling strategy however we would
like.

Note that length comp (lencomp) includes an `NA` value for year. This is
because no consistent pattern was identified, so the user must define
their own input. In this case, we will remove sampling length comps all
together:

``` r
sample_struct$lencomp <- NULL # don't use length sampling
```

The same sampling structure will be used for both scenarios:

``` r
sample_struct_list <- list("H-ctl" = sample_struct, "H-1" = sample_struct)
```

We can now use `run_SSMSE` to run the MSE analysis loop:

``` r
run_res_path <- file.path(run_SSMSE_dir, "results")
dir.create(run_res_path)
# run 1 iteration and 1 scenario of SSMSE using an EM.
run_SSMSE(scen_name_vec = c("H-ctl", "H-1"), # name of the scenario
          out_dir_scen_vec = run_res_path, # directory in which to run the scenario
          iter_list = list(1:5, 1:5), # run with 5 iterations each
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = c(cod_mod_path, normalizePath(cod_1_path)), # OM files
          EM_name_vec = c("cod", "cod"), # cod is included in package data
          MS_vec = c("EM","EM"),       # The management strategy is specified in the EM
          use_SS_boot_vec = c(TRUE, TRUE), # use the SS bootstrap module for sampling
          nyrs_vec = c(6, 6),        # Years to project OM forward
          nyrs_assess_vec = c(3, 3), # Years between assessments
          rec_dev_pattern = c("none", "none"), # Don't use recruitment deviations
          impl_error_pattern = c("none", "none"), # Don't use implementation error
          sample_struct_list = sample_struct_list) # How to sample data for running the EM.
```

The function `SSMSE_summary_all` can be used to summarize the model
results in a list of dataframes. Note that if you have issues, try
reinstalling SSMSE using
`remotes::install_github("nmfs-fish-tools/SSMSE")` and restarting your R
session. Also, make sure you are using the development branch versions
of [r4ss](https://github.com/r4ss/r4ss) and
[ss3sim](https://github.com/ss3sim/ss3sim) (by installing
`remotes::install_github("r4ss/r4ss@development")` and
`remotes::install_github("ss3sim/ss3sim@development")`. These versions
should be installed automatically when SSMSE is downloaded.

``` r
# Summarize 1 iteration of output
summary <- SSMSE_summary_all(normalizePath(run_res_path))
## Extracting results from 2 scenarios
## Starting H-1 with 5 iterations
## Starting H-ctl with 5 iterations
```

Plotting and data manipulation can then be done with these summaries.
For example, SSB over time by model can be plotted. The models include
the Operating Model (cod\_OM), Estimation model (EM) for the historical
period of years 0-100 (cod\_EM\_init), the EM run with last year of data
in year 103 (cod\_EM\_103), and the EM run with last year of data in 106
(cod\_EM\_106).

``` r
library(ggplot2) # use install.packages("ggplot2") to install package if needed
library(tidyr) # use install.packages("tidyr") to install package if needed
## 
## Attaching package: 'tidyr'
## The following object is masked from 'package:testthat':
## 
##     matches
library(dplyr)
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:testthat':
## 
##     matches
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
summary$ts <- tidyr::separate(summary$ts,
                               col = model_run,
                               into = c(NA, "model_type"),
                               remove = FALSE,
                               sep = "_", 
                               extra = "drop")
# check values for cod_OM
summary$scalar %>%
  dplyr::filter(iteration == 1) %>% 
  dplyr::filter(scenario == "H-1") %>% 
  dplyr::select(iteration, scenario, SR_BH_steep, model_run)
##   iteration scenario SR_BH_steep   model_run
## 1         1      H-1        1.00    cod-1_OM
## 2         1      H-1        0.65  cod_EM_103
## 3         1      H-1        0.65  cod_EM_106
## 4         1      H-1        0.65 cod_EM_init
  

# plot SSB by year and model run - need to correct using code from the 
# think tank
ggplot2::ggplot(data = subset(summary$ts, model_run %in% c("cod_OM", "cod-1_OM", "cod_EM_106")), 
                aes(x = year, y = SpawnBio)) +
                geom_vline(xintercept = 100, color = "gray") +
                geom_line(aes(linetype = as.character(iteration), color = model_type))+
                scale_color_manual(values = c("#D65F00", "black")) +
                scale_linetype_manual(values = rep("solid", 50))+
                guides(linetype = FALSE)+
                facet_wrap(~scenario)+
                theme_classic()
```

![](man/figures/README-plot_SSB-1.png)<!-- -->

If you wish to delete the files created from this example, you can use:

``` r
unlink(run_SSMSE_dir, recursive = TRUE)
```

## How can I contribute to SSMSE?

If you have thoughts about how to implement the [upcoming
work](#roadmap-where-is-ssmse-headed-next) or are interested in helping
develop SSMSE, please contact the developers by posting an issue in this
repository or emailing <nmfs.stock.synthesis@noaa.gov>.

If you are interested in contributing, please read the [NMFS Fisheries
Toolbox R Contribution
Guide](https://github.com/nmfs-fish-tools/Resources/blob/master/CONTRIBUTING.md).
This project and everyone participating in it is governed by the [NMFS
Fisheries Toolbox Code of
Conduct](https://github.com/nmfs-fish-tools/Resources/blob/master/CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report
unacceptable behavior to <fisheries.toolbox@noaa.gov>.

## Roadmap: Where is SSMSE headed next?

SSMSE is still a work in progress, with basic framework in development.
Some new directions we hope to work on shortly:

  - Expanding on examples to illustrate the package
  - Improving usability of the wrapper functions that users access
  - Adding more complex sampling options
  - Adding functions to calculate performance metrics
  - Adding functions to make some basic plots of diagonstics and results

If you have thoughts about how to implement the upcoming work or are
interested in helping develop SSMSE, please contact the developers by
posting an issue in this repository or emailing
<nmfs.stock.synthesis@noaa.gov>