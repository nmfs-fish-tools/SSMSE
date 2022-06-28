---
title: 'SSMSE: An R package for Management Strategy Evaluation with Stock Synthesis Operating Models'
tags:
  - R
  - Management Strategy Evaluation
  - fisheries
  - Stock Synthesis
  - stock assessment
authors:
  - name: Kathryn L. Doering^[Co-first author, Corresponding author]
    orcid: 00000-0002-0396-7044
    affiliation: 1
  - name: Nathan R. Vaughan^[Co-first author]
    affiliation: 2
  - name: John F. Walter
    affiliation: 3
  - name: Richard D. Methot
    affiliation: 4
  - name: Skyler R. Sagarese
    orcid: 0000-0002-6631-0103
    affiliation: 3
  - name: Matthew Smith
    affiliation: 3
  - name: Nicholas A. Farmer
    affiliation: 5
  - name: Shannon Calay
    affiliation: 3
  - name: Nancie J. Cummings
    affiliation: 3
  - name: Kelli F. Johnson
    orcid: 0000-0002-5149-451X
    affiliation: 6
  - name: Kristin Marshall
    affiliation: 6
  - name: Cassidy D. Peterson
    orcid: 0000-0002-0836-3039
    affiliation: 7
  - name: Ian G. Taylor
    orcid: 0000-0002-4232-5669
    affiliation: 6
  - name: Chantel R. Wetzel
    affiliation: 6

affiliations:
  - name: Caelum Research Corporation in support of Northwest Fisheries Science Center, National Oceanic and Atmospheric Administration, Seattle, WA
    index: 1
  - name: Vaughan Analytics in support of Southeast Fisheries Science Center, National Oceanic and Atmospheric Administration, Miami, FL
    index: 2
  - name: Southeast Fisheries Science Center, National Oceanic and Atmospheric Administration, Miami, FL
    index: 3
  - name: NOAA Senior Scientist for Stock Assessments, National Marine Fisheries Service, National Oceanic and Atmospheric Administration, Seattle, WA
    index: 4
  - name: Southeast Regional Office, National Oceanic and Atmospheric Administration, St. Petersburg, FL
    index: 5
  - name: Northwest Fisheries Science Center, National Oceanic and Atmospheric Administration, Seattle, WA
    index: 6
  - name: Southeast Fisheries Science Center, National Oceanic and Atmospheric Administration, Beaufort, NC
    index: 7
date: 29 March 2022
bibliography: SSMSE_JOSS.bib
---

# Statement of Need

Management Strategy Evaluation (MSE) is a decision-support tool for fisheries
management. MSE uses closed-loop simulation to evaluate the long-term
performance of management strategies with respect to societal goals like
sustainability and profits [@smith1994; @punt2014 \; \autoref{fig:MSE-diagram}].
Management strategies are pre-defined decision rules that dynamically adjust
management advice given an estimate of population status. In addition to
specifying management actions, management strategies may include the processes
of stock assessment (i.e., using models to determine the size and status of a
population) [@sainsburyetal2000].

Within MSE simulations, operating models (OMs) represent the “true” dynamics and
relevant complexity of the system. Multiple OMs are typically generated for a
single MSE to reflect different uncertainties and assess management performance
under uncertainty. Developing suitable OMs requires an analyst to, at a
minimum, define: the life history characteristics of the population and the fishing
effort and selectivity of all fisheries affecting the population; and consider: the spatial
distribution of the population and any critical environmental covariates or
species interactions. OMs should be calibrated (or “conditioned”) on available
data to ensure that model protjections are consistent with historical
observations [@punt2014]. Due to the many considerations, developing
sufficiently realistic OMs is time-intensive. 

Fortunately, the requirements for specifying OMs are largely the same as the
requirements for developing a stock assessment. Due to the overlap in
requirements and the millions of dollars invested in developing stock
assessments [@methot2015], MSE approaches that build on previous stock
assessment products can increase productivity [@maunder2014]. Stock assessment
models for federally managed species in the U.S. undergo substantial scrutiny
during a peer review process [@lynchetal2018; @brownetal2006], and thus stock
assessment models provide an excellent starting point for OMs used in MSE.

Stock Synthesis [SS3, @methotandwetzel2013] is a generalized single-species
population dynamics modeling platform widely used to assess marine fish
populations. In the U.S., more than 220 stock assessments for federally managed
populations were conducted using SS3 between 2010 and 2020 [@stocksmart]. The
rich set of feature options in SS3 allows model parameterizations that are
specific to a population. SS3 models have been used successfully as OMs in a few
MSEs [e.g., @isc2019; @sharmaetal2020;  @maunder2014]. However, in all cases, a
large amount of time and effort was required to write specialized code modifying
input files and model structure to implement MSE-specific simulations. The
majority of this code was not reusable since it was developed with a specific
MSE analysis and population in mind. While the SS3 OMs were based on existing
stock assessments, in these scenarios the specific code provided little capacity
building for future MSEs. Significant improvements in MSE throughput could be
achieved via the development of generalized MSE software that is easily
adaptable to new analyses and populations. 

Existing generalized MSE tools [e.g., openMSE, @openMSEcite; FLR’s mse R
package, @a4amsecite] have been built around custom OMs developed for use in
each package. These applications provide the benefits of a generalized MSE
codebase, but offer limited capacity to use existing stock assessment products
created using SS3 for OM development. These tools do support importing
specifications from stock assessment model files such as SS3, but converting SS3
models to a different model format often results in some loss of model
structure. Additionally, it can be time consuming for the analyst to learn a
different model format.

The primary goal of the SSMSE project was to develop a tool that can use
existing SS3 models to generate OMs and then use these OMs in MSE simulations.
This approach provides the advantages of allowing a wide selection of existing
stock assessment models to be directly used in MSE.


# Overview

SSMSE gives users flexibility in the MSE setup while reducing the amount of code
that analysts write to conduct novel MSEs. SSMSE is available as an R package
and employs other R dependencies developed for use with SS3 [e.g., ss3sim,
@andersonetal2014; r4ss, @tayloretal2021].

Users only need a few functions to run an analysis using SSMSE
(Table 1; \autoref{fig:SSMSE-workflow}). The `run_SSMSE()` wrapper function runs
the SSMSE simulations (\autoref{fig:SSMSE-steps}). Inputs to `run_SSMSE()`
include the names and locations of the conditioned SS3 models to use as
operating models (`OM_name_vec` and `OM_in_dir_vec`), the type of management
strategy for each scenario (`MS_vec`), the number of iterations to run for each
scenario (`iter_vec`), how to sample from the operating model in each
scenario (`sample_struct_list`), the number of years to run the simulations
(`nyrs_vec`) and how often the management strategy is run (`nyrs_assess_vec`).
Helper functions for setting the variables to pass to `run_SSMSE()` are
available. `run_SSMSE()` includes the option to run iterations in parallel
(`run_parallel = TRUE`), reducing the time required to run simulations. Other
options include the ability to use an SS3 estimation model or a custom function
as a management strategy and the ability to change parameters in the OM during
the projection period of the simulation. The custom function must be able to use
sampling from an SS3 data file as input and output fleet-specific catches by
year. After the simulations are complete, users can call the
`SSMSE_summary_all()` function to compile key model values from many model
folders into three summary tables. The user can then conduct further analyses
and plots based on the summaries. 

Five types of uncertainty that are typically captured in MSEs are process
uncertainty, parameter uncertainty, model uncertainty, errors in assessments,
and implementation uncertainty [@punt2014]. These can all be implemented using
SSMSE:



1. Process uncertainty  can be accounted for by including variation in parameters 
   in the OMs for parameters that are typically treated as fixed in stock assessments.
   Process uncertainty can be captured by using the `future_om_list` input to
   `run_SSMSE()`. This input allows users to specify time-varying trends and
   deviations in recruitment and other model parameter values during the
   simulation period.
2. Parameter uncertainty can be captured by the user creating different
   operating models for use in different scenarios. The helper function
   `develop_OMs()` generates new operating models with different specified
   parameter values to partially automate this process.
3. Model uncertainty includes relationships within the operating model that may
   not be specified correctly (e.g., uncertainty about which stock-recruitment
   relationship form is correct). To capture model uncertainty using SSMSE, the
   user could create multiple operating models (e.g., ones using two unique
   stock-recruitment relationship forms) to use in different scenarios.
4. Errors in assessments as defined here include specifying incorrect fixed parameter values or
   functional model structures in the estimation model and observational noise
   in data resulting in poor estimation of model parameter values (even if the 
   assessment is correctly specified outside of those estimated parameters). Users can
   adjust errors in assessments by specifying different fixed values and
   structures in different scenarios and by changing the sampling scheme through
   the `sample_struct_list` input to `run_SSMSE()` to adjust observation
   uncertainty.
5. Implementation uncertainty happens because it is difficult to perfectly
   implement a theoretical management strategy. For example, fishing may
   continue to occur after the theoretical catch limit is caught because there
   is a time lag in reporting and the catch limit is exceeded before fishing can
   be stopped. Implementation uncertainty (also known as implementation error)
   can be specified in the `future_om_list` input to `run_SSMSE()`.

The source code for SSMSE is available at
[https://github.com/nmfs-fish-tools/SSMSE](https://github.com/nmfs-fish-tools/SSMSE).
A [user
manual](https://nmfs-fish-tools.github.io/SSMSE/manual/index.html) provides more details on how to use the SSMSE tool. SSMSE can be installed from
the R console using the `remotes` package:

```{r}

remotes::install_github(“nmfs-fish-tools/SSMSE”)

```

# Case Study

Natural mortality (i.e., mortality not due to fishing) is a key life history
characteristic that can have large effects on both population estimates and
management benchmarks [e.g., @maceetal2021; @martyetal2003]. Natural mortality
is often assumed constant in population dynamics models because collecting
informative data to estimate time-varying and/or age-varying natural mortality
is difficult. However, for many populations, natural mortality likely varies in
magnitude over time [e.g., @krauseetal2020; @regularetal2022; @plaganyietal2022]. 

In this case study, we used SSMSE to investigate the effects of not accounting
for episodic natural mortality spikes in the estimation model (i.e., stock assessment
model; Table 2) on management objectives related to catch and population size.
Natural mortality spikes could occur due to periodic changes in environmental
conditions that can kill fish, such as red tide [@steidinger2009] or
upwelling-driven hypoxia [@chanetal2008]. We assessed the performance of two
distinct management strategies. We used a cod-like species as the population and
one fishing fleet and one survey in both the operating and estimation models.

Because the pattern of natural mortality is uncertain, we built three OMs, each
reflecting a different hypothesis of the “true” natural mortality dynamics of
the stock: 1) constant instantaneous natural mortality at 0.2 $yr^{-1}$ (per year);
2) natural mortality at 0.2 $yr^{-1}$ with a spike in natural mortality of
0.3 $yr^{-1}$ every 5 years; and 3) natural mortality at 0.2
$yr^{-1}$ with a spike in natural mortality of 0.4 $yr^{-1}$ every 5
years (\autoref{fig:case-study-M}). In all OMs, process uncertainty in
selectivity and recruitment was considered. One fishery length selectivity
parameter was assumed to vary randomly from year to year in the simulations. In
addition, annual recruitment deviations were assumed to vary randomly from year
to year. Selectivity and recruitment likely vary over time
[@sampsonandscott2011; @maunderandthorson2019], so allowing random deviations
was considered a more realistic characterization of uncertainty among
iterations. To ensure differences in performance are due to the management
strategy rather than from the use of different randomly selected selectivity
parameter values and recruitment deviations, SSMSE allows for the same sets of
random values to be used for each scenario by setting a seed in the
`run_SSMSE()` function. We ran 100 iterations of each scenario to characterize
the process uncertainty in recruitment and selectivity. The number of iterations
can also be specified in the `run_SSMSE()` function.

Two management strategies were tested with each of the OMs using the built-in
“EM” management strategy option in SSMSE. The "EM" management strategy uses
an SS3 model to estimate population size and status (simulating a stock
assessment), and the SS3 forecast file associated with the estimation model to
estimate management benchmarks and set future catches consistent with the
harvest controls specified by the user in the estimation model forecast file.
Two management strategies with alternative target harvest rates corresponding to
a Spawning Potential Ratio (SPR) of 30% or 45% ($SPR_{30}$ and
$SPR_{45}$, respectively) were used. The estimation model assumed constant
natural mortality of 0.2 $yr^{-1}$ (i.e., matching true base natural
mortality but not accounting for episodic spikes in natural mortality included in some OMs). 

The forecasting module of the SS3 estimation model estimated the management
benchmarks corresponding to $SPR_{30}$ or $SPR_{45}$. SPR is defined
as the fraction of the fished spawning stock biomass per recruit relative to the
unfished spawning biomass per recruit [@goodyear1993]. For example, a harvest rate corresponding to $SPR_{30}$ would lead to 30% of unfished spawning biomass per recruit (higher harvest rate), while the lower harvest rate associated with
$SPR_{45}$ would leave 45% of unfished spawning biomass per recruit (lower harvest
rate). The $SPR_{30}$ and $SPR_{45}$ management strategies
demonstrate potential tradeoffs associated with managing with less precaution by
allowing more fishing in the short term ($SPR_{30}$) or by managing with
more precaution by allowing less fishing in the short term ($SPR_{45}$).
The assessment and associated management action happened every 5 years in all
scenarios, so the SS3 forecast module for each scenario also generated
projections of five years of catch at the fishing mortality rate corresponding
to $SPR_{45}$ or $SPR_{30}$. The five years of catch was then
removed from the simulated population in the OM as each OM was projected forward
in time until the next management strategy time step (in this case study, every 5 years). The simulations applied a management
period of 50 years into the future. 

Performance metrics quantify the goals of the management system and are used to
measure the relative performance of each management strategy within the MSE. To
quantify performance in the long-term, point estimates of catch by year, standard
deviation of catch across years, and the spawning biomass (a measure
of population size) by year were extracted from the last 25 years of the simulations and
averaged for each iteration across years, then plotted by scenario. In addition,
to understand the short term effects on fishing, short-term catch was calculated
by extracting point estimates of catch from the first 10 years of the
projection, averaging for each iteration across years, and plotting.

The R code used to set up this simulation is available at
[https://nmfs-fish-tools.github.io/SSMSE/manual/M-case-study-ex.html](https://nmfs-fish-tools.github.io/SSMSE/manual/M-case-study-ex.html).

Iterations were excluded if any runs of the estimation model failed to converge,
had a high maximum gradient (>2), or had parameters on bounds. This resulted in
a maximum of six iterations (6%) excluded from any scenario.

We found that managing the stock with more precaution in the face of episodic
natural mortality spikes resulted in both higher long-term catch and less
variability in catch (\autoref{fig:case-study-violin}). However, managing the
stock with more precaution comes at the cost of less short-term catch. These
results were true regardless if natural mortality was correctly captured within
the management strategy (within the estimation model) or not.

Within the same management strategy, scenarios with higher spikes of natural
mortality that were unaccounted for had slightly lower average catch, slightly
higher catch variability, and slightly lower spawning biomass. In the short term, catch was
similar regardless of the size of the natural mortality spikes in the OM.
Although there were some consequences for not accounting for spikes in natural
mortality, the performance metrics demonstrate that the choice of management
strategy rather than capturing natural mortality correctly (or not) leads to a
larger difference in performance.

The result that managing with more precaution results in higher long-term yields
and less variability in yields is not surprising given that the level of
spawning biomass that results in maximum sustainable yield is closer to
$SPR_{45}$ than to $SPR_{30}$ for these populations ($SSB_{MSY}$ was the same 
regardless if there were episodic spkes of natural mortality or not in the OMs). 
Harford et al. (2018) used a custom-built MSE and found that managing with more 
precaution in the face of episodic natural mortality spikes resulted in lower 
probabilities of overfishing and being overfished, but at the expense of lower 
catches. Here with only a few lines of code, SSMSE demonstrates similar findings,
providing a powerful tool for rapidly conducting MSEs from existing SS3 stock 
assessment applications.


# Summary

SSMSE is a generalizable tool for stock assessment scientists and MSE
practitioners. It allows SS3 models to be used directly as OMs (and optionally
as estimation models) within MSEs. We expect that SSMSE will greatly advance the
capacity to conduct MSEs. As SS3 is one of the most widely used stock assessment
platforms, adding MSE capacity means that any existing SS3 model could be the
basis for MSE simulations with less effort and code. This will allow
practitioners to more readily evaluate a wide range of research questions and
potential management actions. 


# Acknowledgements

Thanks to Desiree Tommasi for testing SSMSE and providing comments that greatly improved
this manuscript; Robert Wildermuth and Peter Kuriyama for their extensive
testing of SSMSE; Matthew Damiano, Allan Hicks, and Huihua Lee for
early discussions regarding MSE and the development of SSMSE; and Corrine Bassin
and Christine Stawitz for their support in setting up the repository and
documentation. KD and NV acknowledge funding for this project was provided
through a NOAA Magnuson Stevens Act development grant.


# Tables

Table 1. Functions that users can call in SSMSE.

Function               | Description
---------------------- | ------------------
`run_SSMSE()`            | Run the MSE simulations
`SSMSE_summary_all()`    | Summarize MSE output
`create_sample_struct()`  | Helper function to create a list for future sampling from a model to use as input in `run_SSMSE()`
`create_future_om_list()` | Helper function that provides examples of the structure for the `future_om_list` input to `run_SSMSE()`
`develop_OMs()`           | Helper function to turn one OM into many
`run_EM()`       |  Run an SS3 estimation model (uses run_ss_model)
`run_ss_model()` | Run an SS3 model
`get_bin()`      | Get location of the SS3 binary
`parse_MS()`     | Run the management strategy and get catch by fleet for the projections. A function to reference for setting up custom management strategies. 
`plot_index_sampling()` | Create diagnostic plot to compare the sampled index values to the OM expected values and original OM conditioning index data
`plot_comp_sampling()`  | Create diagnostic plot to compare the sampled composition values to the OM expected values and original OM conditioning composition data.

Table 2. Details about the steps in the case study. See \autoref{fig:SSMSE-steps} for a general schematic of steps.

General step | Details for case study | Differences across scenarios? | Differences across iterations within a scenario? 
---------------------- | ------------------ |---------------------- | ------------------
Create OM | Use OMs that differ in their assumed natural mortality values across scenarios; recruitment deviations and fishery selectivity pattern differ across iterations within scenarios | Yes | Yes 
Sample data from OM | Use sampling scheme: survey index and age composition every 5 years, length composition from the fishery every 5 years. Use same sample size as in the original model the OM is derived from | No | No
Run estimation method | Use SS3 estimation models | No | No
Use management actions to project $n$ years of catch | Use the forecast modules from the SS3 models to project catch 5 years, managing either for $SPR_{30}$ or $SPR_{45}$ | Yes | No
Update OM with $n$ years of catch | $n = 5$ | No | No
Sample $n$ years of data | $n = 5$ | No | No

# Figures

![The main components of MSE simulations. The operating model (OM) represents
the “truth”. From the OM, data can be sampled (in sample data step) and passed
to the management strategy. The management strategy is run and usually
influences the OM (e.g., the management strategy may remove a certain
amount of catch from the OM) as the OM is stepped forward in time. The
management strategy can be subdivided into a step that estimates the population
quantities (often using an estimation method) and a step that simulates
management actions (including error in implementing the management actions).
\label{fig:MSE-diagram}](images/MSE-diagram.png)

![Schematic illustrating the steps within the `run_SSMSE()` function. Note for
simplicity, this diagram only shows steps for a single iteration, even though
multiple iterations and/or scenarios could be called through
`run_SSMSE()`.\label{fig:SSMSE-steps}](images/SSMSE-steps.png)

![Diagram illustrating a basic workflow for using SSMSE. This diagram shows the
functions (ovals) in addition to input and output objects (rounded rectangles)
and the steps for which users will write their own code (rectangle enclosed by
dashed line).\label{fig:SSMSE-workflow}](images/SSMSE-workflow.png)

![Natural mortality patterns in the OMs through the simulation years (years
101-150).\label{fig:case-study-M}](images/case-study-M.png)

![Performance metrics from the case study. Each plot shows a different performance metric. Each violin
represents the distribution of the metric from a different
scenario. Colors of the violins correspond to which management strategy was used
in the scenario. The horizontal lines within each violin represent the median. For the plot in the bottom right corner, SSB means spawning biomass and the horizontal line outside of the violins represents the spawning biomass at the maximum sustainable yield. \label{fig:case-study-violin}](images/case-study-violin.png)

# References
