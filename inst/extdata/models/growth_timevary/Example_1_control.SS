#V3.30.08.02-trans;_2017_09_26;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_data_and_control_files: Example_1_data.SS // Example_1_control.SS
#V3.30.08.02-trans;_2017_09_26;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
2 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
8 #_Nblock_Patterns
 1 2 7 7 1 6 4 1 #_blocks_per_pattern 
# begin and end years of blocks
 1984 2009
 1980 1986 1999 2009
 1981 1984 1985 1988 1989 1992 1993 1996 1997 2000 2001 2004 2005 2009
 1960 1980 1981 1984 1985 1988 1993 1996 1997 2000 2001 2004 2005 2009
 1984 2009
 1981 1984 1985 1988 1989 1992 1993 2000 2001 2004 2005 2009
 1960 1984 1989 2000 2001 2004 2005 2009
 1959 1959
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
# 
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
2 #_Age(post-settlement)_for_L1;linear growth below this
12 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
 0.1 0.4 0.23 0.23 99 0 -5 0 0 0 0 0 0 0 # NatM_p_1_Fem_GP_1
 20 40 32 32 99 0 -5 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 40 65 53.2054 50 99 0 -4 0 0 0 0 0 1 0 # L_at_Amax_Fem_GP_1
 0.1 0.5 0.300609 0.3 99 0 -4 0 0 0 0 0 2 0 # VonBert_K_Fem_GP_1
 0.03 0.16 0.066 0.1 99 0 -5 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.03 0.16 0.062 0.1 99 0 -5 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
 -3 3 7e-06 7e-06 99 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Fem
 -3 3 2.9624 2.9624 99 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Fem
 -3 43 36.89 36.89 99 0 -50 0 0 0 0 0 0 0 # Mat50%_Fem
 -3 3 -0.48 -0.48 99 0 -50 0 0 0 0 0 0 0 # Mat_slope_Fem
 -3 3 1 1 99 0 -50 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 99 0 -50 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
 0 2 1 1 99 0 -50 0 0 0 0 0 0 0 # RecrDist_GP_1
 0 2 1 1 99 0 -50 0 0 0 0 0 0 0 # RecrDist_Area_1
 0 2 1 1 99 0 -50 0 0 0 0 0 0 0 # RecrDist_timing_1
 1 1 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev
 0.000001 0.999999 0.5 0.5  0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
# timevary MG parameters 
#_ LO HI INIT PRIOR PR_SD PR_type  PHASE
 -0.28528 0.200227 0 0 0.100114 6 4 # L_at_Amax_Fem_GP_1_BLK1mult_1984
 -1.10064 0.508798 0 0 0.254399 6 4 # VonBert_K_Fem_GP_1_BLK2mult_1980
 -1.10064 0.508798 0 0 0.254399 6 4 # VonBert_K_Fem_GP_1_BLK2mult_1999
# info on dev vectors created for MGparms are reported with other devs after tag parameter section 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
            12            18          15.4            15            99             0          4          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1          0.85         0.777         0.113             2          4          0          0          0          0          0          0          0 # SR_BH_steep
             1           1.6           1.1           1.1            99             0          6          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0            99             0        -50          0          0          0          0          0          0          0 # SR_regime
             0             2             0             1            99             0        -50          0          0          0          0          0          0          0 # SR_autocorr
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1962 # first year of main recr_devs; early devs can preceed this era
2008 # last year of main recr_devs; forecast devs start in following year
-1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 -9 #_recdev_early_phase
 6 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1961 #_last_early_yr_nobias_adj_in_MPD
 1962 #_first_yr_fullbias_adj_in_MPD
 2007 #_last_yr_fullbias_adj_in_MPD
 2008 #_first_recent_yr_nobias_adj_in_MPD
 1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -7 #min rec_dev
 7 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1962R 1963R 1964R 1965R 1966R 1967R 1968R 1969R 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009F 2010F 2011F 2012F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0 0 0
#
#Fishing Mortality info 
0.1 # F ballpark
-1999 # F ballpark year (neg value to disable)
1 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
0.9 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
#
#_initial_F_parms; count = 0
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#2012 2030
# F rates by fleet
# Yr:  1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fish1 0 0 0 0 0 0 0.022509 0.0297167 0.0104372 0.0149436 0.0280315 0.0229037 0.0133459 0.0265114 0.0350444 0.0375331 0.0427254 0.0238089 0.0182706 0.0229354 0.0132593 0.0208213 0.013691 0.013178 0.0172878 0.015339 0.0277346 0.029047 0.0294861 0.0392412 0.0348628 0.041668 0.0405588 0.0277604 0.0493741 0.0353477 0.0423354 0.0467582 0.0470744 0.0457325 0.0426553 0.0370065 0.0266182 0.0284913 0.0414175 0.0516364 0.0540256 0.0460957 0.050978 0.0250464 0.136493 0.136493 0.136493
# Fish2 0 0 0 0 0 0 0.000125511 0.00670312 0.0115211 0.0178588 0.0145504 0.00530468 0.00870535 0.00303969 0.00347106 0.00322145 0.00124225 0.00109612 0.00110943 0.00259888 0.00366434 0.00502621 0.00662294 0.00834974 0.0085881 0.00509123 0.0113063 0.0151475 0.0183793 0.0201202 0.0164855 0.0197351 0.019798 0.0132467 0.0246795 0.0167517 0.0216696 0.0215777 0.020903 0.0210119 0.00543163 0.0128865 0.0121347 0.0146072 0.0289175 0.0237034 0.0224932 0.0177051 0.0180621 0.0137165 0.0578713 0.0578713 0.0578713
#
#_Q_setup for every fleet, even if no survey
#_1:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm)
#_2:  extra input for link, i.e. mirror fleet
#_3:  0/1 to select extra sd parameter
#_4:  0/1 for biasadj or not
#_5:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         3         1         0         0         1         0  #  Surv1
         4         1         0         0         0         1  #  Surv2
         9         1         0         0         0         1  #  Surv7
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10             2         -0.15             0            99             0          1          0          1       1977       2007         -5          0          0  #  LnQ_base_Surv1(3)
           -15            15      -10.4373             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Surv2(4)
           -15            15      -10.9049             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Surv7(9)
# timevary Q parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type     PHASE  #  parm_name
        0.0001             2             2            99           0.5             0      -5  # LnQ_base_Surv1(3)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # LnQ_base_Surv1(3)_dev_autocorr
# info on dev vectors created for Q parms are reported with other devs after tag parameter section 
#
#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 0 0 0 0 # 1 Fish1
 0 0 0 0 # 2 Fish2
 0 0 0 0 # 3 Surv1
 0 0 0 0 # 4 Surv2
 0 0 0 0 # 5 Surv3
 0 0 0 0 # 6 Surv4
 0 0 0 0 # 7 Surv5
 0 0 0 0 # 8 Surv6
 0 0 0 0 # 9 Surv7
#
#_age_selex_types
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+nages+1; // cubic spline; with 2 additional param for scaling (average over bin range)
#_Pattern Discard Male Special
 20 0 0 0 # 1 Fish1
 20 0 0 0 # 2 Fish2
 12 0 0 0 # 3 Surv1
 10 0 0 0 # 4 Surv2
 20 0 0 0 # 5 Surv3
 15 0 0 5 # 6 Surv4
 15 0 0 5 # 7 Surv5
 15 0 0 5 # 8 Surv6
 11 0 0 0 # 9 Surv7
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
             2            15             6             8            99             0          2          0          0          0          0          0          3          1  #  AgeSel_P1_Fish1(1)
            -9             3            -2          -1.5            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P2_Fish1(1)
            -4            10             3             3            99             0          2          0          0          0          0          0          4          1  #  AgeSel_P3_Fish1(1)
            -9            15             8             2            99             0         -2          0          0          0          0          0          0          1  #  AgeSel_P4_Fish1(1)
         -2000             5         -1002            -1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P5_Fish1(1)
            -5            10            -1          0.45            99             0          2          0          0          0          0          0          5          1  #  AgeSel_P6_Fish1(1)
             2            15             8             8            99             0          2          0          0          0          0          0          6          1  #  AgeSel_P1_Fish2(2)
            -9             3            -2          -1.5            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P2_Fish2(2)
            -2            15             3             3            99             0          2          0          0          0          0          0          7          1  #  AgeSel_P3_Fish2(2)
            -9            15             8             2            99             0         -2          0          0          0          0          0          0          1  #  AgeSel_P4_Fish2(2)
         -2000             5         -1002            -1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P5_Fish2(2)
            -5             5         4.999          0.45            99             0         -2          0          0          0          0          0          0          1  #  AgeSel_P6_Fish2(2)
             0             3             2             1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P1_Surv1(3)
             0             3           0.2             1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P2_Surv1(3)
             1            15             5             8            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P1_Surv3(5)
            -9             3            -2          -1.5            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P2_Surv3(5)
            -9             9             0             3            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P3_Surv3(5)
            -9             9             3             2            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P4_Surv3(5)
           -10             5            -8            -1            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P5_Surv3(5)
            -5             5          4.99          0.45            99             0         -5          0          0          0          0          0          0          0  #  AgeSel_P6_Surv3(5)
             0             2             1             1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P1_Surv7(9)
             0             2             1             1            99             0        -50          0          0          0          0          0          0          0  #  AgeSel_P2_Surv7(9)
# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_1981
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_1985
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_1989
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_1993
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_1997
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_2001
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish1(1)_BLK3add_2005
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_1960
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_1981
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_1985
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_1993
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_1997
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_2001
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish1(1)_BLK4add_2005
           -10            10             0             0            99             0      3  # AgeSel_P6_Fish1(1)_BLK5add_1984
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_1981
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_1985
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_1989
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_1993
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_2001
           -10            10             0             0            99             0      3  # AgeSel_P1_Fish2(2)_BLK6add_2005
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish2(2)_BLK7add_1960
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish2(2)_BLK7add_1989
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish2(2)_BLK7add_2001
           -10            10             0             0            99             0      3  # AgeSel_P3_Fish2(2)_BLK7add_2005
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      1     3     1     1     0     2     0     0     0     0     0     0
#      1     4     2     2     0     2     0     0     0     0     0     0
#      3     1     4     0     0     0     0     1     1  1977  2007     5      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0
#      5     1     6     3     1     2     0     0     0     0     0     0
#      5     3    13     4     1     2     0     0     0     0     0     0
#      5     6    20     5     1     2     0     0     0     0     0     0
#      5     7    21     6     1     2     0     0     0     0     0     0
#      5     9    27     7     1     2     0     0     0     0     0     0
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
      4      1         0
      6      1         0
      4      2         0
      6      2         0
      4      3         0
      5      3         0
      6      3         0
      4      4         0
      5      4         0
      6      4         0
      4      5         0
      5      5         0
      6      5         0
      4      6         0
      5      6         0
      6      6         0
      4      7         0
      5      7         0
      6      7         0
      4      8         0
      5      8         0
      6      8         0
      4      9         0
      5      9         0
      6      9         0
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 9 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
 1 4 1 0 1
 4 5 1 0 1
 4 6 1 0 1
 4 7 1 0 1
 4 8 1 0 1
 1 9 1 0 1
 4 1 1 0 1
 4 2 1 0 1
 5 3 1 0 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  1 #_CPUE/survey:_3
#  0 #_CPUE/survey:_4
#  0 #_CPUE/survey:_5
#  0 #_CPUE/survey:_6
#  0 #_CPUE/survey:_7
#  0 #_CPUE/survey:_8
#  0 #_CPUE/survey:_9
#  1 #_agecomp:_1
#  1 #_agecomp:_2
#  0 #_agecomp:_3
#  0 #_agecomp:_4
#  0 #_agecomp:_5
#  0 #_agecomp:_6
#  0 #_agecomp:_7
#  0 #_agecomp:_8
#  0 #_agecomp:_9
#  1 #_init_equ_catch
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

