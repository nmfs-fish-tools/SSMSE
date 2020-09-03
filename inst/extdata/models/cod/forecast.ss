#C forecast file written by R function SS_writeforecast
#C rerun model to get more complete formatting in forecast.ss_new
#C should work with SS version: 3.3
#C file write time: 2020-08-12 15:28:07
#
1 #_benchmarks
1 #_MSY
0.45 #_SPRtarget
0.4 #_Btarget
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF,  beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)
100 100 100 100 100 100 1 100 1 100
1 #_Bmark_relF_Basis
3 #_Forecast
1 #_Nforecastyrs
0 #_F_scalar
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF, beg_recruits, end_recruits (enter actual year, or values of 0 or -integer to be rel. endyr)
100 100 100 100 1 100
0 #_Fcast_selex
3 #_ControlRuleMethod
0.03 #_BforconstantF
0.01 #_BfornoF
1 #_Flimitfraction
3 #_N_forecast_loops
3 #_First_forecast_loop_with_stochastic_recruitment
0 #_Forecast_loop_control_3
1 #_Forecast_loop_control_4
0 #_Forecast_loop_control_5
101 #_FirstYear_for_caps_and_allocations
0 #_stddev_of_log_catch_ratio
0 #_Do_West_Coast_gfish_rebuilder_output
100 #_Ydecl
100 #_Yinit
1 #_fleet_relative_F
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
2 #_basis_for_fcast_catch_tuning
# enter list of fleet number and max for fleets with max annual catch; terminate with fleet=-9999
-9999 -1
# enter list of area ID and max annual catch; terminate with area=-9999
-9999 -1
# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999
-9999 -1
2 #_InputBasis
-9999 0 0 0
#
999 # verify end of input 
