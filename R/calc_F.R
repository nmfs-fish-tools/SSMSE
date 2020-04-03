# functions to calculate F.
#' get the Fishing mortality from a forecasting model run
#' @param timeseries from SSoutput
#' @param units_of_catch From datalist
#' @param Nfleet number of fishing fleets (does not include surveys)
#' @return a data frame of Fs and tetained catch by units (numbers or biomass) 
#'  seems to have some issues, though
get_F <- function(timeseries, units_of_catch, Nfleet) {
  #TODO: check this function works for fleets using different units, in the
  # case that a fishing fleet has no catch.
  # check that metadata matches up with the agg_F always (could use some
  # sort of merge to make sure this is true instead of cbind.)
  base_F_metadata <- timeseries[timeseries$Area==1, c("Yr", "Era", "Seas")]
  agg_F <- aggregate(timeseries[,-3],list(Yr=timeseries[,2],
                                          Seas=timeseries[,4]),
                      sum, drop = FALSE, simplify = FALSE)
  temp_F <- cbind(base_F_metadata, agg_F[, -(1:12)])
  units_catch<-ifelse(units_of_catch==1,3,6)
  cols_to_keep <- c(1:3,(((1:Nfleet))*8+3),
                    (((1:Nfleet)-1)*8+3+units_catch[Nfleet]))
  #KD: not sure what this part is doing, and seems to have some issues.
  # (extra columns called NA)
  temp_F <- temp_F[, cols_to_keep]
  temp_df<-NULL
  for(i in seq_along(Nfleet)) {
    temp_fleet<-cbind(temp_F[,c(1:3)],
                      rep(i,length(temp_F[,1])),
                      temp_F[,(i+3)],
                      temp_F[,(i+3+Nfleet)])
    temp_df<-rbind(temp_df,temp_fleet)
  }
  temp_df<-temp_df[order(temp_df[,1],temp_df[,3],temp_df[,4]),]
  names(temp_df)<-c("year","Era","seas","fleet","F","Catch_retained")
  temp_df
}