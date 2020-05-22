# functions related to manipulating the sample_struct param

#' Convert user input to r4ss data names
#' @param sample_struct The sampling structure specified by the user to a 
#'  run_SSMSE* wrapper function
#' @param convert_key Data frame defining how r4ss names relate to the sample_struct
#'  names. For now, a 1:1 relationship is assumed.
convert_to_r4ss_names <- function(sample_struct,
  convert_key = data.frame(
  df_name = c(rep("catch", 3), rep("CPUE", 3), rep("lencomp", 5), 
              rep("agecomp", 8)),
  r4ss_name = c("year", "seas", "fleet", 
                "year", "seas", "index", 
                "Yr", "Seas", "FltSvy", "Gender", "Part", 
                "Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo",
                "Lbin_hi"), 
  sample_struct_name = c("Yr", "Seas", "FltSvy", 
                         "Yr", "Seas", "FltSvy", 
                         "Yr", "Seas", "FltSvy", "Sex", "Part",
                         "Yr", "Seas", "FltSvy", "Sex", "Part", "Ageerr", 
                         "Lbin_lo", "Lbin_hi"), stringsAsFactors = FALSE)) {
  # note test-utils includes a check that the default assumed 
  # names for r4ss are true)
  sample_struct_r4ss <- 
    mapply(
      function(df, name_df, key) {
        df_cols <- colnames(df)
        r4ss_cols <- rep(NA, times = length(df_cols))
        for(i in seq_along(df_cols)) {
          r4ss_cols[i] <- key[key$df_name == name_df &
                              key$sample_struct_name == df_cols[i], "r4ss_name"]
        }
        colnames(df) <- r4ss_cols
        df
      }, df = sample_struct, name_df = names(sample_struct), 
      MoreArgs = list(key = convert_key),USE.NAMES = TRUE, SIMPLIFY = FALSE)
  sample_struct_r4ss
}


#' Create the sample_struct list
#' 
#' Create a sampling strcture list using the pattern in a data file and a year
#' range.
#' 
#' @param dat An r4ss list object read in using r4ss::SS_readdat() or the path
#'  (relative or absolute) to an SS data file to read in.
#' @param nyrs Number of years beyond the years included in the dat file to run
#'  the MSE. A single integer value.
#' @export
#' @author Kathryn Doering
#' @example
#'  OM_path <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
#'  # note there is a warning for lencomp because it does not have a consistent pattern 
#'  sample_struct <- create_sample_struct(OM_path, nyrs = 20)
#'  print(sample_struct)
#'
create_sample_struct <- function(dat, nyrs) {
  assertive.types::assert_is_a_number(nyrs)
  if(length(dat) == 1 & is.character(dat)) {
    dat <- SS_readdat(dat, verbose = FALSE)
  }
  catch <- dat[['catch']]
  list_name <- c("catch", "CPUE", "lencomp", "agecomp")
  sample_struct <- lapply(list_name, 
    function(name, dat) {
      df <- dat[[name]]
      if(is.null(df)) {
        return(NULL)
      }
      # get year, seas, fleet combo, ignoring -999 values.
      yr_col <- grep("year|yr", colnames(df),ignore.case = TRUE, value = TRUE)
      seas_col <- grep("seas", colnames(df), ignore.case = TRUE, value = TRUE)
      flt_col <- grep("FltSvy|fleet|index", colnames(df), ignore.case = TRUE, 
                      value = TRUE)
      # find combinations of season and fleet in the df.
      df_combo <- unique(df[,c(seas_col, flt_col), drop = FALSE])
      fill_vec <- vector(mode = "list", length = nrow(df_combo))
      for (i in seq_len(nrow(df_combo))) {
        tmp_seas <-  df_combo[i, seas_col]
        tmp_flt <- df_combo[i, flt_col]
        tmp_yrs <- df[df[[seas_col]] == tmp_seas & 
                        df[[flt_col]] == tmp_flt &
                        df[[yr_col]] != -999, yr_col]
        tmp_yrs <- unique(tmp_yrs)
        tmp_yrs <- tmp_yrs[order(tmp_yrs)]
        # figure out diff between first and second yr. 
        tmp_diff <- tmp_yrs[2] - tmp_yrs[1]
        # reconstruct the pattern
        pat <- seq(tmp_yrs[1], by = tmp_diff, length.out = length(tmp_yrs))
        if (all(pat == tmp_yrs)) { # a pattern was found
          future_pat <- seq(pat[length(pat)], dat$endyr + nyrs, by = tmp_diff)
          future_pat <- future_pat[future_pat > dat$endyr]
          future_pat <- data.frame(Yr = future_pat,
                                   Seas = tmp_seas,
                                   FltSvy = tmp_flt,
                                   stringsAsFactors = FALSE)
        } else {
          # the pattern was not found
          warning("Pattern not found for ", name, ": FltSvy ", tmp_flt, 
            ", Seas ", tmp_seas, ". Returning NA for Yr in this dataframe.")
          future_pat <- data.frame(Yr = NA,
                                   Seas = tmp_seas,
                                   FltSvy = tmp_flt,
                                   stringsAsFactors = FALSE)
        }
        fill_vec[[i]] <- future_pat
      }
      future_pat_all <- do.call("rbind", fill_vec)
                          }, dat = dat)
  names(sample_struct) <- list_name
  sample_struct
}
