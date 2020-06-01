# functions related to manipulating the sample_struct param

#' Convert user input to r4ss data names
#' @param sample_struct The sampling structure specified by the user to a 
#'  run_SSMSE* wrapper function
#' @param convert_key Data frame defining how r4ss names relate to the sample_struct
#'  names. For now, a 1:1 relationship is assumed.
convert_to_r4ss_names <- function(sample_struct,
  convert_key = data.frame(
  df_name = c(rep("catch", 4), rep("CPUE", 4), rep("lencomp", 6), 
              rep("agecomp", 9)),
  r4ss_name = c("year", "seas", "fleet", "catch_se",
                "year", "seas", "index", "se_log",
                "Yr", "Seas", "FltSvy", "Gender", "Part", "Nsamp", 
                "Yr", "Seas", "FltSvy", "Gender", "Part", "Ageerr", "Lbin_lo",
                "Lbin_hi", "Nsamp"), 
  sample_struct_name = c("Yr", "Seas", "FltSvy", "SE",
                         "Yr", "Seas", "FltSvy", "SE",
                         "Yr", "Seas", "FltSvy", "Sex", "Part", "Nsamp",
                         "Yr", "Seas", "FltSvy", "Sex", "Part", "Ageerr", 
                         "Lbin_lo", "Lbin_hi", 'Nsamp'), stringsAsFactors = FALSE)) {
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
  list_name <- c("catch", "CPUE", "lencomp", "agecomp")
  sample_struct <- lapply(list_name, 
    function(name, dat) {
      df <- dat[[name]]
      if(is.null(df)) {
        return(NA)
      }
      # get year, seas, fleet combo, ignoring -999 values.
      yr_col <- grep("year|yr", colnames(df),ignore.case = TRUE, value = TRUE)
      seas_col <- grep("seas", colnames(df), ignore.case = TRUE, value = TRUE)
      flt_col <- grep("FltSvy|fleet|index", colnames(df), ignore.case = TRUE, 
                      value = TRUE)
      input_SE_col <- grep("_se|se_", colnames(df), ignore.case = TRUE, 
                           value = TRUE)# catch sample size
      Nsamp_col <-  grep("Nsamp", colnames(df), ignore.case = TRUE, 
                         value = TRUE)# input sample size
      #sanity checks. should match with 1 (or 0 in some cases) cols. Failing these
      # checks indicate a bug in the code (invalid assuptions of how to match the
      # cols.)
      assertive.properties::assert_is_of_length(yr_col, 1)
      assertive.properties::assert_is_of_length(seas_col, 1)
      assertive.properties::assert_is_of_length(flt_col, 1)
      # b/c only Nsamp or SE should exist for a df
      assertive.base::assert_is_identical_to_true(
        (length(input_SE_col) == 0 & length(Nsamp_col) == 1) |
        (length(input_SE_col) == 1 & length(Nsamp_col) == 0)
      )
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
        # add sample size, if possible
        # see if se or Nsamp is the same across years for the seas/flt. If so,
        # add to the df. If not, add NA's.
        if(length(input_SE_col) == 1) {
          tmp_SE <- unique(df[df[[seas_col]] == tmp_seas & 
                                df[[flt_col]] == tmp_flt &
                                df[[yr_col]] != -999, input_SE_col])
          if(length(tmp_SE) == 1) {
            future_pat$SE <- tmp_SE
          } else {
            future_pat$SE <- NA
            warning("NA included in column SE for ", name, ".")
          }
        }
        if(length(Nsamp_col) == 1) {
          tmp_Nsamp <- unique(df[df[[seas_col]] == tmp_seas & 
                                   df[[flt_col]] == tmp_flt &
                                   df[[yr_col]] != -999, Nsamp_col])
          if(length(tmp_Nsamp) == 1) {
            future_pat$Nsamp <- tmp_Nsamp
          } else {
            future_pat$Nsamp <- NA
            warning("NA included in column Nsamp for ", name, ".")
          }
        }
        fill_vec[[i]] <- future_pat
      }
      future_pat_all <- do.call("rbind", fill_vec)
                          }, dat = dat)
  names(sample_struct) <- list_name
  sample_struct
}

#' Get the full sample structure from user input
#' 
#' Get the ful sample structure from user input by looking at the OM data. If it
#' cannot be unambigously determined, this function will return an error
#' describing what additional user input is required
#' 
#' @param sample_struct The sample structure, as defined by the user. This need
#'  not define all the sampling structure if it can be unambiguously determined
#'  from the OM data file.
#' @param OM_out_dir The directory containing the operating model files
get_full_sample_struct <- function(sample_struct,
                                   OM_out_dir) {
  start <- r4ss::SS_readstarter(file.path(OM_out_dir, "starter.ss"), verbose = FALSE)
  dat <- r4ss::SS_readdat(file.path(OM_out_dir, start$datfile), verbose = FALSE)
  dat$catch <- dat$catch[dat$catch$year != -999, ] # filter out equilibrium values
  # years must always be specified
  full_samp_str <- mapply(
    function(x, x_name, dat) {
      tmp_dat <- dat[[x_name]]
      #to store error msgs:
      error1 <- NULL
      error2 <- NULL
      if(!"Yr" %in% colnames(x)) {
        stop("Column Yr missing from 1 or more data frames in sample_struct, but", 
             " must always be specified")
      }
      if(!"FltSvy" %in% colnames(x)){
        #there must only be 1 fleet
        flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
             value = TRUE)
        flt <- unique(tmp_dat[ ,flt_colname])
        if (length(flt) == 1) {
          x$FltSvy <- flt
        } else {
          error1 <- x_name 
          error2 <- "FltSvy"
        }
      }
      if(!"Seas" %in% colnames(x)){
        x$Seas <- NA #initial value
        seas_colname <- grep("seas", colnames(tmp_dat), ignore.case = TRUE,
                            value = TRUE)
        flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                            value = TRUE)
        for (i in unique(x$FltSvy)) {
          tmp_seas <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, seas_colname])
          if (length(tmp_seas) == 1) {
            x[x$FltSvy == i, "Seas"] <- tmp_seas
          } else {
            error1 <- c(error1, x_name)
            error2 <- c(error2, "Seas")
          }
        }
      }
      if(x_name == "catch" | x_name == "CPUE"){
        if(!"SE" %in% colnames(x)) {
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          se_colname <- grep("catch_se|se_log", colnames(tmp_dat), ignore.case = TRUE, 
                            value = TRUE)
          x$SE <- NA
          for(i in unique(x$FltSvy)) {
            tmp_se <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, se_colname])
            if(length(tmp_se) == 1) {
              x[x$FltSvy == i, "SE"] <- tmp_se
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "SE")
            }
          }
        }
      }
      if(x_name == "lencomp" | x_name == "agecomp") {
        if(!"Sex" %in% colnames(x)){
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          x$Sex <- NA
          for(i in unique(x$FltSvy)) {
            tmp_sx <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Gender"])
            if(length(tmp_sx) == 1) {
              x[x$FltSvy == i, "Sex"] <- tmp_sx
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Sex")
            }
          }
        }
        if(!"Part" %in% colnames(x)){
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          x$Part <- NA
          for(i in unique(x$FltSvy)) {
            tmp_pt <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Part"])
            if(length(tmp_pt) == 1) {
              x[x$FltSvy == i, "Part"] <- tmp_pt
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Part")
            }
          }
        }
        if(!"Nsamp" %in% colnames(x)) {
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          x$Nsamp <- NA
          for(i in unique(x$FltSvy)) {
            tmp_nsamp <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Nsamp"])
            if(length(tmp_nsamp) == 1) {
              x[x$FltSvy == i, "Nsamp"] <- tmp_nsamp
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Nsamp")
            }
          }
        }
      }
      if(x_name == "agecomp") {
        # add Ageerr, Lbin_lo and Lbin_hi
        if(!"Ageerr" %in% colnames(x)) {
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          x$Ageerr <- NA
          for(i in unique(x$FltSvy)) {  
            tmp_err <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Ageerr"])
            if(length(tmp_err) == 1) {
              x[x$FltSvy == i, "Ageerr"] <- tmp_err
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Ageerr")
            }
          }
        }
        if(!"Lbin_lo" %in% colnames(x)) {
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          for(i in unique(x$FltSvy)) {
            x$Lbin_lo <- NA
            tmp_lbin <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Lbin_lo"])
            if(length(tmp_lbin) == 1) {
              x[x$FltSvy == i, "Lbin_lo"] <- tmp_lbin
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Lbin_lo")
            }
          }
        }
        if(!"Lbin_hi" %in% colnames(x)) {
          flt_colname <- grep("FltSvy|fleet|index", colnames(tmp_dat), ignore.case = TRUE, 
                              value = TRUE)
          for(i in unique(x$FltSvy)) {
            x$Lbin_hi <- NA
            tmp_lbin <- unique(tmp_dat[tmp_dat[[flt_colname]] == i, "Lbin_hi"])
            if(length(tmp_lbin) == 1) {
              x[x$FltSvy == i, "Lbin_hi"] <- tmp_lbin
            } else {
              error1 <- c(error1, x_name)
              error2 <- c(error2, "Lbin_hi")
            }
          }
        }
      }

      if(!is.null(error1)) {
        stop("sample_struct could not be automatically expanded due to list ", 
            "elements ", paste0(error1, collapse = ", "), "; colnames ", 
            paste0(error2, collapse = ", "))
      }
    x
    }, x = sample_struct, x_name = names(sample_struct), 
  MoreArgs = list(dat = dat), USE.NAMES = TRUE, SIMPLIFY = FALSE)
  #reorder cols
  tmp_colorder <- c("Yr", "Seas", "FltSvy", "SE")
  if(!is.null(full_samp_str$catch)) {
    full_samp_str$catch <- full_samp_str$catch[, tmp_colorder]
  }
  if(!is.null(full_samp_str$CPUE)) {
    full_samp_str$CPUE <- full_samp_str$CPUE[, tmp_colorder]
  }
  tmp_colorder <- c(tmp_colorder[-length(tmp_colorder)], "Sex", "Part", "Nsamp")
  if(!is.null(full_samp_str$lencomp)) {
    full_samp_str$lencomp <- full_samp_str$lencomp[, tmp_colorder]
  }
  tmp_colorder <- c(tmp_colorder[-length(tmp_colorder)], "Ageerr", "Lbin_lo", "Lbin_hi", "Nsamp")
  if(!is.null(full_samp_str$agecomp)) {
    full_samp_str$agecomp <- full_samp_str$agecomp[, tmp_colorder]
  }
  full_samp_str
}
