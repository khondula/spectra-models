
calculate_running_avgs_chl <- function(df){
  # convert to a tsibble time series with regular 5 minute interval
  df_tsibble <- df %>% 
    dplyr::select(-endDateTime) %>%
    tsibble::as_tsibble(key = siteid, index = startDateTime) %>%
    index_by(datetime = lubridate::floor_date(startDateTime, unit = '5 minute')) %>%
    dplyr::summarise(chl5min = mean(chlorophyll)) %>%
    tsibble::fill_gaps(chl5min = NA)

  # then calculate rolling averages using zoo rollapply  
  chl_df_ma <- df_tsibble %>%
    dplyr::mutate(chl_ma01 = zoo::rollapply(chl5min, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma03 = zoo::rollapply(chl5min, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma04 = zoo::rollapply(chl5min, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma04u = zoo::rollapply(chl5min, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  chl_ma06 = zoo::rollapply(chl5min, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma12 = zoo::rollapply(chl5min, width = 144, FUN = median, na.rm = TRUE, partial = TRUE))
  
  return(list(df_tsibble = df_tsibble, chl_df_ma = chl_df_ma))
}

calculate_running_avgs_fDOM <- function(df){
  # convert to a tsibble time series with regular 5 minute interval
  df_tsibble <- df %>% 
    dplyr::select(-endDateTime) %>%
    tsibble::as_tsibble(key = siteid, index = startDateTime) %>%
    index_by(datetime = lubridate::floor_date(startDateTime, unit = '5 minute')) %>%
    dplyr::summarise(fdom5min = mean(fDOM)) %>%
    tsibble::fill_gaps(fdom5min = NA)
  
  # then calculate rolling averages using zoo rollapply  
  df_ma <- df_tsibble %>%
    dplyr::mutate(ma01 = zoo::rollapply(fdom5min, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma03 = zoo::rollapply(fdom5min, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma04 = zoo::rollapply(fdom5min, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma04u = zoo::rollapply(fdom5min, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  ma06 = zoo::rollapply(fdom5min, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma12 = zoo::rollapply(fdom5min, width = 144, FUN = median, na.rm = TRUE, partial = TRUE))
  
  return(list(df_tsibble = df_tsibble, df_ma = df_ma))
}

calculate_running_avgs_turb <- function(df){
  # convert to a tsibble time series with regular 5 minute interval
  df_tsibble <- df %>% 
    dplyr::select(-endDateTime) %>%
    tsibble::as_tsibble(key = siteid, index = startDateTime) %>%
    index_by(datetime = lubridate::floor_date(startDateTime, unit = '5 minute')) %>%
    dplyr::summarise(turb5min = mean(turbidity)) %>%
    tsibble::fill_gaps(turb5min = NA)
  
  # then calculate rolling averages using zoo rollapply  
  df_ma <- df_tsibble %>%
    dplyr::mutate(ma01 = zoo::rollapply(turb5min, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma03 = zoo::rollapply(turb5min, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma04 = zoo::rollapply(turb5min, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma04u = zoo::rollapply(turb5min, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  ma06 = zoo::rollapply(turb5min, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  ma12 = zoo::rollapply(turb5min, width = 144, FUN = median, na.rm = TRUE, partial = TRUE))
  
  return(list(df_tsibble = df_tsibble, df_ma = df_ma))
}

calculate_running_avgs_abs <- function(df){
  # convert to a tsibble time series with regular 5 minute interval
  df_tsibble <- df %>% 
    tsibble::as_tsibble(index = burst_id) %>%
    index_by(datetime = lubridate::floor_date(burst_id, unit = '15 minute')) %>%
    dplyr::summarise(mean_abs254 = mean(mean_abs254),
                     mean_abs350 = mean(mean_abs350)) %>%
    tsibble::fill_gaps(mean_abs254 = NA, mean_abs350 = NA)
  
  # then calculate rolling averages using zoo rollapply  
  df_ma <- df_tsibble %>%
    dplyr::mutate(abs254_ma01 = zoo::rollapply(mean_abs254, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs254_ma03 = zoo::rollapply(mean_abs254, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs254_ma04 = zoo::rollapply(mean_abs254, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs254_ma04u = zoo::rollapply(mean_abs254, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  abs254_ma06 = zoo::rollapply(mean_abs254, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs254_ma12 = zoo::rollapply(mean_abs254, width = 144, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs350_ma01 = zoo::rollapply(mean_abs350, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs350_ma03 = zoo::rollapply(mean_abs350, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs350_ma04 = zoo::rollapply(mean_abs350, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs350_ma04u = zoo::rollapply(mean_abs350, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  abs350_ma06 = zoo::rollapply(mean_abs350, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  abs350_ma12 = zoo::rollapply(mean_abs350, width = 144, FUN = median, na.rm = TRUE, partial = TRUE))
  
  return(list(df_tsibble = df_tsibble, df_ma = df_ma))
}
