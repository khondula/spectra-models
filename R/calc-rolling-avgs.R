
calculate_running_avgs_chl <- function(df){
  # convert to a tsibble time series with regular 5 minute interval
  df_tsibble <- df %>% 
    dplyr::select(-endDateTime) %>%
    tsibble::as_tsibble(key = siteid, index = startDateTime) %>%
    index_by(datetime = lubridate::floor_date(startDateTime, unit = '5 minute')) %>%
    dplyr::summarise(chl5min = mean(chlorophyll)) %>%
    tsibble::fill_gaps(chl5min = NA)

  # then calculate rolling averages using zoo rollapply  
  chl_df_ma <- chl_df_tsibble %>%
    dplyr::mutate(chl_ma01 = zoo::rollapply(chl5min, width = 12, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma03 = zoo::rollapply(chl5min, width = 36, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma04 = zoo::rollapply(chl5min, width = 48, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma04u = zoo::rollapply(chl5min, width = 48, FUN = mean, na.rm = TRUE, partial = TRUE),
                  chl_ma06 = zoo::rollapply(chl5min, width = 72, FUN = median, na.rm = TRUE, partial = TRUE),
                  chl_ma12 = zoo::rollapply(chl5min, width = 144, FUN = median, na.rm = TRUE, partial = TRUE))
  
  return(list(df_tsibble = df_tsibble, chl_df_ma = chl_df_ma))
}
