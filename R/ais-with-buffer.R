library(lubridate)
library(dplyr)

ais_with_buffer <- function(mysite_datetime, ts_df, mycol){
  interval30 <- lubridate::interval(start = mysite_datetime - ddays(30), end = mysite_datetime + ddays(30))
  interval10 <- lubridate::interval(start = mysite_datetime - ddays(10), end = mysite_datetime + ddays(10))
  interval01 <- lubridate::interval(start = mysite_datetime - ddays(1), end = mysite_datetime + ddays(1))
  interval_12hr <- lubridate::interval(start = mysite_datetime - dhours(12), end = mysite_datetime + dhours(12))
  
  df1nona <- ts_df %>% dplyr::filter(startDateTime %within% interval30, !is.na(.data[[mycol]]))
  df1 <- ts_df %>% dplyr::filter(startDateTime %within% interval30)
  check_30day <- nrow(df1nona) > 0
  df2nona <- ts_df %>% dplyr::filter(startDateTime %within% interval10, !is.na(.data[[mycol]]))
  df2 <- ts_df %>% dplyr::filter(startDateTime %within% interval10)
  check_10day <- nrow(df2nona) > 0
  df3nona <- ts_df %>% dplyr::filter(startDateTime %within% interval01, !is.na(.data[[mycol]]))
  df3 <- ts_df %>% dplyr::filter(startDateTime %within% interval01)
  check_1day <- nrow(df3nona) > 0
  df4nona <- ts_df %>% dplyr::filter(startDateTime %within% interval_12hr, !is.na(.data[[mycol]]))
  df4 <- ts_df %>% dplyr::filter(startDateTime %within% interval_12hr)
  check_12hr <- nrow(df4nona) > 0
  
  df <- data.frame(flightline_datetime = mysite_datetime,
                   check_30day = check_30day,
                   check_10day = check_10day,
                   check_1day = check_1day,
                   check_12hr = check_12hr)
  return(list(summary = df, data30 = df1, data10 = df2, data1= df3, dataHr = df4))
  
}

# mycol is the data to be not NA
# this function is a shorter window for matching AOS and AIS data

ais_with_buffer2 <- function(mysite_datetime, ts_df, mycol){
  interval3d <- lubridate::interval(start = mysite_datetime - ddays(3), end = mysite_datetime + ddays(3))
  interval1d <- lubridate::interval(start = mysite_datetime - ddays(1), end = mysite_datetime + ddays(1))
  interval_6hr <- lubridate::interval(start = mysite_datetime - dhours(6), end = mysite_datetime + dhours(6))
  interval_1hr <- lubridate::interval(start = mysite_datetime - dhours(6), end = mysite_datetime + dhours(6))
  
  df1nona <- ts_df %>% dplyr::filter(startDateTime %within% interval3d, !is.na(.data[[mycol]]))
  df1 <- ts_df %>% dplyr::filter(startDateTime %within% interval3d)
  check_3day <- nrow(df1nona) > 0
  df2nona <- ts_df %>% dplyr::filter(startDateTime %within% interval1d, !is.na(.data[[mycol]]))
  df2 <- ts_df %>% dplyr::filter(startDateTime %within% interval1d)
  check_1day <- nrow(df2nona) > 0
  df3nona <- ts_df %>% dplyr::filter(startDateTime %within% interval_6hr, !is.na(.data[[mycol]]))
  df3 <- ts_df %>% dplyr::filter(startDateTime %within% interval_6hr)
  check_6hr <- nrow(df3nona) > 0
  df4nona <- ts_df %>% dplyr::filter(startDateTime %within% interval_1hr, !is.na(.data[[mycol]]))
  df4 <- ts_df %>% dplyr::filter(startDateTime %within% interval_1hr)
  check_1hr <- nrow(df4nona) > 0
  
  df <- data.frame(aos_datetime = mysite_datetime,
                   check_3day = check_3day,
                   check_1day = check_1day,
                   check_6hr = check_6hr,
                   check_1hr = check_1hr)
  return(list(summary = df, data3d = df1, data1d = df2, data6h= df3, data1hr = df4))
  
}
