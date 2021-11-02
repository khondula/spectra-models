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
