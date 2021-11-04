
match_aos_sensor <- function(aos_datetimes_df, df_movavg){
  my_range <- lubridate::as.interval(range(df_movavg$datetime)[1], range(df_movavg$datetime)[2])
  my_aostimes <- aos_datetimes_df %>% dplyr::filter(collect_datetime %within% my_range)
  
  closest_ids <- my_aostimes$collect_datetime %>%
    purrr::map(~difftime(.x, df_movavg$datetime)) %>% 
    purrr::map(~abs(.x)) %>%
    purrr::map_int(~which.min(.x))
  
  df <- my_aostimes %>% bind_cols(df_movavg[closest_ids,])
  return(df)
}
# my_range <- lubridate::as.interval(range(chl_df_ma$datetime)[1], range(chl_df_ma$datetime)[2])
# my_aoptimes <- aop_datetimes_df %>% dplyr::filter(collectDateTime %within% my_range)
# 
# closest_ids <- my_aoptimes$collectDateTime %>%
#   purrr::map(~difftime(.x, chl_df_ma$datetime)) %>% 
#   purrr::map(~abs(.x)) %>%
#   purrr::map_int(~which.min(.x))
# 
# df <- my_aoptimes %>% bind_cols(chl_df_ma[closest_ids,])
