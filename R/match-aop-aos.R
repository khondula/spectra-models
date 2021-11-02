

match_aop_aos <- function(aop_dates, aos_df, aos_date_col = 'collect_date', thresh1_days = 30, thresh2_days = 60){
  
  my_aos_dates <- unique(aos_df[[aos_date_col]])
  
  get_bordering_dates <- function(aop_date){
    
    before_date <- NA
    if(any(which(my_aos_dates <= aop_date))){
      before_date_id <- max(which(my_aos_dates <= aop_date))
      before_date <- my_aos_dates[before_date_id]
    }
    after_date <- NA
    if(any(which(my_aos_dates >= aop_date))){
      after_date_id <- min(which(my_aos_dates >= aop_date))
      after_date <- my_aos_dates[after_date_id]
    }
    
    return(data.frame(aos_before = before_date,
                      aos_after = after_date))
  }
  
  dates_df <- aop_dates %>% 
    purrr::map_dfr(~get_bordering_dates(.x)) %>% 
    mutate(flightdate = aop_dates) %>%
    dplyr::select(flightdate, aos_before, aos_after) %>% 
    dplyr::mutate(days_before = difftime(flightdate, aos_before, units = 'days'),
                  days_after = difftime(aos_after, flightdate, units = 'days')) %>%
    dplyr::mutate(days_before = as.numeric(days_before),
                  days_after = as.numeric(days_after)) %>%
    rowwise() %>%
    dplyr::mutate(min_days = min(days_before, days_after, na.rm = TRUE)) %>%
    dplyr::mutate(meets_thresh1 = min_days <= thresh1_days) %>%
    dplyr::mutate(meets_thresh2 = min_days <= thresh2_days)
  
  # actual data
  
  match_df <- dates_df %>%
    dplyr::filter(meets_thresh2) %>%
    tidyr::pivot_longer(cols = c(days_before, days_after)) %>%
    group_by(flightdate) %>%
    arrange(flightdate, value) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(aos_match = dplyr::case_when(name == 'days_before' ~ aos_before,
                                               name == 'days_after' ~ aos_after)) %>%
    dplyr::select(flightdate, aos_match, min_days) %>%
    dplyr::rename(days = min_days)
  
  data_df <- match_df %>%
    dplyr::left_join(aos_df, by = c('aos_match' = aos_date_col))
  
  return(list(dates = dates_df, matches = match_df, data = data_df))
  
}
