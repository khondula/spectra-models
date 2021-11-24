my_flightline <- flightlines[1]

process_flightline_refl <- function(my_flightline){
  
  # split up flightline info
  my_flightline_datestring <- str_split(my_flightline, "_", simplify = TRUE)[1]
  my_flightline_date <- lubridate::as_date(my_flightline_datestring)
  my_fl <- str_split(my_flightline, "_", simplify = TRUE)[2]
  my_aop_yr <- lubridate::year(my_flightline_date)
  # read in L1 refl vals
  spectra_df <- grep(my_flightline, spectra_files[my_spectra_fileIDs], value = TRUE) %>% vroom::vroom()
  # find and read in L1 metadata
  my_meta_fileIDs <- str_detect(spectra_meta_files, glue::glue('{mysite}.*({my_aop_yr})'))
  meta_df <- grep(my_flightline, spectra_meta_files[my_meta_fileIDs], value = TRUE) %>% vroom::vroom()
  # read in radiance metadata
  my_rad_fileIDs <- str_detect(rad_files, glue::glue('{mysite}.*({my_flightline_datestring})'))
  radmeta_df <- rad_files[my_rad_fileIDs] %>% vroom() %>% dplyr::filter(nmdLctn %in% my_loc)
  
  # identify metadata of interest
  my_loc_check <- any(my_loc %in% meta_df$nmdLctn)
  if(!my_loc_check){message(glue('{my_loc} not in {my_flightline}'))}
  if(my_loc_check){
    # should be the same for buoy c0 and c1
    my_meta <- meta_df %>% dplyr::filter(nmdLctn %in% my_loc)
    my_theta <- my_meta[['solar_zenith']] %>% unique()
    my_clouds <- my_meta[['clouds']] %>% unique()
    my_loc_type <- my_meta[['loctype']] %>% unique()
    my_cellrow <- my_meta[['cellrow']] %>% unique() # need to align with radiance metadata
    my_cellcol <- my_meta[['cellcol']] %>% unique() # need to align with radiance metadata
  
  # check for data
  any_vals_check <- any(spectra_df[[my_loc_type[1]]] != -9999)
  if(!any_vals_check){message(glue('No data for {my_flightline}'))}
  if(any_vals_check){
    my_rad_df <- radmeta_df %>% dplyr::filter(cellrow %in% my_cellrow & cellcol %in% my_cellcol)
    my_gpstime_hrs <- my_rad_df[['gpstime_hrs']] %>% unique()
    
    my_datetime_utc = lubridate::as_datetime(glue('{my_flightline_date} {hms::hms(hours = my_gpstime_hrs)}'))
    # select spectra from desired location and interpolate to 1 nm resolution with approx
    loc_in_spectra <- my_loc_type[my_loc_type %in% names(spectra_df)][1]
    my_spectra_df <- spectra_df %>% dplyr::select(all_of(loc_in_spectra), wl, band)

    # APPROX USES LINEAR INTERPOLATION
    # my_spectra_df2 <- approx(my_spectra_df$wl, my_spectra_df[[loc_in_spectra]], xout = my_wls) %>%
    #   as.data.frame() %>% dplyr::rename(wl = x, rho_approx = y) %>%
    #   dplyr::mutate(rho_approx = rho_approx/10000)

    # SPLINE INTERPOLATION
    my_spectra_df2 <- spline(my_spectra_df$wl, my_spectra_df[[loc_in_spectra]], 
                             xout = my_wls, method = "natural") %>%
      as.data.frame() %>% dplyr::rename(wl = x, rho_natspline = y) %>%
      dplyr::mutate(rho_approx = rho_natspline/10000)
    
    # my_spectra_df2 %>% 
    #   left_join(my_spectra_df3) %>% 
    #   tidyr::pivot_longer(cols = 2:3) %>%
    #   ggplot(aes(x = wl, y = value)) +
    #   geom_line(aes(col = name))
    #   # geom_point(aes(col = name)) +
    #   # facet_wrap(vars(name))
    
    wv_bands <- c(1260:1560, 1760:1960) # water vapor bands to remove
    
    gg1 <- my_spectra_df %>%
      dplyr::mutate(wlint = round(wl)) %>%
      dplyr::filter(!wlint %in% wv_bands) %>%
      dplyr::rename(rho_w = 1) %>%
      ggplot(aes(x = wl, y = rho_w/10000)) +
      geom_rect(aes(xmin = 1260, xmax = 1560, ymin = -Inf, ymax = Inf), fill = 'gray', alpha = 0.10) +
      geom_rect(aes(xmin = 1760, xmax = 1960, ymin = -Inf, ymax = Inf), fill = 'gray', alpha = 0.10) +
      geom_point(pch = 21) +
      geom_line(data = my_spectra_df2, aes(y = rho_approx), col = 'blue') +
      coord_cartesian(ylim = c(0, NA)) +
      theme_bw() +
      ggtitle(glue('Interpolated reflectance to 1nm for 400-800 nm, wv bands masked,
              {mysite} {my_aop_yr} at {loc_in_spectra}
              {my_flightline_date}, line {my_fl}, {my_clouds}'))
    
    my_theta_rad = (my_theta*pi)/180
    
    g0 = 0.089
    g1 = 0.1245
    # MODEL REMOTE SENSING REFLECTANCE AND UNDERWATER
    my_spectra_df3 <- my_spectra_df2 %>%
      dplyr::mutate(Rrs1 = rho_approx/pi) %>%
      dplyr::mutate(Rrs2 = rho_approx * (cos(my_theta_rad)/pi)) %>%
      dplyr::mutate(rrs1 = Rrs1/(0.52 + 1.7*Rrs1)) %>%
      dplyr::mutate(rrs2 = Rrs2/(0.52 + 1.7*Rrs2)) %>%
      dplyr::mutate(u1 = (-g0 + sqrt((g0^2) + 4*g1 * rrs1))/2*g1) %>%
      dplyr::mutate(u2 = (-g0 + sqrt((g0^2) + 4*g1 * rrs2))/2*g1) %>%
      dplyr::mutate(my_theta = my_theta,
                    my_clouds = my_clouds, 
                    my_gpstime_hrs = my_gpstime_hrs)
    
    gg2 <- my_spectra_df3 %>%
      ggplot(aes(x = wl, y = rho_approx)) +
      geom_line() +
      geom_line(aes(y = Rrs1), col = 'blue', lty = 2) + # dashed - no angle effect
      geom_line(aes(y = Rrs2), col = 'blue', lty = 1) +
      geom_line(aes(y = rrs1), col = 'purple', lty = 2) + # purple = underwater
      geom_line(aes(y = rrs2), col = 'purple', lty = 1) +
      theme_bw() +
      coord_cartesian(ylim = c(0, NA)) +
      ggtitle(glue('{mysite} {my_aop_yr} at {loc_in_spectra}
                     {my_flightline_date}, line {my_fl}, {my_clouds}
                     theta_z = {round(my_theta, 1)} deg
                     {my_datetime_utc} UTC'))
    
    # gg3 <- cowplot::plot_grid(gg1, gg2)
    return(list(plot1 = gg1, plot2 = gg2, data = my_spectra_df3))
  }
  }}

