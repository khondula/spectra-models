# LEE QUASI ANALYTICAL ALGORITHM
calcQAAv6 <- function(fl_data_long, flightline_id){
  
  qaa_bands <- c(412, 443, 490, 555, 670)
  Rrs670_thresh = 0.0015
  ref_wl = 555
  h0 = -1.146
  h1 = -1.366
  h2 = -0.469
  aw555 = 0.06385
  bbw555 = 0.000700  
  aw412 = 0.0069
  aw443 = 0.0107
  aw670 = 0.41220
  bbw412 = 0.002350
  bbw443 = 0.001725
  bbw670 = 0.000300
  
  sz_df <- data.frame(name = c('rrs1', 'rrs2', 'u1', 'u2'),
                      bdrf = c('no_theta', 'wtheta', 'no_theta', 'wtheta'))
  
  df <- fl_data_long %>% 
    dplyr::filter(wl %in% qaa_bands) %>% 
    dplyr::filter(flightline_id %in% my_fl_id)
  
  check_ref_df <- df %>% 
    dplyr::filter(wl %in% 670, name %in% c('Rrs1', 'Rrs2')) %>%
    dplyr::mutate(refwl_670 = value < Rrs670_thresh)
  
  chk1 = check_ref_df$refwl_670
  chk2 = check_ref_df$refwl_670
  
  if(any(chk1, chk2)){message(glue('NEED TO USE 670 nm FOR REF WL'))}
  # if refwl_670 = TRUE, ref_wl = 670
  
  rrs_wide <- df %>% 
    dplyr::filter(name %in% c('rrs1', 'rrs2')) %>%
    dplyr::mutate(wl = glue::glue('rrs{wl}')) %>%
    group_by(name) %>% group_split() %>%
    purrr::map(~dplyr::select(.x, wl, name, value)) %>%
    purrr::map(~tidyr::pivot_wider(., names_from = wl, values_from = value)) %>%
    bind_rows() %>%
    left_join(sz_df, by = 'name') %>% dplyr::select(-name)
  
  u_wide <- df %>% 
    dplyr::filter(name %in% c('u1', 'u2')) %>%
    dplyr::mutate(wl = glue::glue('u{wl}')) %>%
    group_by(name) %>% group_split() %>%
    purrr::map(~dplyr::select(.x, wl, name, value)) %>%
    purrr::map(~tidyr::pivot_wider(., names_from = wl, values_from = value)) %>%
    bind_rows() %>%
    left_join(sz_df, by = 'name') %>% dplyr::select(-name)
  
  # STEP 3 - abs at reference wavelength (555 or 670)
  abs_df <- rrs_wide %>%
    dplyr::mutate(myX = log((rrs443 + rrs490)/(rrs555 + 5*(rrs670/rrs490)*rrs670))) %>%
    dplyr::mutate(abs555 = aw555 + 10^(h0 + h1*myX + h2*(myX^2)))
  
  # STEP 4 - backs at ref wl
  
  bbp_df <- u_wide %>% left_join(abs_df) %>%
    dplyr::mutate(bbp555 = ((u555 * abs555)/(1 - u555)) - bbw555)
  
  # STEP 5 - eta?? based on 443/555
  
  eta_df <- rrs_wide %>% 
    dplyr::mutate(fancyN = 2*(1 - 1.2*exp(-0.9*(rrs443/rrs555))))
  
  # STEP 6 - backs spectra
  
  bbp_df2 <- bbp_df %>% 
    left_join(eta_df) %>% dplyr::select(bdrf, bbp555, fancyN) %>%
    dplyr::mutate(bbp412 = bbp555 * (555/412)^fancyN) %>%
    dplyr::mutate(bbp443 = bbp555 * (555/443)^fancyN) %>%
    dplyr::mutate(bbp670 = bbp555 * (555/670)^fancyN)
  
  # STEP 7 - abs spectra
  
  abs_df2 <- u_wide %>% left_join(bbp_df2) %>%
    dplyr::mutate(a412 = (1 - u412)*(bbw412 + bbp412)/u412) %>%
    dplyr::mutate(a443 = (1 - u443)*(bbw443 + bbp443)/u443) %>%
    dplyr::mutate(a670 = (1 - u670)*(bbw670 + bbp670)/u670)
  
  # STEP 8
  
  sqig_df <- rrs_wide %>% 
    dplyr::mutate(squig1 = 0.74 + (0.2/(0.8 + (rrs443/rrs555)))) %>%
    dplyr::mutate(bigS = 0.015 + (0.002/(0.6 + (rrs443/rrs555)))) %>%
    dplyr::mutate(squig2 = exp(bigS*(442.5-415.5)))
  
  # STEP 9
  
  qaa_df <- abs_df2 %>% 
    left_join(sqig_df) %>%
    dplyr::mutate(adg443 = ((a412 - squig1*a443)/(squig2 - squig1)) - ((aw412 - squig1*aw443)/(squig2 - squig1))) %>%
    dplyr::mutate(adg412 = adg443 * exp(-bigS*(412-443))) %>%
    dplyr::mutate(adg670 = adg443 * exp(-bigS*(670-443))) %>%
    dplyr::mutate(aphy412 = a412 - adg412 - aw412) %>%
    dplyr::mutate(aphy443 = a443 - adg443 - aw443) %>%
    dplyr::mutate(aphy670 = a670 - adg670 - aw670)
  
  qaa_df2 <- qaa_df %>% 
    dplyr::select(bdrf, bbp412, bbp555, fancyN, a412, a443, squig1, bigS, squig2, adg443, adg412, aphy412, aphy443, aphy670) %>%
    dplyr::mutate(flightline_id = flightline_id)
  
  return(qaa_df2)
}
