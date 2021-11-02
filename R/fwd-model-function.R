
make_fwd_model <- function(chla_ugL, 
                           cdom_ref,
                           cdom_slope,
                           nap_ref,
                           nap_slope,
                           nw_ref,
                           nw_gamma,
                           theta_deg,
                           wlCDOM_ref = 440,
                           wlNAP_ref = 440,
                           wlNW_ref = 550){
  
  # inputs
  my_wls = 400:800
  water_df <- readr::read_csv('data/water-approx1nm.csv', col_types = 'ddd')
  phyStar_df <- readr::read_csv('data/gege-wasi-spectra.csv', col_types = 'ddddddd')

  # ABSORBANCE
  abs_water <- water_df %>% dplyr::filter(wl %in% my_wls) %>% 
    dplyr::select(wl, absorb_m1) %>% dplyr::rename(abs_w = absorb_m1)
  
  # phyto
  phy_df <- phyStar_df %>% 
    dplyr::rename(wl = nm) %>%
    dplyr::filter(wl %in% my_wls) %>% 
    dplyr::mutate(abs_phy = chla_ugL*phyto_mixA) %>%
    dplyr::select(wl, abs_phy)
  
  # CDOM
    abs_cdom <- my_wls %>% purrr::map_dbl(~cdom_ref * exp(-1*cdom_slope * (.x - wlCDOM_ref)))
    cdom_df <- data.frame(wl = my_wls, abs_cdom)
    
  # Non algal particles
    abs_nap <- my_wls %>% purrr::map_dbl(~nap_ref * exp(-1*nap_slope * (.x - wlNAP_ref)))
    nap_df <- data.frame(wl = my_wls, abs_nap)
    
    abs_tot <- abs_water %>% 
      dplyr::left_join(cdom_df) %>%
      dplyr::left_join(nap_df) %>%
      dplyr::left_join(phy_df) %>%
      dplyr::mutate(abs_tot = abs_w + abs_cdom + abs_nap + abs_phy)
    
  # BACKSCATTER
    
    backs_water <- water_df %>% dplyr::filter(wl %in% my_wls) %>% 
      dplyr::select(wl, backs_m1) %>% dplyr::rename(backs_w = backs_m1)
    
    nw_part <- my_wls %>% purrr::map_dbl(~nw_ref * (wlNW_ref/(.x))^(-1*nw_gamma))
    nw_df <- data.frame(wl = my_wls, nw_part)
    
    backs_tot <- backs_water %>% 
      dplyr::left_join(nw_df) %>%
      dplyr::mutate(backs_tot = backs_w + nw_part)
    
    # reflectance
    g1 = 0.0949
    g2 = 0.0794
    theta_rad = (theta_deg*pi)/180
      
    refl_df <- abs_tot %>% 
      left_join(backs_tot) %>%
      dplyr::mutate(u1 = backs_tot/(abs_tot + backs_tot)) %>%
      dplyr::mutate(r_rs = g1*u1 + g2*u1^2) %>%
      dplyr::mutate(Rrs = (-0.52*r_rs)/(1.7*r_rs-1)) %>%
      dplyr::mutate(rho1 = Rrs * pi) %>%
      dplyr::mutate(rho2 = Rrs * (pi/cos(theta_rad)))
    
    r1 <- refl_df %>%
      ggplot(aes(x = wl, y = r_rs)) +
      geom_line(aes(y = Rrs), col = 'blue', lty = 1) +
      geom_line(aes(y = rho1), col = 'black', lty = 1) +
      geom_line(aes(y = rho2), col = 'purple', lty = 2) +
      geom_line(col = 'blue', lty = 2) +
      theme_bw() +
      ylim(c(0, NA)) +
      ylab("Reflectance") +
      ggtitle(glue::glue('Modeled reflectance spectra
                         chla = {chla_ugL} ug/L, CDOM = {cdom_ref}, S = {cdom_slope}, NAP = {nap_ref}, S = {nap_slope}, 
                         NWbb = {nw_ref}, theta = {theta_deg} deg,
                         abs ref = {wlCDOM_ref}nm, bb ref = {wlNW_ref}nm'))
    
    return(list(r1, refl_df))
    
}
