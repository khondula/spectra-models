# install.packages('colorSpec')
# library(colorSpec)
# range(cie1nm$Wavelength)
# range(cie5nm$Wavelength)
cie1nm <- colorSpec::xyz1931.1nm %>% as.data.frame() %>% 
  dplyr::rename(wl = Wavelength) %>% dplyr::filter(wl %in% 400:800)
# cie5nm <- colorSpec::xyz1931.5nm %>% as.data.frame() %>% dplyr::rename(wl = Wavelength)
# cie5m_long <- cie5nm %>% tidyr::pivot_longer(cols = c(2:4))
# sum(cie1nm$x)
# sum(cie5nm$x)
# cie1nm %>%
#   tidyr::pivot_longer(cols = c(2:4)) %>%
#   ggplot(aes(x = Wavelength, y = value)) +
#     geom_point(aes(col = name)) +
#   geom_point(data = cie5m_long, pch = 21, aes(fill = name), size = 2) +
#   ggtitle('CIE color scales')
# forel ule color scale stuff
  # fu_color_scale <- c(FU1 = 'blue', FU2 = 'blue', FU3 = 'dodgerblue', FU4 = 'cyan',
  #                   FU5 = 'cyan', FU6 = 'darkcyan', FU7 = 'darkcyan', FU8 = 'darkcyan',
  #                   FU9 = 'darkcyan', FU10 = 'green', FU11 = 'green', FU12 = 'green3',
  #                   FU13 = 'green3', FU14 = 'green4', FU15 = 'olivedrab', 
  #                   FU16 = 'olivedrab', FU17 = 'olivedrab', FU18 = 'goldenrod', 
  #                   FU19 = 'goldenrod2', FU20 = 'goldenrod3', FU21 = 'goldenrod4')
  
  fu_color_scale <- c(FU01 = '#2156BC', FU02 = '#316CC6', FU03 = '#317DBB', FU04 = '#4B80A0',
                      FU05 = '#568F9A', FU06 = '#6E9399', FU07 = '#698C86', FU08 = '#789F72',
                      FU09 = '#7BA752', FU10 = '#7DAE38', FU11 = '#95B645', FU12 = '#94B660',
                      FU13 = '#A5BC76', FU14 = '#AAB86D', FU15 = '#ADB55F', 
                      FU16 = '#A8A965', FU17 = '#B09E5C', FU18 = '#B2A054', 
                      FU19 = '#AD8B43', FU20 = '#A36802', FU21 = '#A34A04')

cie <- readr::read_csv('~/Box/data/CIE1nm.csv', col_names = c('wl', 'x', 'y', 'z')) %>% 
  dplyr::filter(wl %in% 400:800)

# cie5nm <- readr::read_csv('cie-xyz.csv')
# cie5nm %>% tidyr::pivot_longer(cols = 2:4) %>%
#   ggplot(aes(x = wl, y = value, col = name)) +
#   geom_line() +
#   geom_point()

# units::set_units(pi, 'degrees')
fuLUT <- readr::read_csv('~/Box/data/FU-scale.csv') %>%
  dplyr::mutate(Fuclass = str_remove(Fuclass, "-"))

# fuLUT %>%
#   ggplot(aes(x = xNovoa, y = yNovoa)) +
#   geom_hline(aes(yintercept = 0.33)) +
#   geom_vline(aes(xintercept = 0.33)) +
#   # geom_point(aes(x = xWang, y = yWang, fill = Fuclass),
#   #                pch = 22, size = 4) +
#   geom_point(pch = 21, size = 4, aes(fill = Fuclass)) +
#   scale_fill_manual(values = fu_color_scale) +
#   geom_point(aes(x = 0.33, y = 0.33), pch = 23, fill = 'white', size = 3) +
#   ggtitle("Forel Ule colors")

# fuLUT_mod <- fuLUT %>%
#   dplyr::mutate(xW = xWang-0.33, yW = yWang-0.33) %>%
#   dplyr::mutate(alpha_rad2 = atan2(yW, xW)) %>%
#   dplyr::mutate(alpha_rad2u = units::set_units(alpha_rad2, 'radians'))
# 
# fuLUT_mod[['alpha_deg']] <- purrr::map_dbl(fuLUT_mod$alpha_rad2u, ~units::set_units(.x, 'degrees'))

# spectra_df <- spectra_list[[1]] %>%
#   dplyr::filter(name == 'rho_approx', flightline_id == unique(spectra_list[[1]]$flightline_id)[1])

calculate_FU <- function(spectra_df, id_col = 'flightline_id'){
  check_neg <- any(spectra_df$value < 0)
  if(check_neg){message(glue('negative values in spectra {spectra_df$flightline_id[1]} corrected to 0'))}
  
  spectra_df <- spectra_df %>% 
    dplyr::mutate(value = case_when(value < 0 ~ 0, TRUE ~ value))
  # calculate X Y Z
  fx_linear <- approxfun(spectra_df[['wl']], cie1nm[['x']]*spectra_df[['value']])
  myx_integral <- integrate(fx_linear, lower = 400, upper = 800, subdivisions = 401, stop.on.error = FALSE)
  my_X = myx_integral$value
  
  fy_linear <- approxfun(spectra_df$wl, cie1nm[['y']]*spectra_df$value)
  myy_integral <- integrate(fy_linear, lower = 400, upper = 800, subdivisions = 401, stop.on.error = FALSE)
  my_Y = myy_integral$value
  
  fz_linear <- approxfun(spectra_df$wl, cie1nm[['z']]*spectra_df$value)
  myz_integral <- integrate(fz_linear, lower = 400, upper = 800, subdivisions = 401, stop.on.error = FALSE)
  my_Z = myz_integral$value
  
  my_sum = my_X + my_Y + my_Z
  my_x = my_X/my_sum
  my_y = my_Y/my_sum
  
  my_xW = my_x - (1/3)
  my_yW = my_y - (1/3)
  
  my_alpha_rad = base::atan(my_yW/my_xW)
  if(my_xW >= 0){my_alpha_deg = my_alpha_rad * (180/pi)}
  if(my_xW < 0){my_alpha_deg = 180 + my_alpha_rad * (180/pi)}
  # if (my_xW >= 0) {my_alpha_deg = my_alpha_rad*180/pi}
  # if (my_xW < 0) {my_alpha_deg = 180 + my_alpha_rad*180/pi}
  # min angle from look up table that my_alpha_deg is greater than
  # smallest number that my angle is greater than

  my_diffs <- my_alpha_deg - fuLUT[['alphaTNovoa']]
  my_diffs[my_diffs<0] <- NA
  fuLUT_id <- which.min(is.na(my_diffs)) # first false value for logical vector
  
  if(my_alpha_deg < min(fuLUT[['alphaTNovoa']], na.rm = TRUE)){fuLUT_id = 21}
  # based on lookup table for FU classes
  wl_dominant <- fuLUT[['dominant_wl']][fuLUT_id]
  fu_class <- fuLUT[['Fuclass']][fuLUT_id]
  
  return(list(id = spectra_df[[id_col]][1], 
              hue_angle = my_alpha_deg, 
              fu_class = fu_class, 
              wl_dominant = wl_dominant))
}


get_allflightlines_FUs <- function(spectras_df, flightlines){ 
  
  fu_df <- flightlines %>%
    purrr::map(~dplyr::filter(spectras_df, flightline_id %in% .x)) %>%
    purrr::map(~calculate_FU(spectra_df = .x)) %>%
    purrr::map_df(~as.data.frame(.x)) %>%
    dplyr::bind_rows()
  
  return(fu_df)
}