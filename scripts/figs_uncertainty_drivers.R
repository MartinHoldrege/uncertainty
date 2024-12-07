# Purpose--create 5 simple figure for use in presentation
# they should schematically represent sources of uncertainty
# in ecological projections/forecasts

# Author: Martin Holdrege

# Script Started: 12/6/2024



# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/figure_functions.R')
theme_set(theme_custom1())


# fig params --------------------------------------------------------------

# * colors ----------------------------------------------------------------

cols_rcp <- c('RCP45' = '#3288bd', 'RCP85' = '#d53e4f')




# fig functions -----------------------------------------------------------

fill_color_rcp = function() {
  list(scale_color_manual(values = cols_rcp),
       scale_fill_manual(values = cols_rcp)
  )
}

jpeg2 = function(filename, ...) {
  jpeg(filename, width = 3, height = 3, units = 'in',
       res = 600, bg = 'transparent')
}

# fig 4 -------------------------------------------------------------------

# Uncertainty in ecological response
# show area of core sagebrush (observed 2000-2021) and then projected
# for mid and end of century

# * data ------------------------------------------------------------------

# millions of acres of core sagebrush area (CSA) from Doherty et al 2022 
# (https://doi.org/10.3133/ofr20221081) (figure 5)
area_mac = c(53.8, 45.0, 49.0, 42.2, 33.4)
area_mha = area_mac*0.404686 # convert to millions of ha
 
# observed area of CSA
csa_obs <- tibble(
  year = c(2001, 2006, 2011, 2016, 2020),
  area = area_mha
)

# projected area of CSA from Holdrege et al 2024 (REM)
# Numbers from table D.1

csa_proj0 <- tibble(
  year = c(2045, 2045, 2085, 2085), # using mid-points of 30 yr time-periods
  RCP = c('RCP45', 'RCP85', 'RCP45', 'RCP85'),
  area_low = c(8206, 7420, 5643, 1807),
  area_med = c(10781, 9803, 8979, 5838),
  area_hi = c(13006, 12225, 12348, 10396)
)

csa_proj1 <- csa_proj0 %>% 
  mutate(area_low = area_low/1000, # convert to millions of ha
         area_med = area_med/1000,
         area_hi = area_hi/1000)

row <- csa_obs %>% 
  filter(year == max(year)) %>% 
  rename(area_med = area) %>% 
  mutate(area_low = area_med,
         area_hi = area_med)

# adding in dummy rows so that the projections
# can start at current values
csa_proj2 <- bind_rows(row, row) %>% 
  mutate(RCP = c('RCP45', 'RCP85')) %>% 
  bind_rows(csa_proj1)

# * figure ----------------------------------------------------------------

g <- ggplot(mapping = aes(x = year)) +
  geom_vline(xintercept = max(csa_obs$year), color = 'gray') +
  geom_line(data = csa_obs, aes(y = area)) +
  geom_point(data = csa_obs, aes(y = area)) +
  geom_ribbon(data = csa_proj2, aes(ymin = area_low, 
                                    ymax = area_hi, fill = RCP),
              alpha = 0.2) +
  geom_point(data = csa_proj1, aes(y = area_med, color = RCP)) +
  geom_line(data = csa_proj2, aes(y = area_med, color = RCP)) +

  coord_cartesian(ylim = c(0, max(csa_obs$area))) +
  labs(x = 'Year',
       y = 'Core Sagebrush Area (millions ha)') +
  theme(legend.position = 'none') +
  fill_color_rcp()

jpeg2('figures/fig4_ecological-uncertainty_v1.jpg')
g
dev.off()





