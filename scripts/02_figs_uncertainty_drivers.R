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


# read in data ------------------------------------------------------------

rr1 <- read_csv('data_processed/newRR_metrics_mean_1980-2099.csv',
                show_col_types = FALSE)

# fig functions/params -----------------------------------------------------------

fill_color_rcp = function() {
  list(scale_color_manual(values = cols_rcp),
       scale_fill_manual(values = cols_rcp)
  )
}

jpeg2 = function(filename, ...) {
  jpeg(filename, width = 3, height = 3, units = 'in',
       res = 600, bg = 'transparent')
}

alpha_ribbon <- 0.2
xlim1 <- c(1980, 2090)

base1 <- function() {
  list(
    fill_color_rcp(),
    labs(x = 'Year'),
    theme(legend.position = 'none')
    )
}

create_time_series <- function(obs, proj1, proj2, y, y_med, y_low, y_high) {
  ggplot(mapping = aes(x = year)) +
    geom_vline(xintercept = max(obs$year), color = 'gray') +
    geom_line(data = obs, aes(y = .data[[y]])) +
    geom_point(data = obs, aes(y = .data[[y]])) +
    geom_ribbon(data = proj2, aes(ymin = .data[[y_low]], 
                                  ymax = .data[[y_high]], fill = RCP),
                alpha = alpha_ribbon) +
    geom_point(data = proj1, aes(y = .data[[y_med]], color = RCP)) +
    geom_line(data = proj2, aes(y = .data[[y_med]], color = RCP)) +
    base1()
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

g <- create_time_series(obs = csa_obs, proj1 = csa_proj1, proj2 = csa_proj2, 
                   y = 'area', y_med = 'area_med', y_low = 'area_low', 
                   y_high = 'area_hi')
g2 <- g+
  coord_cartesian(ylim = c(0, max(csa_obs$area)),
                  xlim = xlim1) +
  labs(y = 'Core Sagebrush Area (millions ha)') 

jpeg2('figures/fig4_ecological-uncertainty_v1.jpg')
g2
dev.off()

# fig 2 -------------------------------------------------------------------

# Uncertainty in climate response
rr_obs <- rr1 %>% 
  filter(RCP == 'Historical')

rr_proj2 <- rr1 %>% 
  filter(RCP != 'Historical') 

rr_proj1 <- rr1 %>% 
  filter(RCP != 'Historical', is.na(comment)) 
  


g <- create_time_series(obs = rr_obs, proj1 = rr_proj1, proj2 = rr_proj2, 
                        y = 'Tmean', y_med = 'Tmean_med', y_low = 'Tmean_low', 
                        y_high = 'Tmean_high')
g2 <- g+
  coord_cartesian(xlim = xlim1) +
  labs(y = 'Mean Temperature') 

jpeg2('figures/fig2_climate-uncertainty_v1.jpg')
g2
dev.off()

# fig 3 -------------------------------------------------------------------

# Uncertainty ecological response

g <- create_time_series(obs = rr_obs, proj1 = rr_proj1, proj2 = rr_proj2, 
                        y = 'DDD', y_med = 'DDD_med', y_low = 'DDD_low', 
                        y_high = 'DDD_high')
g2 <- g+
  coord_cartesian(xlim = xlim1) +
  labs(y = 'Dry Degree Days') 
g2
jpeg2('figures/fig3_ecological-driver_v1.jpg')
g2
dev.off()
