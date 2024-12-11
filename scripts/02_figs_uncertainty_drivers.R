# Purpose--create 5 simple figure for use in presentation
# they should schematically represent sources of uncertainty
# in ecological projections/forecasts

# Author: Martin Holdrege

# Script Started: 12/6/2024



# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/figure_functions.R')
theme_set(theme_custom1())


# params --------------------------------------------------------------
v <- 'v1' # append to figure file names

# read in data ------------------------------------------------------------

rr1 <- read_csv('data_processed/newRR_metrics_mean_1980-2099.csv',
                show_col_types = FALSE)

scd1 <- read_csv('data_processed/SEI_area-by-class_2001-2100.csv')

# fig functions/params -----------------------------------------------------------

png2 = function(filename, ...) {
  png(filename, width = 3, height = 3, units = 'in',
       res = 600, bg = 'transparent')
}

xlim1 <- c(1980, 2090)

# ecological uncertainty figs ---------------------------------------------

# Uncertainty in ecological response
# show area of core sagebrush (or other class) and then projected
# for mid and end of century, for with and without fire

# * data ------------------------------------------------------------------

units <- '(million ha)'
lookup_ylab <- c('CSA' = paste('Core Sagebrush Area', units),
                 'GOA' = paste('Growth Opportunity Area', units),
                 'CSAGOA' = paste('CSA + GOA', units))

classes <- names(lookup_ylab)

runs <- c("fire1_eind1_c4grass1_co20_2311", "fire0_eind1_c4grass1_co20")

p <- expand_grid(run = runs,
                 class = classes)
scd2 <- scd1 %>% 
  mutate(across(matches('area'), .fns = \(x) x/10e6)) # convert to millions ha

for(i in 1:nrow(p)) {
  class <- p$class[i]
  run <- p$run[i]
  obs <- scd2 %>% 
    filter(RCP == 'Historical',
           class == !!class)
  proj <- scd2 %>%  
    filter(RCP != 'Historical',
           class == !!class,
           run == !!run)
  
  g <- create_timeseries_fig2(obs = obs, proj = proj,
                              y = 'area', y_med = 'area_med', y_low = 'area_lo', 
                              y_hi = 'area_hi')
  
  g2 <- g+
    coord_cartesian(xlim = xlim1) +
    expand_limits(y = 0) +
    labs(y = lookup_ylab[class]) 
  
  png2(paste0('figures/timeseries/scd/', class, '_area_', run, '_', v, '.png'))
  g2
  dev.off()
  
}



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
