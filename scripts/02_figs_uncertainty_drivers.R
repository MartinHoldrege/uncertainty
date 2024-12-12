# Purpose--create 5 simple figure for use in presentation
# they should schematically represent sources of uncertainty
# in ecological projections/forecasts

# Author: Martin Holdrege

# Script Started: 12/6/2024



# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/fig_functions.R')
theme_set(theme_custom1())

# params --------------------------------------------------------------

v <- 'v1' # append to figure file names

# read in data ------------------------------------------------------------

rr1 <- read_csv('data_processed/newRR_metrics_mean_1980-2099.csv',
                show_col_types = FALSE)

scd1 <- read_csv('data_processed/SEI_area-by-class_2001-2100.csv')

# fig functions/params -----------------------------------------------------------

png2 = function(filename, ...) {
  png(filename, width = 3.5, height = 3, units = 'in',
       res = 600, bg = 'transparent')
}

xlim1 <- c(1980, 2090)

# ecological uncertainty figs ---------------------------------------------

# Uncertainty in ecological response
# show area of core sagebrush (or other class) and then projected
# for mid and end of century, for with and without fire
scd2 <- scd1 %>% 
  mutate(across(matches('area'), .fns = \(x) x/1e6)) # convert to millions ha
SEIv11_area_mha = c(53.8, 45.0, 49.0, 42.2, 33.4)*0.404686 # from theobald et al 2022 #area of core

SEIv30_area_mha <- scd2 %>% 
  filter(RCP == 'Historical',
         class == 'CSA', 
         year %in% (c(1, 6, 11, 16, 20) + 2000)) %>% 
  arrange(year) %>% 
  pull(area)

# the problem is that v30 and v11 areas are notably inconsistent from
# each other
plot(SEIv11_area_mha, SEIv30_area_mha, 
     xlab = 'CSA area (as per Doherty et al. 2022) (million ha)',
     ylab = 'CSA area (as per SEI v30) (million ha)')
abline(0, 1)


units <- '(million ha)'
lookup_ylab <- c('CSA' = paste('Core Sagebrush Area', units),
                 'GOA' = paste('Growth Opportunity Area', units),
                 'CSAGOA' = paste('CSA + GOA', units))

classes <- names(lookup_ylab)

runs <- c("fire1_eind1_c4grass1_co20_2311", "fire0_eind1_c4grass1_co20")

p <- expand_grid(run = runs,
                 class = classes)


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
  g2
  
  # png2(paste0('figures/timeseries/scd/', class, '_area_', run, '_', v, '.png'))
  # g2
  # dev.off()
  
}

# climate figures -----------------------------------------------------------

# Uncertainty in climate response
rr_obs <- rr1 %>% 
  filter(RCP == 'Historical')

rr_proj1 <- rr1 %>% 
  filter(RCP != 'Historical') 
  
g <- create_timeseries_fig2(obs = rr_obs, proj = rr_proj1,
                        y = 'Tmean', y_med = 'Tmean_med', y_low = 'Tmean_low', 
                        y_hi = 'Tmean_hi')
g2 <- g+
  coord_cartesian(xlim = xlim1) +
  labs(y = 'Mean Temperature') 

png2(paste0('figures/timeseries/climate/tmean_', v, '.png'))
g2
dev.off()

# driver figures ----------------------------------------------------------

# uncertainty in ecological driver

g <- create_timeseries_fig2(obs = rr_obs, proj = rr_proj1,
                            y = 'DDD', y_med = 'DDD_med', y_low = 'DDD_low', 
                            y_hi = 'DDD_hi')
g2 <- g+
  coord_cartesian(xlim = xlim1) +
  labs(y = 'Dry Degree Days') 

png2(paste0('figures/timeseries/driver/DDD_', v, '.png'))
g2
dev.off()
