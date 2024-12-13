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
scd_version <- 'v1' # v1 or v3 (v1 = Doherty et al. 2022)

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
  filter(RCP != 'Historical' | scd_version == !!scd_version) %>% 
  mutate(across(matches('area'), .fns = \(x) x/1e6)) # convert to millions ha



units <- '(million ha)'
lookup_ylab <- c('CSA' = paste('Core Sagebrush Area', units),
                 'GOA' = paste('Growth Opportunity Area', units),
                 'CSAGOA' = paste('CSA + GOA', units))

classes <- names(lookup_ylab)

runs <- c("fire1_eind1_c4grass1_co20_2311", "fire0_eind1_c4grass1_co20",
          "fire1_eind1_c4grass0_co20_2311")

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
  
  # keeping limits the same for different runs, for comparability
  ylims <- scd2 %>% 
    filter(class == !!class,
           run %in% runs) %>% 
    select(matches('area')) %>% 
    pivot_longer(everything()) %>% 
    pull('value') %>% 
    range(na.rm = TRUE)
  
  g <- create_timeseries_fig2(obs = obs, proj = proj,
                              y = 'area', y_med = 'area_med', y_low = 'area_lo', 
                              y_hi = 'area_hi',
                              # don't add best fit line
                              model = FALSE)
  
  g2 <- g+
    coord_cartesian(xlim = xlim1) +
    expand_limits(y = ylims) +
    labs(y = lookup_ylab[class]) 
  g2
  
  png2(paste0('figures/timeseries/scd/', class, '_area_SEI', scd_version
              ,'_', run, '_', v, '.png'))
  print(g2)
  dev.off()
  
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
