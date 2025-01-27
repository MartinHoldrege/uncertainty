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

png2 = function(filename, width = 3.5, height = 3,...) {
  png(filename, width = width, height = height, units = 'in',
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

runs <- c("Default" = "fire1_eind1_c4grass1_co20_2311", 
          "No fire" = "fire0_eind1_c4grass1_co20",
          "No C4 grass exp." = "fire1_eind1_c4grass0_co20_2311",
          "CO2 fertilization" = "fire1_eind1_c4grass1_co21_2311")

runs_lookup <- names(runs)
names(runs_lookup) <- runs
scd2$run_name <- runs_lookup[scd2$run]

p <- expand_grid(run = runs,
                 class = classes)

# keeping limits the same for different runs, for comparability
ylims <- map(classes, function(class) {
    tmp <- scd2 %>% 
    filter(run %in% runs, class == !!class) %>% 
    select(matches('area')) %>% 
    pivot_longer(everything())
  
    # limits for plots that don't show cones of uncertainty
    tmp2 <- tmp %>% 
    filter(!str_detect(name, '_hi|_lo')) 

  list(full = range(tmp$value, na.rm = TRUE),
      med_only = range(tmp2$value, na.rm = TRUE))
} )
names(ylims) <- classes

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
                              y_hi = 'area_hi',
                              # don't add best fit line
                              model = FALSE)
  
  g2 <- g+
    coord_cartesian(xlim = xlim1) +
    expand_limits(y = ylims[[class]]$full) +
    labs(y = lookup_ylab[class]) 
  g2
  
  png2(paste0('figures/timeseries/scd/', class, '_area_SEI', scd_version
              ,'_', run, '_', v, '.png'))
  print(g2)
  dev.off()
  
}


# * compare runs ----------------------------------------------------------

# comparisons against the default

# list element names will go into file name
runcomps <- list("fire01_eind1_c4grass1_co20_2311" = c(runs['Default'],
                                                      runs['No fire']),
                 "fire1_eind1_c4grass01_co20_2311" = c(runs['Default'],
                                                       runs['No C4 grass exp.']),
                 "fire1_eind1_c4grass1_co201_2311" = c(runs['Default'],
                                                       runs['CO2 fertilization']))


for(comp in names(runcomps)) {
  for(class in classes) {
    obs <- scd2 %>% 
      filter(RCP == 'Historical',
             class == !!class)
    proj1 <- scd2 %>%  
      filter(RCP != 'Historical',
             class == !!class,
             run %in% runcomps[[comp]])
    
    proj2 <- create_timeseries_df(proj = proj1, obs = obs,
                                  y = 'area', y_med = 'area_med', 
                                  y_low = 'area_lo',
                                  y_hi = 'area_hi', model = FALSE)
    
    proj2$run_name <- factor(proj2$run_name) %>% 
      fct_relevel("Default") # so default always comes first
    
    # the warning 'No shared levels found between...' seems to be benign
    g1 <- create_timeseries_fig(obs = obs,
                          proj1 = proj1,
                          proj2 = proj2,
                          y = 'area',
                          y_med = 'area_med',
                          line_var = 'run_name',
                          model = FALSE,
                          ribbon = FALSE,
                          legend.position = 'right')


    g2 <- g1 + 
      scale_linetype(name = 'Modeling assumption') +
      labs(y = lookup_ylab[class]) +
      expand_limits(y = ylims[[class]]$full, # previously used ylim[[class]]$med_only
                    x = xlim1)
      theme(
        legend.title = element_text(size = rel(0.7)),     # Legend title size
        legend.text = element_text(size = rel(0.7)),      # Legend text size
        legend.key.size = unit(0.5, "lines"),             # Legend key size
        legend.spacing.y = unit(0.2, "cm"),               # Vertical spacing
        legend.spacing.x = unit(0.2, "cm")                # Horizontal spacing
      )

    png2(paste0('figures/timeseries/scd/compare_assumptions/', class, '_area_SEI', 
                scd_version,'_', comp, '_', v, '.png'),
         width = 5.3)
    print(g2)
    dev.off()

  }

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
  labs(y = 'Mean Temperature (\u00B0C)') 

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

