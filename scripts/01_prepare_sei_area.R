# Purpose: Prepare dataframe(s) of historical and projected area 
# of sagebrush ecological integrity classes for use time-series figures

# Author: Martin Holdrege

# Script Started: December 11, 2024


# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/general_functions.R')
source('src/fig_functions.R')
theme_set(theme_custom1())


# params ------------------------------------------------------------------

compare_versions <- TRUE # create figure comparing SCD versions

# read in files -----------------------------------------------------------

# area in each SEI class from 2001 to 2021 based on Theobald's SCD v30
# assets
# area calculated in the SEI/scripts/uncertainty/area_by_SEIclass_and_year.js
# script
area1 <- read_csv("data_raw/scd/SEIv30_area-by-class_2001_2021.csv")

# projections of area
# this csv contains the results that are in tables
# in the Appendix D of Holdrege et al. 2024
c9_proj1 <- read_csv("../SEI/data_processed/summary_stats/area-by-c9_summaries_vsw4-3-4.csv")

# prepare historical dataframe --------------------------------------------
# historical area by SEI class
area2 <- area1 %>% 
  mutate(c3 = code2c3(Q5sc3),
         scd_version = 'v3') %>% 
  select(-`system:index`, -.geo, -file, -Q5sc3)

area3 <- area2 %>% 
  pivot_wider(values_from = 'area_ha',
              names_from = 'c3') %>% 
  mutate(CSAGOA = CSA + GOA) %>% # total core and grow area
  select(-ORA) %>% 
  pivot_longer(cols = -c(year, scd_version),
               values_to = 'area',
               names_to = 'class') %>% 
  mutate(RCP = 'Historical')


# historical data from v11 ----------------------------------------------------

# numbers from fig 5 in doherty et al 2022
v11a <- tibble(
  c3 = rep(c('CSA', 'GOA', 'ORA'), each = 5),
  # units are millions of acres
  area = c(53.8, 45.0, 49.2, 42.2, 33.4,
           90.7, 90.0, 88.4, 81.9, 84.3,
           100.4, 109.9, 107.3, 120.8, 127.2),
  year = rep(c(2001, 2006, 2011, 2016, 2020), 3),
  scd_version = 'v1'
)

v11b <- v11a
v11b$area <- v11a$area*0.404686 # convert to millions ha
v11b$RCP <- 'Historical'

v11c <- v11b %>% 
  mutate(area = area*1e6,
         units = 'ha')

v11d <- v11c %>% 
  pivot_wider(values_from = 'area',
              names_from = 'c3') %>% 
  mutate(CSAGOA = CSA + GOA) %>% 
  select(-ORA) %>% 
  pivot_longer(cols = c('CSA', 'GOA', 'CSAGOA'),
               values_to = 'area',
               names_to = 'class')
  

# prepare projected dataframe ---------------------------------------------

proj0 <- c9_proj1 %>% 
  filter(units != 'perc') %>% 
  mutate(across(.cols = matches('( becomes )|(Stable)'),
                .fns = as.character)) %>% 
  pivot_longer(cols = matches('( becomes )|(Stable)'),
               values_to = 'area') %>% 
  mutate(area = str_replace(area, "<", '0'),# replacing near 0 values with 0 
         area = as.numeric(area)*1000, # convert to ha
         # future (projected) SEI class
         c3 = case_when(
           str_detect(name, 'CSA$') ~ 'CSA',
           str_detect(name, 'GOA$') ~ 'GOA',
           str_detect(name, 'ORA$') ~ 'ORA'
         )) %>% 
  select(-units)

proj1 <- proj0 %>% 
  # combining areas of future c3 class
  group_by(run, RCP,years, summary, c3) %>% 
  summarize(area = sum(area),
            .groups = 'drop') %>% 
  pivot_wider(id_cols = c('run', 'RCP', 'years','summary'),
              values_from = 'area',
              names_from = 'c3'
              ) %>% 
  mutate('CSAGOA' = CSA + GOA,
         year = epoch2year(years)) %>% 
  select(-ORA) %>% 
  pivot_longer(cols = c('CSA', 'GOA', 'CSAGOA'),
               values_to = 'area',
               names_to = 'class') %>% 
  pivot_wider(names_from = c('summary'),
              values_from = 'area',
              names_prefix = 'area_')

# combine output ----------------------------------------------------------

comb <- bind_rows(area3, proj1, v11d) %>% 
  mutate(units = 'ha')

# save files --------------------------------------------------------------

write_csv(comb, 'data_processed/SEI_area-by-class_2001-2100.csv')

# compare versions --------------------------------------------------------
if (compare_versions) {
  
  combv <- area2 %>% # combine versions
    mutate(area = area_ha/1e6,
           c3 = as.character(c3)) %>% 
    select(-area_ha) %>% 
    bind_rows(v11b)
  
  # total area
  tot <- combv %>% 
    group_by(year, scd_version) %>% 
    summarize(area = sum(area)) 

  
  # difference in area between the two versions is trivial
  # (good news!), and note that the v11 numbers used here are rounded from the report
  ggplot(tot, aes(x = area, fill = scd_version)) +
    geom_histogram()
  (max(tot$area) - min(tot$area))/min(tot$area)*100 # less than 0.02% different
  
  # total area of climate projections
  tot_proj0 <- proj0 %>% 
    filter(run == unique(run)[1],
           RCP == unique(RCP)[1],
           years == unique(years)[1],
           summary == 'med') %>% 
    mutate(area = area/1e6) 
    
  tot_proj <- tot_proj0 %>% 
    pull(area) %>% 
    sum() 
  
  stopifnot(max(abs(tot_proj-tot$area))< 0.1) # very similar total area 
  
  # 2020 core calculated from projected areas (which are relative to v1, 2020)
  csa2020proj <- tot_proj0 %>% 
    # core in 2020
    filter(name == 'Stable CSA' | str_detect(name, '^CSA')) %>% 
    pull(area) %>% 
    sum()
  
  csa2020v1 <- v11b %>% 
    filter(c3 == 'CSA', year == 2020) %>% 
    pull(area)
  stopifnot(abs(csa2020proj - csa2020v1) < 0.1) # want these to match (within rounding)
  
  png('figures/timeseries/SEIClass_area_comparing_versions_2001-2021.png',
      width = 7.5, height = 3.5, units = 'in', res = 600)
  print(ggplot(combv, aes(x = year, y = area, color = scd_version)) +
    geom_line() +
    geom_point() +
    facet_wrap(~c3) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
    labs(y = 'Area (millions ha)',
         subtitle = 'Comparing SCD data versions (v1 = Doherty et al 2022)'))
  dev.off()
}
