# Purpose: Prepare dataframe(s) of historical and projected area 
# of sagebrush ecological integrity classes for use time-series figures

# Author: Martin Holdrege

# Script Started: December 11, 2024


# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/general_functions.R')

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
  mutate(c3 = code2c3(Q5sc3)) %>% 
  select(-`system:index`, -.geo, -file, -Q5sc3)

area3 <- area2 %>% 
  pivot_wider(values_from = 'area_ha',
              names_from = 'c3') %>% 
  mutate(CSAGOA = CSA + GOA) %>% # total core and grow area
  select(-ORA) %>% 
  pivot_longer(cols = -year,
               values_to = 'area',
               names_to = 'class') %>% 
  mutate(RCP = 'Historical')


# prepare projected dataframe ---------------------------------------------

proj1 <- c9_proj1 %>% 
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

comb <- bind_rows(area3, proj1) %>% 
  mutate(units = 'ha')

# save files --------------------------------------------------------------

write_csv(comb, 'data_processed/SEI_area-by-class_2001-2100.csv')
