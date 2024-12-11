# purpose: summarize data from D. Schlaepfers R&R metrics/projections
# work, specifically summarizing data so that have historical and projected
# climate variable (Tmean) and one ecologically relevant variable (dry
# degree days). For figure making

# Author: Martin Holdrege

# Script started Dec. 9, 2024


# dependencies ------------------------------------------------------------

library(tidyverse)
source('src/general_functions.R')

# params ------------------------------------------------------------------

path1 <- "D:/USGS/large_files/uncertainty/data_raw/newRR3/"


# read in files ------------------------------------------------------------

# ambient values
paths_ambient <- file.path(path1, 'ambient_1980-2021_IndRuns_Values_annualmetrics',
  c("DDD__sc1_NA_ambient_1980-2021__value-sim.csv",
    "Tmean__sc1_NA_ambient_1980-2021__value-sim.csv"))

ambient_l1 <- map(paths_ambient, read_csv, show_col_types = FALSE)
names(ambient_l1) <- str_extract(paths_ambient, '[[:alpha:]]+(?=__sc1)')

# future, projected values
paths_fut <- list.files(path1, pattern = 'RCP', full.names = TRUE, recursive = TRUE) %>% 
  str_subset("(low|high|med).csv$")
names(paths_fut) <- basename(paths_fut) %>% 
  str_replace('.csv', '')

fut_l1 <- map(paths_fut, function(x) {
  read_csv(x, show_col_types = FALSE) %>% 
    select(count_rangelands, matches('(DDD_mean)|(Tmean_mean)'))
})


# functions ---------------------------------------------------------------

calc_wmean <- function(df, xvar, wvar = 'count_rangelands') {
  df <- drop_na(df)
  weighted.mean(x = df[[xvar]], w = df[[wvar]])
}

# calculated the weighted mean for each year
calc_annual_means <- function(df, var_name) {
  out <- df %>% 
    select(-count_sim) %>% 
    rename(row = value) %>% 
    pivot_longer(
      cols = matches('\\d{4}$'),
      names_to = 'year'
    ) %>% 
    mutate(year = as.numeric(str_extract(year, '\\d{4}$'))) %>% 
    group_by(year) %>% 
    nest() %>% 
    mutate(mean = map_dbl(data, calc_wmean, xvar = 'value', wvar = 'count_rangelands')) %>% 
    select(-data) %>% 
    ungroup()
  
  names(out) <- c('year', var_name)
  out
}

# summarize historical time-series ----------------------------------------

# one column per year over the historical time period (using ambient (observed)
# climate, count_rangelands provides the weight to average by (i.e. the number
# of pixels that row of data extrapolated too))

ambient1 <- map2(ambient_l1, names(ambient_l1), calc_annual_means) %>% 
  reduce(left_join, by = 'year') %>% 
  mutate(RCP = 'Historical')


# summarize projected delta values ----------------------------------------------

fut_delta1 <- bind_rows(fut_l1, .id = 'file') %>% 
  mutate(RCP = str_extract(file, 'RCP\\d{2}'),
         years = str_extract(file, '\\d{4}\\-\\d{4}'),
         summary = str_extract(file, 'low|high|med')) %>% 
  rename(Tmean_delta = Tmean_mean_delta_acrmoddistr,
         DDD_delta = DDD_mean_delta_acrmoddistr) %>% 
  select(-file)

fut_delta2 <- fut_delta1 %>% 
  group_by(RCP, years, summary) %>% 
  nest() %>% 
  summarize(Tmean_delta = map_dbl(data, calc_wmean, xvar = 'Tmean_delta'),
            DDD_delta = map_dbl(data, calc_wmean, xvar = 'DDD_delta'),
            .groups = 'drop') 

# calculate future values -------------------------------------------------

ambient_avg <- ambient1 %>% 
  filter(year >= 1980 & year <=2010) %>% 
  summarize(DDD_mean = mean(DDD),
            Tmean_mean = mean(Tmean))

fut1 <- fut_delta2

# calculating the future value has the ambient normal + the delta
fut1$DDD <- fut1$DDD_delta + ambient_avg$DDD_mean
fut1$Tmean <- fut1$Tmean_delta + ambient_avg$Tmean_mean

fut2 <- fut1 %>% 
  select(-matches('delta')) %>% 
  mutate(year = epoch2year(years)) %>% 
  pivot_wider(values_from = c('DDD', 'Tmean'),
              names_from = 'summary') %>% 
  rename_with(.fn = function(x) {
    str_replace(x, 'high', 'hi')
  })


# combine outputs ---------------------------------------------------------

comb1 <- bind_rows(fut2, ambient1)


# write files -------------------------------------------------------------

write_csv(comb1, 'data_processed/newRR_metrics_mean_1980-2099.csv')

         