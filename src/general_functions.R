# general functions used in this repository

# factors -----------------------------------------------------------------

# convert numeric code to SEI class. 
code2c3 <- function(x) {
  stopifnot(x %in% 1:3)
  factor(x, levels = 1:3,
         labels = c('CSA', 'GOA', 'ORA'))
}

# misc -------------------------------------------------------------------

# convert epoch to the midpoint year (for plotting in time-series)
epoch2year <- function(x) {
  out <- dplyr::case_when(
    x == '2031-2060' ~ 2045,
    x == '2071-2100' ~ 2085,
    x == '2029-2064' ~ 2046,
    x == '2064-2099'  ~ 2081,
    TRUE ~ NA
  )
  if(any(is.na(out))) {
    stop('some epochs not parsed')
  }
  out
}

