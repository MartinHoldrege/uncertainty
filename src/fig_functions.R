# functions for figure creation


# dependencies ------------------------------------------------------------

source('src/dataframes.R')
source('src/fig_params.R') # for color vectors

# theme -------------------------------------------------------------------

theme_custom1 <- function() {
  theme_bw() %+replace%
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          strip.background = element_blank())
}


# colors/fill -------------------------------------------------------------

fill_color_rcp = function() {
  list(scale_color_manual(values = cols_rcp),
       scale_fill_manual(values = cols_rcp)
  )
}

# timeseries figs ---------------------------------------------------------


base_timeseries <- function() {
  list(
    fill_color_rcp(),
    labs(x = 'Year'),
    theme(legend.position = 'none')
  )
}

create_timeseries_fig <- function(obs, proj1, proj2, y, y_med, y_low, y_hi,
                                  model = TRUE) {
  
  g1 <-  ggplot(mapping = aes(x = year)) +
    geom_vline(xintercept = max(obs$year), color = 'gray') +
    geom_line(data = obs, aes(y = .data[[y]]))
  
  if(model ) {
    g1 <- g1 + geom_smooth(data = obs, aes(y = .data[[y]]), se = FALSE,
                      color = 'gray', linetype = 2, method = 'lm',
                      formula = 'y~x')
  }
  
  g1 +
    geom_point(data = obs, aes(y = .data[[y]])) +
    geom_ribbon(data = proj2, aes(ymin = .data[[y_low]], 
                                  ymax = .data[[y_hi]], fill = RCP),
                alpha = 0.2) +
    geom_point(data = proj1, aes(y = .data[[y_med]], color = RCP)) +
    geom_line(data = proj2, aes(y = .data[[y_med]], color = RCP)) +
    base_timeseries()
} 

#' timeseries figure
#'
#' @param obs dataframe of observed timeseries
#' @param proj dataframe of projected timeseries
#' @param y name of the y value in obs
#' @param y_med name of the variable providing the median estimate in proj
#' @param y_low name of the variable providing the low estimate in proj
#' @param y_hi name of the variable providing the high estimate in proj
#'
#' @return
#' ggplot figure, where observed timeseries is shown,
#' and projected values with uncertainty (starting at the end of the best
#' fit line throught the observed data)
#'
#' @examples
create_timeseries_fig2 = function(obs, proj, y, y_med, y_low, y_hi,
                                  model = TRUE) {
  proj2 <- create_timeseries_df(proj = proj, obs = obs,
                                y = y, y_med = y_med, y_low = y_low,
                                y_hi = y_hi, model = model)
  
  create_timeseries_fig(obs = obs, proj1 = proj, proj2 = proj2, 
                        y = y, y_med = y_med, y_low = y_low,
                        y_hi = y_hi, model = model)
}

