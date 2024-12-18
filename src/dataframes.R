# functions for preparing dataframes


# for time series ---------------------------------------------------------

#' create datafraome of projected values, to start where
#' observed values end
#'
#' @param proj dataframe of projected values
#' @param obs dataframe of observed values
#' @param y name of the y value in obs
#' @param y_med name of the variable providing the median estimate in proj
#' @param y_low name of the variable providing the low estimate in proj
#' @param y_hi name of the variable providing the high estimate in proj
#'
#' @return the proj dataframe but with additional dummy rows (for plotting)
#' added. These provide values at the final year in the observed
#' data, at the level predicted by a linear model through the observed data
create_timeseries_df <- function(proj, obs, y, y_med, y_low, y_hi,
                                 model = TRUE) {
  
  stopifnot(
    c('year', y) %in% names(obs),
    c('year', y_med, y_low, y_hi, 'RCP') %in% names(proj)
  )
  
  newdata <- obs[obs$year == max(obs$year), ]
  if (model) {
    form <- as.formula(paste0(y, ' ~ year'))
    m <- lm(form, data = obs);
    yhat <- predict(m, newdata = newdata)
  } else {
    # just use the last observation as the 'starting' point
    # the projected values (instead of the linear prediction)
    yhat <- newdata[[y]]
  }

  # prediction for the last year in the observed timeseries
  
  stopifnot(length(yhat) == 1)
  
  col_names <- c('RCP', 'class', 'run', 'run_name') # possible grouping columns
  col_names <- col_names[col_names%in% names(proj)] # only include those actually present
  rows <- distinct(proj[col_names])
  rows$year <- max(obs$year)
  rows[[y_med]] <- yhat
  rows[[y_low]] <- yhat
  rows[[y_hi]] <- yhat
  rows$comment <- 'dummy row'
  out <- bind_rows(rows, proj)
  out
}
