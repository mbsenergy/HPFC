
#' Creates hourly calendar regressor to calibrate the model in the future
#'
#' The regrossrs are cos, sin, yday, weekend ...
#'
#' @param x A daily dataframe calendar with date and long term seasonality.
#' @returns A hourly dataframe with 17 columns
#' @import data.table
#' @export


create_calendar_h = function(dataframe){

  calendar=copy(dataframe)

  ### generate hourly calendar
  calendar_hourly = do.call("rbind", replicate(24, calendar, simplify = FALSE))[order(date)]
  calendar_hourly[, hour := rep(rep(1:24), nrow(calendar))]

  #### create weekend/holiday
  calendar_hourly[, weekend_h := fifelse(holiday == 1, 1, weekend)]
  calendar_hourly[, quarter := paste('Q', quarter, sep = '')]

  #### create ddhh for plot
  calendar_hourly[, ddhh := paste(date, hour, sep = "H")]

  calendar_hourly = calendar_hourly[order(date,hour)]
  
  cols_vec = c("date", "hour", "ddhh", "period", "value_gas",
               "weekend", "quarter", "holiday", "month", "year", "yday",
               'season', 'season_winter', 'season_spring', 'season_summer', 'season_fall',
               "L_t", "spot_forward_month_BL", "spot_forward_month_PL",
               "value_day", "value_day2", "forward_month_BL", "forward_quarter_BL",
               "BL_prev_m", "history_forecast")
  
  set(calendar_hourly, , names(calendar_hourly)[!names(calendar_hourly) %in% cols_vec], NULL)
  # calendar_hourly = calendar_hourly[, .(date, hour, ddhh, period, weekend, quarter, holiday, month, year, yday, L_t, spot_forward_month_BL, spot_forward_month_PL, trade_close, trade_close2, forward_month_BL, forward_quarter_BL, BL_prev_m, history_forecast)]

  return(calendar_hourly)

}
