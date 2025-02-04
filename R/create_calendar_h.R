
#' Creates hourly calendar regressor to calibrate the model in the future
#'
#' The regrossrs are cos, sin, yday, weekend ...
#'
#' @param x A daily dataframe calendar with date and long term seasonality.
#' @returns A hourly dataframe with 17 columns
#' @export


create_calendar_h=function(dataframe){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")}
  if (!('L_t' %in% colnames(dataframe)) | class(dataframe$L_t) != 'numeric') {stop("L_t column must be format numeric")}

  calendar=copy(dataframe)

  ### generate hourly calendar
  calendar_hourly = do.call("rbind", replicate(24, calendar, simplify = FALSE))[order(date)]
  calendar_hourly[, hour := rep(rep(1:24), nrow(calendar))]

  #### create weekend/holiday
  calendar_hourly[, weekend_h := fifelse(holiday == 1, 1, weekend)]
  calendar_hourly[, quarter := paste('Q', quarter, sep = '')]

  #### create ddhh for plot
  calendar_hourly[, ddhh := paste(date, hour, sep = "H")]

  calendar_hourly=calendar_hourly[order(date,hour)]

  kc_cols(calendar_hourly, c('date', 'hour', 'ddhh', 'period', 'weekend', 'quarter', 'holiday', 'month', 'year', 'yday', 'L_t', 'spot_forward_month_BL', 'spot_forward_month_PL', 'trade_close', 'trade_close2', 'forward_month_BL', 'forward_quarter_BL', 'BL_prev_m', 'history_forecast'))

  return(calendar_hourly)

}

#
# dd_Lt_day=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "dd_Lt_day.rds"))
#
# create_calendar_h=function(dataframe){
#
#   if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")}
#   if (!('L_t' %in% colnames(dataframe)) | class(dataframe$L_t) != 'numeric') {stop("L_t column must be format numeric")}
#
#   calendar=copy(dataframe)
#
#   ### generate hourly calendar
#   calendar_hourly = do.call("rbind", replicate(24, calendar, simplify = FALSE))[order(date)]
#   calendar_hourly[, hour := rep(rep(1:24), nrow(calendar))]
#
#   #### create weekend/holiday
#   calendar_hourly[, weekend_h := fifelse(holiday == 1, 1, weekend)]
#   calendar_hourly[, quarter := paste('Q', quarter, sep = '')]
#
#   #### create ddhh for plot
#   calendar_hourly[, ddhh := paste(date, hour, sep = "H")]
#
#   calendar_hourly=calendar_hourly[order(date,hour)]
#
#   kc_cols(calendar_hourly, c('date', 'hour', 'ddhh', 'period', 'weekend', 'quarter', 'holiday', 'month', 'year', 'yday', 'L_t', 'spot_forward_month_BL', 'spot_forward_month_PL', 'trade_close', 'trade_close2', 'forward_month_BL', 'forward_quarter_BL', 'BL_prev_m', 'history_forecast'))
#
#   return(calendar_hourly)
#
# }
#
# f4=create_calendar_h(dd_Lt_day)
# saveRDS(f4, file = file.path('..', 'HPFC','data', 'data_package', "calendar_hourly.rds"))
