
#' Filter the daily outlier from the spot time series with breaks
#'
#' The outlier eliminated have 3 std dev greater than the mean ...
#'
#' @param x A hourly dataframe with breaks.
#' @returns A dataframe with 3 columns date, price, breaks
#' @import data.table
#' @export

filter_outlier_daily_gas = function(dataframe){

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('trade_close' %in% colnames(dataframe)) | class(dataframe$trade_close) != 'numeric') {stop("trade_close column must be format integer")
  } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }

  df_ttf = copy(dataframe)

  setnames(df_ttf, colnames(df_ttf), c('date', 'trade_close', 'break_group_p'))


  df_ttf_ham = df_ttf[, .(date, trade_close)]


  ## HAMILTON FILTER
  hamilton_filter = neverhpfilter::yth_filter(xts::as.xts(df_ttf_ham), h = 24, p = 4, output = c("x", "trend", "cycle"))
  dt_hamilton_filter = as.data.table(hamilton_filter)
  setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
  dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
  dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
  dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]


  ## add hp trend

  df_ttf[, hp_trend := dt_hamilton_filter$trend]

  # find diff over 3 std dev
  df_ttf[, delta := trade_close - (hp_trend)]
  df_ttf[, sd_delta := sd(delta), by=break_group_p]

  df_ttf[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
  df_ttf[, out_values := fifelse(out_dummy == 1, trade_close, NA_real_)]

  filtered_ttf_dd = df_ttf[out_dummy == 0, .(date, trade_close, out_dummy, break_group_p)]

  return(filtered_ttf_dd)

}
