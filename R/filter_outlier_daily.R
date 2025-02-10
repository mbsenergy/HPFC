
#' Filter the daily outlier from the spot time series with breaks
#'
#' The outlier eliminated have 3 std dev greater than the mean ...
#'
#' @param x A hourly dataframe with breaks.
#' @param y Price column name.
#' @returns A dataframe with 2 columns date, price
#' @import data.table
#' @export

filter_outlier_daily = function(dataframe, smp_name) {

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('hour' %in% colnames(dataframe)) | class(dataframe$hour) != 'numeric') {stop("hour column must be format numeric")
  } else if (!(smp_name %in% colnames(dataframe)) | class(dataframe[,.(get(smp_name))][[1]]) != 'numeric') {stop("spot price column must be format numeric")
  } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }

  df_dam = copy(dataframe)

  setnames(df_dam, colnames(df_dam), c('date', 'hour', 'smp', 'break_group_p'))

  df_dam_day = df_dam[, smp_day := mean(smp, na.rm = TRUE), by = date]
  df_dam_day = df_dam_day[, .(date, smp_day)] |> unique()


  ## HAMILTON FILTER
  hamilton_filter = neverhpfilter::yth_filter(xts::as.xts(df_dam_day), h = 24, p = 4, output = c("x", "trend", "cycle"))
  dt_hamilton_filter = as.data.table(hamilton_filter)
  setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
  dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
  dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
  dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]


  df_dam_day=unique(df_dam[, .(date,break_group_p)])[df_dam_day, on = 'date']

  ## add hp trend
  df_dam_day[, hp_trend := dt_hamilton_filter$trend]

  # find diff over 3 std dev
  df_dam_day[, delta := smp_day - (hp_trend)]

  df_dam_day[, sd_delta := sd(delta), by = break_group_p]

  df_dam_day[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
  df_dam_day[, out_values := fifelse(out_dummy == 1, smp_day, NA_real_)]

  filtered_dam_dd = df_dam_day[out_dummy == 0, .(date, smp_day, out_dummy)]

  return(filtered_dam_dd)

}
