
#' Decompose the history spot price into Macro trend and detrended
#'
#' The Macro trend is derived using HP filter
#'
#' @param x A daily dataframe with price
#' @returns A dataframe with 4 columns date, trade_close, hp_trend and detr_smp_day
#' @import data.table
#' @export

detrend_daily_gas = function(dataframe) {

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('trade_close' %in% colnames(dataframe)) | class(dataframe$trade_close) != 'numeric') {stop("trade_close column must be format numeric")}


  filtered_ttf_dd=copy(dataframe)

  min_date = as.Date(min(filtered_ttf_dd$date))

  df_ttf_dd_ts = ts(filtered_ttf_dd$trade_close,
                    start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
                    frequency = 365)


  #add HP trend to dataframe daily
  hp_filter2 = mFilter::hpfilter(df_ttf_dd_ts,
                                 freq = 8.322 * 10 ^ 7,  ### 8.322 * 10 ^ 7
                                 type = "lambda",
                                 drift = F) ### lambda as in Caldana et al (2017)

  filtered_ttf_dd[, hp_trend := hp_filter2$trend]

  # subtract hp_trend from raw data
  filtered_ttf_dd[, detr_smp_day := trade_close - hp_trend]

  filtered_ttf_dd = filtered_ttf_dd[, .(date, trade_close, hp_trend, detr_smp_day, break_group_p)]

  return(filtered_ttf_dd)

}
