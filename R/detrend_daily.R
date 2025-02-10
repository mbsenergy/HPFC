
#' Decompose the history spot price into Macro trend and detrended
#'
#' The Macro trend is derived using HP filter
#'
#' @param x A daily dataframe with price
#' @returns A dataframe with 4 columns date, smp_day, hp_trend and detr_smp_day
#' @import data.table
#' @export

detrend_daily = function(dataframe){

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('smp_day' %in% colnames(dataframe)) | class(dataframe$smp_day) != 'numeric') {stop("smp_day column must be format numeric")}


  filtered_dam_dd = copy(dataframe)

  min_date = as.Date(min(filtered_dam_dd$date))

  df_dam_dd_ts = ts(filtered_dam_dd$smp_day,
                    start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
                    frequency = 365)


  #add HP trend to dataframe daily
  hp_filter2 = mFilter::hpfilter(df_dam_dd_ts,
                                 freq = 8.322 * 10 ^ 7,  ### 8.322 * 10 ^ 7
                                 type = "lambda",
                                 drift = F) ### lambda as in Caldana et al (2017)

  filtered_dam_dd[, hp_trend := hp_filter2$trend]

  # subtract hp_trend from raw data
  filtered_dam_dd[, detr_smp_day := smp_day - hp_trend]

  filtered_dam_dd = filtered_dam_dd[, .(date, smp_day, hp_trend, detr_smp_day)]

  return(filtered_dam_dd)

}
