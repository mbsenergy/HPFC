
#' Detect the breaks in the spot price time series
#'
#' The function use changepoint algorithm ...
#'
#' @param x A dataframe.
#' @param y Price column name.
#' @returns A dataframe with 4 columns date, price and break period
#' @export

break_detection_gas = function(dataframe, price_name){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!(price_name %in% colnames(dataframe)) | class(dataframe[,.(get(price_name))][[1]]) != 'numeric') {stop("spot price column must be format numeric") }

  if (nrow(dataframe) < 365 * 2) { stop('a minimum of 2 year history is required') }

  # warning se ci sono NA su smp
  df_ttf = copy(dataframe)
  set(df_ttf, , names(df_ttf)[!names(df_ttf) %in% c('date', price_name)], NULL)
  setnames(df_ttf, colnames(df_ttf), c('date', 'trade_close'))

  min_date = as.Date(min(df_ttf$date))

  setorderv(df_ttf, cols = c('date'), order = 1L)
  df_ttf[, trade_close := nafill(trade_close, 'locf')]
  df_ttf[, trade_close := nafill(trade_close, 'nocb')]


  #### create time series of day price
  df_ttf_ts_detr = ts(df_ttf$trade_close,
                      start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
                      frequency = 365)

  #### start from n of breaks = to n years, check if there is a period with less than 3 months in the last 5 years,
  #### if not stops, otherwise consider 1 break less

  for (j in 1:round(length(df_ttf_ts_detr) / (365))){

    n_breaks = 1 + round(length(df_ttf_ts_detr) / (365)) - j

    Vvalue_h = suppressWarnings(changepoint::cpt.var(df_ttf_ts_detr, Q = n_breaks, method = 'BinSeg'))

    cutoff = length(df_ttf_ts_detr)-((365)*5)

    vec = c(0, changepoint::cpts(Vvalue_h)[changepoint::cpts(Vvalue_h) > cutoff], length(df_ttf_ts_detr))

    difs = diff(vec)
    if (!any(24 * 90 > difs)) { break } }

  #### create variable with period group
  discv_date_h = df_ttf$date[changepoint::cpts(Vvalue_h)]

  df_ttf[,break_group_p := findInterval(date,discv_date_h)]
  df_ttf = df_ttf[, .(date, trade_close, break_group_p)]

  return(df_ttf)

}
