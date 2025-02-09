
#' Detect the breaks in the spot price time series
#'
#' The function use changepoint algorithm ...
#'
#' @param x A dataframe.
#' @param y Price column name.
#' @returns A dataframe with 4 columns date, hour, price and break period
#' @export

break_detection = function(dataframe, smp_name){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('hour' %in% colnames(dataframe)) | class(dataframe$hour) != 'numeric') {stop("hour column must be format numeric")
  } else if (!(smp_name %in% colnames(dataframe)) | class(dataframe[,.(get(smp_name))][[1]]) != 'numeric') {stop("spot price column must be format numeric") }

  if (nrow(dataframe)<365*24*2){stop('a minimum of 2 year history is required') }

  # warning se ci sono NA su smp

  df_dam = copy(dataframe)
  set(df_dam, , names(df_dam)[!names(df_dam) %in% c('date', 'hour', smp_name)], NULL)
  setnames(df_dam, colnames(df_dam), c('date', 'hour', 'smp'))

  min_date = as.Date(min(df_dam$date))
  setorderv(df_dam, cols = c('date'), order = 1L)
  df_dam[, smp := nafill(smp, 'locf')]

  #### create deviation from daily mean

  df_dam[, smp_day := mean(smp), by = 'date']
  df_dam[, smp_h := smp - smp_day]

  #### create time series of h deviation
  df_dam_ddhh_ts_detr = ts(df_dam$smp_h,
                           start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
                           frequency = 365 * 24)

  #### start from n of breaks = to n years, check if there is a period with less than 3 months in the last 5 years,
  #### if not stops, otherwise consider 1 break less

  for (j in 1:round(length(df_dam_ddhh_ts_detr) / (365 * 24))) {

    n_breaks = 1 + round(length(df_dam_ddhh_ts_detr) / (365 * 24)) - j

    Vvalue_h = suppressWarnings(changepoint::cpt.var(df_dam_ddhh_ts_detr, Q = n_breaks, method = 'BinSeg'))

    cutoff = length(df_dam_ddhh_ts_detr) - ((365 * 24) * 5)

    vec = c(0, changepoint::cpts(Vvalue_h)[changepoint::cpts(Vvalue_h) > cutoff], length(df_dam_ddhh_ts_detr))

    difs = diff(vec)
    if (!any(24 * 90 > difs)){ break }}

  #### create variable with period group
  discv_date_h = df_dam$date[changepoint::cpts(Vvalue_h)]
  df_dam[,break_group_p:=findInterval(date,discv_date_h)]

  df_dam = df_dam[, .(date, hour, smp, break_group_p)]
  setnames(df_dam, colnames(df_dam), c('date', 'hour', smp_name, 'break_group_p'))

  return(df_dam)

}
