
#' Filter the hourly outlier from the spot time series with breaks
#'
#' The outlier eliminated have 3 std dev greater than the mean ...
#'
#' @param x A hourly dataframe with breaks.
#' @param y Price column name.
#' @returns A dataframe with 5 columns date, hour, price and break period and ddhh
#' @export

filter_outlier_hourly = function(dataframe,smp_name) {

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('hour' %in% colnames(dataframe)) | class(dataframe$hour) != 'numeric') {stop("hour column must be format numeric")
  } else if (!(smp_name %in% colnames(dataframe)) | class(dataframe[,.(get(smp_name))][[1]]) != 'numeric') {stop("spot price column must be format numeric")
  } else if (!('out_dummy' %in% colnames(dataframe)) | class(dataframe$out_dummy) != 'numeric') {stop("out dummy column must be format numeric")
  } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }

  filtered1_dam_ddhh = copy(dataframe)
  setnames(filtered1_dam_ddhh, smp_name, 'smp')

  #drop outlier with dummy = 0
  filtered1_dam_ddhh[, ddhh := paste(date, hour, sep = "H")]


  #filtered1_dam_ddhh = df_dam[out_dummy == 0, ]
  filtered1_dam_ddhh[,smp_h := smp - smp_day]
  filtered1_dam_ddhh[, we := chron::is.weekend(date)]
  filtered1_dam_ddhh[, mean_h := mean(smp_h), by = c('break_group_p', 'hour', 'we')]

  filtered1_dam_ddhh[, delta_h := smp_h - (mean_h)]
  filtered1_dam_ddhh[, sd_delta_h := sd(delta_h), by = c('break_group_p', 'hour', 'we')]

  filtered1_dam_ddhh[, out_dummy := as.numeric(abs(delta_h) > 3 * sd_delta_h)]
  filtered1_dam_ddhh[, out_values := fifelse(out_dummy == 1, smp_h, NA_real_)]

  filtered1_dam_ddhh[, out_perday := sum(out_dummy), by = date]
  filtered1_dam_ddhh[, out_dummy := fifelse(out_perday > 4, 1, out_dummy)]

  filtered_dam_ddhh = filtered1_dam_ddhh[out_dummy == 0, .(date, hour, ddhh, smp, break_group_p)]

  return(filtered_dam_ddhh)

}
