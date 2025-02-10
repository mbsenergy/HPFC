
#' Create regressors in the hourly dataframe to estimate the model
#'
#' creates calendar variable, break dummy and bl polynomial
#'
#' @param x A hourly dataframe with price produced by filter outlier hourly
#' @returns A dataframe with 49 columns
#' @import data.table
#' @export

hourly_regressors=function(dataframe){

  # check formati

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
  } else if (!('hour' %in% colnames(dataframe)) | class(dataframe$hour) != 'numeric') {stop("hour column must be format numeric")
  } else if (!('smp' %in% colnames(dataframe)) | class(dataframe$smp) != 'numeric') {stop("smp column must be format numeric")
  } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }

  filtered_dam_ddhh=copy(dataframe)

  #add hp_trend to hourly DT
  #filtered_dam_ddhh = filtered_dam_dd[, .(date, hp_trend, weight)][filtered_dam_ddhh, on = "date"]


  # subtract hp_trend from raw data
  #filtered_dam_ddhh[, detr_smp := smp - as.numeric(hp_trend)]

  #create dummy for monthly, quarterly, weekend seasonality
  filtered_dam_ddhh[,`:=` (yday = data.table::yday(date),
                           wday = data.table::wday(date),
                           quarter = data.table::quarter(date),
                           month = data.table::month(date),
                           weekend = as.numeric(chron::is.weekend(date)),
                           obs = .I)]


  # filtered_dam_ddhh[,`:=`(weekend_q1 = as.numeric(weekend == 1 & quarter == 1),
  #                         weekend_q4 = as.numeric(weekend == 1 & quarter == 4),
  #                         weekend_q2 = as.numeric(weekend == 1 & quarter == 2),
  #                         weekend_q3 = as.numeric(weekend == 1 & quarter == 3),
  #                         work_day_q1 = as.numeric(weekend == 0 & quarter == 1),
  #                         work_day_q4 = as.numeric(weekend == 0 & quarter == 4),
  #                         work_day_q2 = as.numeric(weekend == 0 & quarter == 2),
  #                         work_day_q3 = as.numeric(weekend == 0 & quarter == 3))]

  #filtered_dam_ddhh = filtered_dam_dd[, .(date, detr_smp_day)][filtered_dam_ddhh, on = 'date']


  ## Hourly Dummies ---------------------------------------------------------------------------------------------

  ####create dummy for weekday
  filtered_dam_ddhh[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]

  ####create dummy for hours
  filtered_dam_ddhh[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]

  #### create baseload
  filtered_dam_ddhh[, bl := mean(smp), by=date]

  #### create last market regime period
  filtered_dam_ddhh[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]

  filtered_dam_ddhh[,smp_h:=smp-bl]

  filtered_dam_ddhh[,yday2:=yday^2]
  filtered_dam_ddhh[,yday3:=yday^3]
  filtered_dam_ddhh[,bl2:=bl^2]
  filtered_dam_ddhh[,bl3:=bl^3]


  return(filtered_dam_ddhh)

}

#
# filtered_dam_ddhh=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "filtered_dam_ddhh.rds"))
#
# hourly_regressors=function(dataframe){
#
#   # check formati
#
#   if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")
#   } else if (!('hour' %in% colnames(dataframe)) | class(dataframe$hour) != 'numeric') {stop("hour column must be format numeric")
#   } else if (!('smp' %in% colnames(dataframe)) | class(dataframe$smp) != 'numeric') {stop("smp column must be format numeric")
#   } else if (!('break_group_p' %in% colnames(dataframe)) | class(dataframe$break_group_p) != 'integer') {stop("break_group_p column must be format integer") }
#
#   filtered_dam_ddhh=copy(dataframe)
#
#   #add hp_trend to hourly DT
#   #filtered_dam_ddhh = filtered_dam_dd[, .(date, hp_trend, weight)][filtered_dam_ddhh, on = "date"]
#
#
#   # subtract hp_trend from raw data
#   #filtered_dam_ddhh[, detr_smp := smp - as.numeric(hp_trend)]
#
#   #create dummy for monthly, quarterly, weekend seasonality
#   filtered_dam_ddhh[,`:=` (yday = data.table::yday(date),
#                            wday = data.table::wday(date),
#                            quarter = data.table::quarter(date),
#                            month = data.table::month(date),
#                            weekend = as.numeric(chron::is.weekend(date)),
#                            obs = .I)]
#
#
#   # filtered_dam_ddhh[,`:=`(weekend_q1 = as.numeric(weekend == 1 & quarter == 1),
#   #                         weekend_q4 = as.numeric(weekend == 1 & quarter == 4),
#   #                         weekend_q2 = as.numeric(weekend == 1 & quarter == 2),
#   #                         weekend_q3 = as.numeric(weekend == 1 & quarter == 3),
#   #                         work_day_q1 = as.numeric(weekend == 0 & quarter == 1),
#   #                         work_day_q4 = as.numeric(weekend == 0 & quarter == 4),
#   #                         work_day_q2 = as.numeric(weekend == 0 & quarter == 2),
#   #                         work_day_q3 = as.numeric(weekend == 0 & quarter == 3))]
#
#   #filtered_dam_ddhh = filtered_dam_dd[, .(date, detr_smp_day)][filtered_dam_ddhh, on = 'date']
#
#
#   ## Hourly Dummies ---------------------------------------------------------------------------------------------
#
#   ####create dummy for weekday
#   filtered_dam_ddhh[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
#
#   ####create dummy for hours
#   filtered_dam_ddhh[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
#
#   #### create baseload
#   filtered_dam_ddhh[, bl := mean(smp), by=date]
#
#   #### create last market regime period
#   filtered_dam_ddhh[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]
#
#   filtered_dam_ddhh[,smp_h:=smp-bl]
#
#   filtered_dam_ddhh[,yday2:=yday^2]
#   filtered_dam_ddhh[,yday3:=yday^3]
#   filtered_dam_ddhh[,bl2:=bl^2]
#   filtered_dam_ddhh[,bl3:=bl^3]
#
#
#   return(filtered_dam_ddhh)
#
# }
#
# p6=hourly_regressors(filtered_dam_ddhh)
# saveRDS(p6, file = file.path('..', 'HPFC','data', 'data_package', "filtered_dam_ddhh_wreg.rds"))
