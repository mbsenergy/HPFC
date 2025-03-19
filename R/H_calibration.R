
#' Calibrate hourly seasonality on future calendar
#'
#' The calibration is done using the nls esimated with hourly model function ...
#'
#' @param x An hourly dataframe calendar with date, long term seasonality and forward structure.
#' @param y The hourly model estimated as nls object
#' @param y The modality of calibration: can be 'forecast' if forward are used or 'backtest' if month average spot price are used. default is forecast
#' @returns A hourly dataframe with 15 columns to be used for hourly estimation
#' @import data.table
#' @export

H_calibration=function(DT, model_h){

  DTW = copy(DT)

  #### explicit epsilon u as in Caldana
  DTW[, epsilon_u := spot_forward_month_BL]

  DTW[, break_h := 1]
  DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
  DTW[, yday2 := yday^2]
  DTW[, yday3 := yday^3]
  DTW[, value_gas2 := value_gas^2]

  DTW[,bl := epsilon_u + L_t]

  DTW[, bl2 := bl^2]
  DTW[, bl3 := bl^3]

  pred_hourly = predict(model_h, DTW)
  DTW[, prediction_h := pred_hourly]
  DTW[, L_u := pred_hourly + L_t]
  DTW[, L_e_u := pred_hourly + L_t + epsilon_u]

  return(DTW)

}

#
# calendar_hourly_adjusted=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "calendar_hourly_adjusted.rds"))
#
# model_hourly=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "hourly_model.rds"))
#
# H_calibration=function(dataframe,model_h){
#
#   if (!('spot_forward_month_BL' %in% colnames(dataframe)) | class(dataframe$spot_forward_month_BL) != 'numeric') {stop("spot_forward_month_BL column must be format numeric")}
#   if (!('yday' %in% colnames(dataframe)) | class(dataframe$yday) != 'integer') {stop("yday column must be format integer")}
#   if (!('trade_close' %in% colnames(dataframe)) | class(dataframe$trade_close) != 'numeric') {stop("trade_close column must be format numeric")}
#
#   if (class(model_h) != 'nls') {stop("model_h must be format nls")}
#
#   DTW=copy(dataframe)
#
#   #### explicit epsilon u as in Caldana
#   DTW[, epsilon_u := spot_forward_month_BL]
#
#   DTW[, break_h := 1]
#   DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
#   DTW[, yday2 := yday^2]
#   DTW[, yday3 := yday^3]
#   DTW[, trade_close2 := trade_close^2]
#
#   DTW[,bl:=epsilon_u+L_t]
#   DTW[, bl2 := bl^2]
#   DTW[, bl3 := bl^3]
#
#   pred_hourly=predict(model_h, DTW)
#   DTW[, prediction_h := pred_hourly]
#   DTW[, L_u := pred_hourly + L_t]
#   DTW[, L_e_u := pred_hourly + L_t + epsilon_u]
#
#   return(DTW)
#
# }
#
#
# f6=H_calibration(calendar_hourly_adjusted,model_hourly)
# saveRDS(f6, file = file.path('..', 'HPFC','data', 'data_package', "dd_Leu.rds"))
