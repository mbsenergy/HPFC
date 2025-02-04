
#' Calibrate hourly seasonality on future calendar
#'
#' The calibration is done using the nls esimated with hourly model function ...
#'
#' @param x An hourly dataframe calendar with date, long term seasonality and forward structure.
#' @param y The hourly model estimated as nls object
#' @param y The modality of calibration: can be 'forecast' if forward are used or 'backtest' if month average spot price are used. default is forecast
#' @returns A hourly dataframe with 15 columns to be used for hourly estimation
#' @export

H_calibration=function(dataframe,model_h,modality){

  if (!('spot_forward_month_BL' %in% colnames(dataframe)) | class(dataframe$spot_forward_month_BL) != 'numeric') {stop("spot_forward_month_BL column must be format numeric")}
  if (!('yday' %in% colnames(dataframe)) | class(dataframe$yday) != 'integer') {stop("yday column must be format integer")}
  if (!('trade_close' %in% colnames(dataframe)) | class(dataframe$trade_close) != 'numeric') {stop("trade_close column must be format numeric")}

  if (class(model_h) != 'nls') {stop("model_h must be format nls")}

  Lt_Lu_hour=copy(dataframe)

  #### explicit epsilon u as in Caldana
  Lt_Lu_hour[, epsilon_u := spot_forward_month_BL]

  Lt_Lu_hour[, break_h := 1]
  Lt_Lu_hour[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
  Lt_Lu_hour[, yday2 := yday^2]
  Lt_Lu_hour[, yday3 := yday^3]
  Lt_Lu_hour[, trade_close2 := trade_close^2]

  if (modality=='forecast'){
  Lt_Lu_hour[,bl:=epsilon_u+L_t]
  } else if (modality=='backtest'){
  Lt_Lu_hour[,bl:=epsilon_u]
  } else if (missing(modality)){
    Lt_Lu_hour[,bl:=epsilon_u+L_t]
  }


  Lt_Lu_hour[, bl2 := bl^2]
  Lt_Lu_hour[, bl3 := bl^3]

  pred_hourly=predict(model_h, Lt_Lu_hour)
  Lt_Lu_hour[, prediction_h := pred_hourly]
  Lt_Lu_hour[, L_u := pred_hourly + L_t]
  Lt_Lu_hour[, L_e_u := pred_hourly + L_t + epsilon_u]

  return(Lt_Lu_hour)

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
#   Lt_Lu_hour=copy(dataframe)
#
#   #### explicit epsilon u as in Caldana
#   Lt_Lu_hour[, epsilon_u := spot_forward_month_BL]
#
#   Lt_Lu_hour[, break_h := 1]
#   Lt_Lu_hour[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
#   Lt_Lu_hour[, yday2 := yday^2]
#   Lt_Lu_hour[, yday3 := yday^3]
#   Lt_Lu_hour[, trade_close2 := trade_close^2]
#
#   Lt_Lu_hour[,bl:=epsilon_u+L_t]
#   Lt_Lu_hour[, bl2 := bl^2]
#   Lt_Lu_hour[, bl3 := bl^3]
#
#   pred_hourly=predict(model_h, Lt_Lu_hour)
#   Lt_Lu_hour[, prediction_h := pred_hourly]
#   Lt_Lu_hour[, L_u := pred_hourly + L_t]
#   Lt_Lu_hour[, L_e_u := pred_hourly + L_t + epsilon_u]
#
#   return(Lt_Lu_hour)
#
# }
#
#
# f6=H_calibration(calendar_hourly_adjusted,model_hourly)
# saveRDS(f6, file = file.path('..', 'HPFC','data', 'data_package', "dd_Leu.rds"))
