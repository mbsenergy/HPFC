#' Adjust Calibrates and Detrend Data by Forward Period
#'
#' This function adjusts the periods based on the forward calendar and detrends the 'L_t' values for historical periods. It also categorizes dates as 'history' or 'forecast' based on a provided last date.
#'
#' @param DT A data.table containing the power data with necessary columns.
#' @param last_date A string representing the last date in 'YYYY-MM-DD' format.
#' @return A data.table with adjusted periods and detrended values for 'L_t'.
#' @import data.table
#' @export

period_calibration = function(DT, last_date) {
  
  # Ensure required columns are numeric
  if (!('forward_month_BL' %in% colnames(DT)) | class(DT$forward_month_BL) != 'numeric') {
    stop("forward_month_BL column must be format numeric")
  } else if (!('forward_quarter_BL' %in% colnames(DT)) | class(DT$forward_quarter_BL) != 'numeric') {
    stop("forward_quarter_BL column must be format numeric")
  } else if (!('L_t' %in% colnames(DT)) | class(DT$L_t) != 'numeric') {
    stop("L_t column must be format numeric")
  } else if (!('BL_prev_m' %in% colnames(DT)) | class(DT$BL_prev_m) != 'numeric') {
    stop("BL_prev_m column must be format numeric")
  }
  
  DTW = copy(DT)
  
  # Create period of reference forward
  DTW[, period := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), paste("mese", month, year, sep = ''),
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), paste('remainder_year', year, sep = ''),
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), paste('remainder_quarter', quarter, year, sep = ''),
    !is.na(forward_month_BL), paste("mese", month, year, sep = '')
  )]
  
  # Convert 'last_date' to Date type and calculate the current period start
  begin_current_period = as.Date(paste(substr(last_date, 1, 7), '01', sep = "-"))
  
  # Detrend 'L_t' by forward period
  DTW[, period := fifelse(date < begin_current_period, 'history', period)]
  DTW[, history_forecast := fifelse(date <= last_date, 0, 1)]
  DTW[, L_t := L_t - mean(L_t), by = period]
  
  # Remove columns no longer needed
  DTW[, `:=`(
    forward_month_BL = NULL,
    forward_quarter_BL = NULL,
    BL_prev_m = NULL
  )]
  
  return(DTW)
}
