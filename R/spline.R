#' Apply Spline to Gas Data
#'
#' This function applies a spline smoothing technique to power data to adjust for daily variations using a smoothing parameter.
#'
#' @param DT A data.table containing the power data with the 'L_e_u' column.
#' @param smoothig_parameter A numeric or integer parameter to control the smoothing.
#' @return A data.table with adjusted 'L_e_u' values based on the spline smoothing.
#' @import data.table
#' @export

spline_gas = function(DT, smoothig_parameter = 15) {
  
  # Ensure 'L_e_u' column exists and is numeric, and that 'smoothig_parameter' is numeric or integer
  if (!('L_e_u' %in% colnames(DT)) | class(DT$L_e_u) != 'numeric') {
    stop("L_e_u column must be format numeric")
  }
  if (!(class(smoothig_parameter) %in% c('integer', 'numeric'))) {
    stop("smoothig_parameter must be format numeric or integer")
  }
  
  # Copy input data table
  DTW = copy(DT)
  
  # Create daily DB with L_e_u mean to find daily spline
  DTS = copy(DTW)
  DTS[, D_mean := mean(L_e_u, na.rm = TRUE), by = date]
  DTS = unique(DTS[, .(date, D_mean)])
  
  # Create spline
  DTS[, obs := .I]
  DTS[, spline_L_e_u := spline(obs, D_mean, xout = obs)$y]
  
  # Apply smoothing spline using specified smoothing parameter
  DTS[, smooth_line := smooth.spline(obs, spline_L_e_u, df = nrow(DTS) / smoothig_parameter)$y]
  
  # Merge spline values with the original data table
  DTW = DTS[, .(smooth_line, date)][DTW, on = 'date']
  
  # Calculate smooth period, delta, and adjusted values
  DTW[, smooth_period := mean(smooth_line), by = 'period']
  DTW[, delta_smooth := epsilon_u - smooth_period]
  DTW[, smooth_corrected := smooth_line + delta_smooth]
  DTW[, L_e_u_adj := smooth_corrected]
  
  return(DTW)
}


#' Apply Spline to Power with Weekly Deviation Adjustment
#'
#' This function computes the weekly deviation and applies a spline smoothing technique to adjust 'L_e_u' values, substituting with spot values before forward date.
#'
#' @param DT A data.table containing the power data with the 'L_e_u' column.
#' @param smoothig_parameter A numeric or integer parameter to control the smoothing.
#' @return A data.table with adjusted 'L_e_u' values, considering weekly deviation and spline smoothing.
#' @import data.table
#' @export

spline_pwr = function(DT, smoothig_parameter = 15) {
  
  # Copy input data table
  DTW = copy(DT)
  
  # Ensure 'L_e_u' column exists and is numeric
  if (!('L_e_u' %in% colnames(DT)) | class(DT$L_e_u) != 'numeric') {
    stop("L_e_u column must be format numeric")
  }
  
  # Calculate weekly deviation from weekly mean
  DTW[, week_n := data.table::week(date)]
  DTW[, deviation_from_w_wmean := L_e_u - mean(L_e_u, na.rm = TRUE), by = .(week_n, year)]
  
  # Create daily DB with L_e_u mean to find daily spline
  DTS = copy(DTW)
  DTS[, D_mean := mean(L_e_u, na.rm = TRUE), by = date]
  DTS = unique(DTS[, .(date, D_mean)])
  
  # Apply spline to the daily L_e_u mean
  DTS[, obs := .I]
  DTS[, spline_L_e_u := spline(obs, D_mean, xout = obs)$y]
  
  # Apply smoothing using the smoothing parameter
  DTS[, smooth_line := smooth.spline(obs, spline_L_e_u, df = nrow(DTS) / smoothig_parameter)$y]
  
  # Merge spline values on hourly data
  DTW = DTS[, .(smooth_line, date)][DTW, on = 'date']
  
  # Adjust L_e_u using weekly mean and deviation
  DTW[, W_mean_adj := mean(smooth_line, na.rm = TRUE), by = .(week_n, year)]
  DTW[, L_e_u_adj := W_mean_adj + deviation_from_w_wmean]
  
  # Substitute spline with spot values before forward date
  DTW[, L_e_u_adj := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj)]
  
  return(DTW)
}
