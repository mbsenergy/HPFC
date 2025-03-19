
#' Apply the spline smoothing to the hourly forecast
#'
#' The spline is done through the function spline for pwr
#'
#' @param x An hourly DT calendar with date, long term seasonality and forward structure.
#' @returns A hourly DT with 57 columns to be used for hourly estimation
#' @import data.table
#' @export

apply_spline_pwr = function(DT, smoothig_parameter) {

  DTW = copy(DT)

  #### create L_e_u deviation from week mean
  DTW[, week_n := data.table::week(date)]
  DTW[, deviation_from_w_wmean := L_e_u - mean(L_e_u, na.rm = TRUE), by = .(week_n, year)]

  #### create daily DB with L_e_u mean to find daily spline
  DTS = copy(DTW)
  DTS[, D_mean := mean(L_e_u, na.rm = TRUE), by = date]
  DTS = DTS[, .(date, D_mean)] |> unique()

  #### create spline (smooth param set =15 at the beginning)
  DTS[, obs := .I]
  DTS[, spline_L_e_u := spline(obs,D_mean, xout = obs)$y]
  DTS[, smooth_line := smooth.spline(obs, spline_L_e_u,
                                           df = dim(DTS)[1] / smoothig_parameter)$y]

  #### merge spline on hourly
  DTW = DTS[, .(smooth_line, date)][DTW, on = 'date']

  #### combine spline and weekly deviation
  DTW[, W_mean_adj := mean(smooth_line, na.rm = TRUE), by = .(week_n, year)]
  DTW[, L_e_u_adj := W_mean_adj + deviation_from_w_wmean]

  #### substitute spline with spot before fwd date
  DTW[, L_e_u_adj := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj)]

  return(DTW)

}
