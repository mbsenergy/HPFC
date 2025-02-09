
#' Apply the spline smoothing to the hourly forecast
#'
#' The spline is done through the function spline for pwr
#'
#' @param x An hourly dataframe calendar with date, long term seasonality and forward structure.
#' @returns A hourly dataframe with 57 columns to be used for hourly estimation
#' @export

apply_spline_pwr=function(dataframe,smoothig_parameter){

  if (!('L_e_u' %in% colnames(dataframe)) | class(dataframe$L_e_u) != 'numeric') {stop("L_e_u column must be format numeric")}
  if (!(class(smoothig_parameter) == 'integer' | class(smoothig_parameter) == 'numeric')) {stop("smoothig_parameter must be format numeric or integer")}

  Lt_Lu_hour=copy(dataframe)

  #### create L_e_u deviation from week mean
  Lt_Lu_hour[, week_n := lubridate::week(date)]
  Lt_Lu_hour[, deviation_from_w_wmean := L_e_u - mean(L_e_u, na.rm = TRUE), by = .(week_n, year)]

  #### create daily DB with L_e_u mean to find daily spline
  DB_spline = copy(Lt_Lu_hour)
  DB_spline[, D_mean := mean(L_e_u, na.rm = TRUE), by = date]
  DB_spline = DB_spline[, .(date, D_mean)] |> unique()

  #### create spline (smooth param set =15 at the beginning)
  DB_spline[, obs := .I]
  DB_spline[, spline_L_e_u := spline(obs,D_mean, xout = obs)$y]
  DB_spline[, smooth_line := smooth.spline(obs, spline_L_e_u,
                                           df = dim(DB_spline)[1] / smoothig_parameter)$y]

  #### merge spline on hourly
  Lt_Lu_hour = DB_spline[, .(smooth_line, date)][Lt_Lu_hour, on = 'date']

  #### combine spline and weekly deviation
  Lt_Lu_hour[, W_mean_adj := mean(smooth_line, na.rm = TRUE), by = .(week_n, year)]
  Lt_Lu_hour[, L_e_u_adj := W_mean_adj + deviation_from_w_wmean]

  #### substitute spline with spot before fwd date
  Lt_Lu_hour[, L_e_u_adj := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj)]

  #-------------------------------------------------------- return to forward

  return(Lt_Lu_hour)

}
