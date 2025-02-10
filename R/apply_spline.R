
#' Apply the spline smoothing to the daily forecast
#'
#' The spline is done through the function spline ...
#'
#' @param x An hourly dataframe calendar with date, long term seasonality and forward structure.
#' @returns A hourly dataframe with 57 columns to be used for hourly estimation
#' @import data.table
#' @export

apply_spline=function(dataframe,smoothig_parameter){

  if (!('L_e_u' %in% colnames(dataframe)) | class(dataframe$L_e_u) != 'numeric') {stop("L_e_u column must be format numeric")}
  if (!(class(smoothig_parameter) == 'integer' | class(smoothig_parameter) == 'numeric')) {stop("smoothig_parameter must be format numeric or integer")}

  Lt_Lu_hour=copy(dataframe)

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


  #-------------------------------------------------------- return to forward

  Lt_Lu_hour[, smooth_period := mean(smooth_line), by='period']

  Lt_Lu_hour[, delta_smooth := epsilon_u - smooth_period]
  Lt_Lu_hour[, smooth_corrected := smooth_line + delta_smooth]

  Lt_Lu_hour[, L_e_u_adj := smooth_corrected]

  return(Lt_Lu_hour)

}

