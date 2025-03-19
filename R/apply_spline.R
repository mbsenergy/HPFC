
#' Apply the spline smoothing to the daily forecast
#'
#' The spline is done through the function spline ...
#'
#' @param x An hourly DT calendar with date, long term seasonality and forward structure.
#' @returns A hourly DT with 57 columns to be used for hourly estimation
#' @import data.table
#' @export

apply_spline = function(DT, smoothig_parameter) {

  if (!('L_e_u' %in% colnames(DT)) | class(DT$L_e_u) != 'numeric') {stop("L_e_u column must be format numeric")}
  if (!(class(smoothig_parameter) == 'integer' | class(smoothig_parameter) == 'numeric')) {stop("smoothig_parameter must be format numeric or integer")}

  DTW = copy(DT)

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

  #-------------------------------------------------------- return to forward

  DTW[, smooth_period := mean(smooth_line), by='period']

  DTW[, delta_smooth := epsilon_u - smooth_period]
  DTW[, smooth_corrected := smooth_line + delta_smooth]

  DTW[, L_e_u_adj := smooth_corrected]

  return(DTW)

}

