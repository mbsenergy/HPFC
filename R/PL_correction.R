
#' Apply the PL correction to the hourly forecast
#'
#' The PL correction is applied to make the price coeherent with Peak load price if available ...
#'
#' @param x An hourly dataframe calendar with date, and the forecast adjusted.
#' @returns A hourly dataframe with 71 columns with the final_forecast
#' @import data.table
#' @export

PL_correction=function(dataframe){

  if (!('L_e_u_adj' %in% colnames(dataframe)) | class(dataframe$L_e_u_adj) != 'numeric') {stop("L_e_u_adj column must be format numeric")}

  Lt_Lu_hour=copy(dataframe)

  #### explicit epsilon as in Caldana
  Lt_Lu_hour[, epsilon_u := spot_forward_month_BL]

  #### correct period before last date availbale (history)
  Lt_Lu_hour[, period := fifelse(history_forecast == 0, as.character(date), period)]

  #### peak hour during day, offpeak night and we
  Lt_Lu_hour[, peak := as.numeric(hour >= 9 & hour <= 20 & weekend == 0)]

  #### intermediate varibale
  Lt_Lu_hour[, tot_hours := .N, by = period]

  Lt_Lu_hour[,`:=`(sum_L_e_u_peak = sum(L_e_u_adj * peak),
                   sum_L_e_u_off = sum(L_e_u_adj * (1 - peak)),
                   number_peak_hrs = sum(peak)), by = period]

  Lt_Lu_hour[, number_off_peak := tot_hours - number_peak_hrs, by = period]

  #### PL correction
  Lt_Lu_hour[, final_forward_month_PL := fifelse(is.na(spot_forward_month_PL), sum_L_e_u_peak / number_peak_hrs, spot_forward_month_PL)]
  Lt_Lu_hour[, ci_term_on_peak := (number_peak_hrs * final_forward_month_PL - sum_L_e_u_peak) / (number_peak_hrs)]
  Lt_Lu_hour[, ci_term_off_peak := ((tot_hours * epsilon_u - number_peak_hrs * final_forward_month_PL) - sum_L_e_u_off) / number_off_peak]


  Lt_Lu_hour[, phi_u := fifelse(peak == 1, ci_term_on_peak, ci_term_off_peak)]

  #### forecast vs history
  Lt_Lu_hour[, f_u := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj + phi_u)]
  Lt_Lu_hour[, L_u := fifelse(history_forecast == 0, 0, L_u)]
  Lt_Lu_hour[, epsilon_u := fifelse(history_forecast == 0, 0, epsilon_u)]
  Lt_Lu_hour[, phi_u := fifelse(history_forecast == 0, 0, phi_u)]


  #### daily terms
  Lt_Lu_hour[,`:=`(daily_L_e_u = mean(L_e_u),
                   daily_L_e_u_adj = mean(L_e_u_adj),
                   daily_f_u = mean(f_u)), by = date]

  Lt_Lu_hour[, PL_corr := fifelse(phi_u != 0, phi_u + spot_forward_month_BL, as.numeric(NA))]

  Lt_Lu_hour[,final_forecast:=f_u]


  return(Lt_Lu_hour)

}

