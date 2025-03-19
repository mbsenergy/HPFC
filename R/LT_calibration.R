
#' Calibrate long term seasonality on future calendar
#'
#' The function use changepoint algorithm ...
#'
#' @param x A DT with calendar regressor, ttf and power future.
#' @param y Long term parameters created by the function 'long term models'.
#' @returns A DT with 52 columns among whihch L_t
#' @import data.table
#' @export

LT_calibration = function(DT, profile_matrix) {

  if(any(sapply(profile_matrix,class) != 'numeric')) {stop('long term parameters must be numeric')}

  DTW = copy(DT)
  DTW[, value_gas2 := value_gas^2]

  DTW = DTW[, L_t :=
                    cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term[1] +
                    cos((2 * pi) * yday_season) * profile_matrix$cos_season[1] +
                    cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer[1] +
                    sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term[1] +
                    sin((2 * pi) * yday_season) * profile_matrix$sin_season[1] +
                    sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer[1] +
                    summer * profile_matrix$summer[1] +

                    value_gas * profile_matrix$value_gas[1] +
                    value_gas2 * profile_matrix$value_gas2[1] +

                    # generate day-type deviations :
                    holiday * profile_matrix$holiday[1] +
                    day_1 * profile_matrix$day_1[1] +
                    day_2 * profile_matrix$day_2[1] +
                    day_3 * profile_matrix$day_3[1] +
                    day_4 * profile_matrix$day_4[1] +
                    day_5 * profile_matrix$day_5[1] +
                    day_6 * profile_matrix$day_6[1] +
                    day_7 * profile_matrix$day_7[1] +

                    yday_1 * profile_matrix$yday_1[1] +
                    yday_2 * profile_matrix$yday_2[1] +
                    yday_3 * profile_matrix$yday_3[1] +
                    yday_4 * profile_matrix$yday_4[1] +
                    yday_5 * profile_matrix$yday_5[1] +
                    yday_6 * profile_matrix$yday_6[1] +
                    yday_7 * profile_matrix$yday_7[1] +
                    yday_8 * profile_matrix$yday_8[1] +
                    yday_9 * profile_matrix$yday_9[1] +
                    yday_10 * profile_matrix$yday_10[1]

  ]

  return(DTW)

}
