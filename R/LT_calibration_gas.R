
#' Calibrate long term seasonality on future calendar
#'
#' The function use changepoint algorithm ...
#'
#' @param x A dataframe with calendar regressor, ttf and power future.
#' @param y Long term parameters created by the function 'long term models'.
#' @returns A dataframe with 52 columns among whihch L_t
#' @export

LT_calibration_gas=function(dataframe,profile_matrix){

  if (!('date' %in% colnames(dataframe)) | class(dataframe$date) != 'Date') {stop("date column must be format Date")}
  if (!('holiday' %in% colnames(dataframe)) | class(dataframe$holiday) != 'numeric') {stop("date column must be format numeric")}

  if (!('forward_month_BL_gas' %in% colnames(dataframe)) | class(dataframe$forward_month_BL_gas) != 'numeric') {stop("forward_month_BL_gas column must be format numeric")
  } else if (!('forward_quarter_BL_gas' %in% colnames(dataframe)) | class(dataframe$forward_quarter_BL_gas) != 'numeric') {stop("forward_quarter_BL column must be format numeric")
  } else if (!('BL_gas_prev_m' %in% colnames(dataframe)) | class(dataframe$BL_gas_prev_m) != 'numeric') {stop("BL_gas_prev_m column must be format numeric") }

  if(any(sapply(profile_matrix,class) != 'numeric')) {stop('long term parameters must be numeric')}

  Lt_day=copy(dataframe)

  Lt_day = Lt_day[, L_t :=

                    # generate long-term seasonality (intra-year trends)  :

                    cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term[1] +
                    cos((2 * pi) * yday_season) * profile_matrix$cos_season[1] +
                    cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer[1] +
                    sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term[1] +
                    sin((2 * pi) * yday_season) * profile_matrix$sin_season[1] +
                    sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer[1] +
                    summer * profile_matrix$summer[1] +
                    profile_matrix$break_group[1] +

                    # trade_close * profile_matrix$trade_close[1] +
                    # trade_close2 * profile_matrix$trade_close2[1] +

                    # cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term_p[1] +
                    # cos((2 * pi) * yday_season) * profile_matrix$cos_season_p[1] +
                    # cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer_p[1] +
                    # sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term_p[1] +
                    # sin((2 * pi) * yday_season) * profile_matrix$sin_season_p[1] +
                    # sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer_p[1] +
                    # profile_matrix$break_group[1] +

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

  setnames(Lt_day, c('BL_gas_prev_m', 'forward_quarter_BL_gas', 'forward_month_BL_gas'), c('BL_prev_m', 'forward_quarter_BL', 'forward_month_BL'))

  return(Lt_day)

}
