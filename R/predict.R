#' Gas Prediction Function
#'
#' This function predicts gas-related parameters ('L_t') using a series of long-term and seasonal parameters provided in the 'profile_matrix'.
#' 
#' @param DT A data.table containing the input data, including relevant columns like 'yday', 'holiday', 'summer', etc.
#' @param profile_matrix A matrix containing the long-term and seasonal parameters to be used in the prediction.
#' @return A data.table with the predicted 'L_t' values and updated column names.
#' @import data.table
#' @export

predict_lt_gas = function(DT, profile_matrix) {
  
  # Ensure 'profile_matrix' columns are numeric
  if (any(sapply(profile_matrix, class) != 'numeric')) {
    stop('long term parameters must be numeric')
  }
  
  DTW = copy(DT)
  
  # Perform the gas prediction for 'L_t'
  DTW[, L_t := 
        cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term[1] +
        cos((2 * pi) * yday_season) * profile_matrix$cos_season[1] +
        cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer[1] +
        sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term[1] +
        sin((2 * pi) * yday_season) * profile_matrix$sin_season[1] +
        sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer[1] +
        summer * profile_matrix$summer[1] +
        profile_matrix$break_group[1] +
        
        # Day-type deviations
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
  
  # Rename columns for clarity
  setnames(DTW, c('BL_gas_prev_m', 'forward_quarter_BL_gas', 'forward_month_BL_gas'), 
           c('BL_prev_m', 'forward_quarter_BL', 'forward_month_BL'))
  
  return(DTW)
}



#' Long-term Power Prediction
#'
#' This function predicts long-term power values ('L_t') using a series of parameters provided in the 'profile_matrix'.
#' 
#' @param DT A data.table containing the input data, including relevant columns like 'yday', 'value_gas', 'holiday', etc.
#' @param profile_matrix A matrix containing the long-term and seasonal parameters to be used in the prediction.
#' @return A data.table with the predicted 'L_t' values.
#' @import data.table
#' @export

predict_lt_pwr = function(DT, profile_matrix) {
  
  # Ensure 'profile_matrix' columns are numeric
  if (any(sapply(profile_matrix, class) != 'numeric')) {
    stop('long term parameters must be numeric')
  }
  
  DTW = copy(DT)
  
  # Compute the square of 'value_gas' to avoid repetitive calculation
  DTW[, value_gas2 := value_gas^2]
  
  # Perform the long-term prediction for 'L_t'
  DTW[, L_t := 
        cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term[1] +
        cos((2 * pi) * yday_season) * profile_matrix$cos_season[1] +
        cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer[1] +
        sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term[1] +
        sin((2 * pi) * yday_season) * profile_matrix$sin_season[1] +
        sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer[1] +
        summer * profile_matrix$summer[1] +
        value_gas * profile_matrix$value_gas[1] +
        value_gas2 * profile_matrix$value_gas2[1] +
        
        # Day-type deviations
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




#' Short-Term Power Prediction
#'
#' This function predicts short-term power parameters (`L_u`, `L_e_u`) using a pre-trained model `model_h` and data from `DT`.
#' 
#' @param DT A data.table containing the input data, including time-related variables, gas values, and any additional predictors.
#' @param model_h A pre-trained model used to predict the hourly power values.
#' @return A data.table with the predicted power values (`L_u`, `L_e_u`) and relevant columns.
#' @import data.table
#' @export

predict_st_pwr = function(DT, model_h) {
  
  # Ensure model_h is a valid model object
  # if (missing(model_h) || !inherits(model_h, "model")) {
  #   stop("The provided 'model_h' is not a valid model object.")
  # }
  
  DTW = copy(DT)
  
  # Explicitly set epsilon_u as the spot_forward_month_BL
  DTW[, epsilon_u := spot_forward_month_BL]
  
  # Create hour indicators (1 for the corresponding hour, 0 otherwise)
  DTW[, break_h := 1]
  DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) fifelse(hour == i, 1, 0))]
  
  # Generate polynomial terms for yday and value_gas
  DTW[, yday2 := yday^2]
  DTW[, yday3 := yday^3]
  DTW[, value_gas2 := value_gas^2]
  
  # Calculate bl and its powers
  DTW[, bl := epsilon_u + L_t]
  DTW[, bl2 := bl^2]
  DTW[, bl3 := bl^3]
  
  # Predict hourly values using the provided model
  pred_hourly = predict(model_h, DTW)
  DTW[, prediction_h := pred_hourly]
  
  # Calculate predicted L_u and L_e_u
  DTW[, L_u := prediction_h + L_t]
  DTW[, L_e_u := prediction_h + L_t + epsilon_u]
  
  return(DTW)
}


#' Short-Term Power Prediction
#'
#' This function predicts short-term power parameters (`L_u`, `L_e_u`) using a pre-trained model `model_h` and data from `DT`.
#' 
#' @param DT A data.table containing the input data, including time-related variables, gas values, and any additional predictors.
#' @param model_h A pre-trained model used to predict the hourly power values.
#' @return A data.table with the predicted power values (`L_u`, `L_e_u`) and relevant columns.
#' @import data.table
#' @export

predict_st_pwr = function(DT, model_h) {
  
  # Ensure model_h is a valid model object
  # if (missing(model_h) || !inherits(model_h, "model")) {
  #   stop("The provided 'model_h' is not a valid model object.")
  # }
  
  DTW = copy(DT)
  
  # Explicitly set epsilon_u as the spot_forward_month_BL
  DTW[, epsilon_u := spot_forward_month_BL]
  
  # Create hour indicators (1 for the corresponding hour, 0 otherwise)
  DTW[, break_h := 1]
  DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) fifelse(hour == i, 1, 0))]
  
  # Generate polynomial terms for yday and value_gas
  DTW[, yday2 := yday^2]
  DTW[, yday3 := yday^3]
  DTW[, value_gas2 := value_gas^2]
  
  # Calculate bl and its powers
  DTW[, bl := epsilon_u + L_t]
  DTW[, bl2 := bl^2]
  DTW[, bl3 := bl^3]
  
  # Predict hourly values using the provided model
  pred_hourly = predict(model_h, DTW)
  DTW[, prediction_h := pred_hourly]
  
  # Calculate predicted L_u and L_e_u
  DTW[, L_u := prediction_h + L_t]
  DTW[, L_e_u := prediction_h + L_t + epsilon_u]
  
  return(DTW)
}

