#' Train long-term gas model
#'
#' Fits a regression model to estimate long-term gas price effects based on break periods.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `detr_value` (numeric): Detrended gas price.
#'   - `break_group_p` (integer): Market regime grouping.
#'   - `cos_long_term`, `sin_long_term`, `holiday`, `day_2` to `day_7` (numeric): Regressors.
#' @return A `data.table` with model coefficients, including missing terms filled with 0.
#' @import data.table
#' @importFrom stats lm step
#' @export

train_lt_model_gas = function(DT) {
  
  # Ensure DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Required columns
  required_cols = c("detr_value", "break_group_p", "cos_long_term", "sin_long_term", 
                    "holiday", paste0("day_", 2:7))
  
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Copy dataset
  DT_reg = copy(DT)
  
  # Number of break groups
  n_groups = max(DT_reg$break_group_p) + 1
  
  # Generate regression formula dynamically
  reg_terms = c()
  for (x in 1:n_groups) {
    reg_terms = c(reg_terms, 
                  paste0("break_group_", x), 
                  paste0("cos_long_term_", x), 
                  paste0("sin_long_term_", x),
                  paste0("holiday_", x),
                  paste0("day_", 2:7, "_", x),
                  paste0("yday_", 1:10, "_", x))
  }
  regression_formula = paste("detr_value ~ 0 +", paste(reg_terms, collapse = " + "))
  
  # Fit model using stepwise regression
  model_1 = suppressWarnings(suppressMessages(
    eval(substitute(step(lm(regression_formula, weights = weight, data = DT_reg), 
                         direction = "both", trace = 0)))
  ))
  
  # Determine forecast group
  forecast_group = ifelse(min(DT_reg[break_group_p == max(break_group_p)]$date) < as.Date('2022-01-01'), 
                          n_groups - 1, n_groups)
  
  # Define expected terms based on forecast group
  terms = paste(c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 
                  'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 
                  'day_1', paste0("day_", 2:7), paste0("yday_", 1:10), "break_group"), 
                forecast_group, sep = "_")
  
  # Identify missing terms and fill them with 0
  missing_terms = setdiff(terms, names(model_1$coefficients))
  profile_matrix = as.data.table(matrix(c(model_1$coefficients, rep(0, length(missing_terms))), nrow = 1))
  setnames(profile_matrix, , c(names(model_1$coefficients), missing_terms))
  
  # Drop unnecessary columns
  set(profile_matrix, , names(profile_matrix)[!names(profile_matrix) %in% terms], NULL)
  
  # Fix column names (remove trailing "_x" part)
  setnames(profile_matrix, , gsub("_\\d+$", "", names(profile_matrix)))
  
  return(profile_matrix)
}



#' Train Long-Term model - PWR
#'
#' This function fits a regression model to estimate the long-term seasonal component of power prices.
#' It includes trigonometric seasonal components, day-of-week effects, and gas price interactions.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `detr_value` (numeric): Detrended power price.
#'   - `value_gas` (numeric): Gas price variable.
#'   - `weight` (numeric): Regression weights.
#' @return A `data.table` containing the estimated coefficients from the regression model.
#' @import data.table
#' @export

train_lt_model_pwr = function(DT) {
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Check if required columns exist
  required_cols = c("detr_value", "value_gas", "weight")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Copy DT to avoid modifying the original
  DTW = copy(DT)
  
  # Create squared gas price term
  DTW[, value_gas2 := value_gas^2]
  
  # Define base regression formula
  regression_terms = c(
    "cos_long_term", "cos_season", "cos_season_summer",
    "holiday", "sin_long_term", "sin_season", "sin_season_summer",
    "summer", "day_1", "day_2", "day_3", "day_4", "day_5", "day_6", "day_7"
  )
  
  # Add polynomial day-of-year terms
  yday_terms = paste0("yday_", 1:10)
  
  # Add gas price terms
  additional_terms = c("value_gas", "value_gas2")
  
  # Construct regression formula
  regression_formula = as.formula(paste("detr_value ~", paste(c(regression_terms, yday_terms, additional_terms), collapse = " + ")))
  
  # Fit the regression model with stepwise selection
  model_1 = suppressWarnings(suppressMessages(
    step(lm(regression_formula, weights = weight, data = DTW), direction = "both", trace = 0)
  ))
  
  # Ensure all expected terms are present in the coefficient table
  expected_terms = c(regression_terms, yday_terms, additional_terms)
  missing_terms = setdiff(expected_terms, names(model_1$coefficients))
  
  # Create a coefficient profile matrix
  profile_matrix = as.data.table(matrix(c(model_1$coefficients, rep(0, length(missing_terms))), nrow = 1))
  setnames(profile_matrix, , c(names(model_1$coefficients), missing_terms))
  
  return(profile_matrix)
}


#' Predict Short-Term model - PWR
#'
#' This function fits a nonlinear regression model to estimate short-term effects in power prices.
#' The model captures hourly patterns, seasonal effects, and interactions with gas prices.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `value_h` (numeric): Detrended power price.
#'   - `value_gas` (numeric): Gas price variable.
#'   - `hour_1` to `hour_24` (numeric): Hourly dummies.
#'   - `weekend`, `break_h` (numeric): Weekend and market regime indicators.
#' @return A nonlinear least squares (NLS) model.
#' @import data.table
#' @importFrom stats nls nls.control
#' @export

train_st_model_pwr = function(DT){
  
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Required columns for the model
  required_cols = c("value_h", "value_gas", paste0("hour_", 1:24), "weekend", "break_h")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Copy DT to avoid modifying the original
  DTW = copy(DT)
  
  DTW[, value_gas2 := value_gas^2]
  
  nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
              printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
              nDcentral = FALSE)
  
  model_h=nls(value_h ~
                hour_1*coeff_hour_1+
                hour_2*coeff_hour_2+
                hour_3*coeff_hour_3+
                hour_4*coeff_hour_4+
                hour_5*coeff_hour_5+
                hour_6*coeff_hour_6+
                hour_7*coeff_hour_7+
                hour_8*coeff_hour_8+
                hour_9*coeff_hour_9+
                hour_10*coeff_hour_10+
                hour_11*coeff_hour_11+
                hour_12*coeff_hour_12+
                hour_13*coeff_hour_13+
                hour_14*coeff_hour_14+
                hour_15*coeff_hour_15+
                hour_16*coeff_hour_16+
                hour_17*coeff_hour_17+
                hour_18*coeff_hour_18+
                hour_19*coeff_hour_19+
                hour_20*coeff_hour_20+
                hour_21*coeff_hour_21+
                hour_22*coeff_hour_22+
                hour_23*coeff_hour_23+
                (-1*(coeff_hour_1+coeff_hour_2+
                       coeff_hour_3+coeff_hour_4+
                       coeff_hour_5+coeff_hour_6+
                       coeff_hour_7+coeff_hour_8+
                       coeff_hour_9+coeff_hour_10+
                       coeff_hour_11+coeff_hour_12+
                       coeff_hour_13+coeff_hour_14+
                       coeff_hour_15+coeff_hour_16+
                       coeff_hour_17+coeff_hour_18+
                       coeff_hour_19+coeff_hour_20+
                       coeff_hour_21+coeff_hour_22+
                       coeff_hour_23))*hour_24+ # end hour_ dummy --------------
              hour_1*bl*coeff_bl_hour_1+
                hour_2*bl*coeff_bl_hour_2+
                hour_3*bl*coeff_bl_hour_3+
                hour_4*bl*coeff_bl_hour_4+
                hour_5*bl*coeff_bl_hour_5+
                hour_6*bl*coeff_bl_hour_6+
                hour_7*bl*coeff_bl_hour_7+
                hour_8*bl*coeff_bl_hour_8+
                hour_9*bl*coeff_bl_hour_9+
                hour_10*bl*coeff_bl_hour_10+
                hour_11*bl*coeff_bl_hour_11+
                hour_12*bl*coeff_bl_hour_12+
                hour_13*bl*coeff_bl_hour_13+
                hour_14*bl*coeff_bl_hour_14+
                hour_15*bl*coeff_bl_hour_15+
                hour_16*bl*coeff_bl_hour_16+
                hour_17*bl*coeff_bl_hour_17+
                hour_18*bl*coeff_bl_hour_18+
                hour_19*bl*coeff_bl_hour_19+
                hour_20*bl*coeff_bl_hour_20+
                hour_21*bl*coeff_bl_hour_21+
                hour_22*bl*coeff_bl_hour_22+
                hour_23*bl*coeff_bl_hour_23+
                (-1*(coeff_bl_hour_1+coeff_bl_hour_2+
                       coeff_bl_hour_3+coeff_bl_hour_4+
                       coeff_bl_hour_5+coeff_bl_hour_6+
                       coeff_bl_hour_7+coeff_bl_hour_8+
                       coeff_bl_hour_9+coeff_bl_hour_10+
                       coeff_bl_hour_11+coeff_bl_hour_12+
                       coeff_bl_hour_13+coeff_bl_hour_14+
                       coeff_bl_hour_15+coeff_bl_hour_16+
                       coeff_bl_hour_17+coeff_bl_hour_18+
                       coeff_bl_hour_19+coeff_bl_hour_20+
                       coeff_bl_hour_21+coeff_bl_hour_22+
                       coeff_bl_hour_23))*hour_24*bl + # end hour_ bl interaction --------------
              hour_1*bl2*coeff_bl2_hour_1+
                hour_2*bl2*coeff_bl2_hour_2+
                hour_3*bl2*coeff_bl2_hour_3+
                hour_4*bl2*coeff_bl2_hour_4+
                hour_5*bl2*coeff_bl2_hour_5+
                hour_6*bl2*coeff_bl2_hour_6+
                hour_7*bl2*coeff_bl2_hour_7+
                hour_8*bl2*coeff_bl2_hour_8+
                hour_9*bl2*coeff_bl2_hour_9+
                hour_10*bl2*coeff_bl2_hour_10+
                hour_11*bl2*coeff_bl2_hour_11+
                hour_12*bl2*coeff_bl2_hour_12+
                hour_13*bl2*coeff_bl2_hour_13+
                hour_14*bl2*coeff_bl2_hour_14+
                hour_15*bl2*coeff_bl2_hour_15+
                hour_16*bl2*coeff_bl2_hour_16+
                hour_17*bl2*coeff_bl2_hour_17+
                hour_18*bl2*coeff_bl2_hour_18+
                hour_19*bl2*coeff_bl2_hour_19+
                hour_20*bl2*coeff_bl2_hour_20+
                hour_21*bl2*coeff_bl2_hour_21+
                hour_22*bl2*coeff_bl2_hour_22+
                hour_23*bl2*coeff_bl2_hour_23+
                (-1*(coeff_bl2_hour_1+coeff_bl2_hour_2+
                       coeff_bl2_hour_3+coeff_bl2_hour_4+
                       coeff_bl2_hour_5+coeff_bl2_hour_6+
                       coeff_bl2_hour_7+coeff_bl2_hour_8+
                       coeff_bl2_hour_9+coeff_bl2_hour_10+
                       coeff_bl2_hour_11+coeff_bl2_hour_12+
                       coeff_bl2_hour_13+coeff_bl2_hour_14+
                       coeff_bl2_hour_15+coeff_bl2_hour_16+
                       coeff_bl2_hour_17+coeff_bl2_hour_18+
                       coeff_bl2_hour_19+coeff_bl2_hour_20+
                       coeff_bl2_hour_21+coeff_bl2_hour_22+
                       coeff_bl2_hour_23))*hour_24*bl2 + # end hour_ bl2 interaction --------------
              hour_1*bl3*coeff_bl3_hour_1+
                hour_2*bl3*coeff_bl3_hour_2+
                hour_3*bl3*coeff_bl3_hour_3+
                hour_4*bl3*coeff_bl3_hour_4+
                hour_5*bl3*coeff_bl3_hour_5+
                hour_6*bl3*coeff_bl3_hour_6+
                hour_7*bl3*coeff_bl3_hour_7+
                hour_8*bl3*coeff_bl3_hour_8+
                hour_9*bl3*coeff_bl3_hour_9+
                hour_10*bl3*coeff_bl3_hour_10+
                hour_11*bl3*coeff_bl3_hour_11+
                hour_12*bl3*coeff_bl3_hour_12+
                hour_13*bl3*coeff_bl3_hour_13+
                hour_14*bl3*coeff_bl3_hour_14+
                hour_15*bl3*coeff_bl3_hour_15+
                hour_16*bl3*coeff_bl3_hour_16+
                hour_17*bl3*coeff_bl3_hour_17+
                hour_18*bl3*coeff_bl3_hour_18+
                hour_19*bl3*coeff_bl3_hour_19+
                hour_20*bl3*coeff_bl3_hour_20+
                hour_21*bl3*coeff_bl3_hour_21+
                hour_22*bl3*coeff_bl3_hour_22+
                hour_23*bl3*coeff_bl3_hour_23+
                (-1*(coeff_bl3_hour_1+coeff_bl3_hour_2+
                       coeff_bl3_hour_3+coeff_bl3_hour_4+
                       coeff_bl3_hour_5+coeff_bl3_hour_6+
                       coeff_bl3_hour_7+coeff_bl3_hour_8+
                       coeff_bl3_hour_9+coeff_bl3_hour_10+
                       coeff_bl3_hour_11+coeff_bl3_hour_12+
                       coeff_bl3_hour_13+coeff_bl3_hour_14+
                       coeff_bl3_hour_15+coeff_bl3_hour_16+
                       coeff_bl3_hour_17+coeff_bl3_hour_18+
                       coeff_bl3_hour_19+coeff_bl3_hour_20+
                       coeff_bl3_hour_21+coeff_bl3_hour_22+
                       coeff_bl3_hour_23))*hour_24*bl3 + # end hour_ bl3 interaction --------------
              # NEW SEASONAL REGRESSORS
              season_winter*coeff_season_winter +
                season_spring*coeff_season_spring +
                season_summer*coeff_season_summer +
                season_fall*coeff_season_fall +         
                hour_1*yday*coeff_yday_hour_1+
                hour_2*yday*coeff_yday_hour_2+
                hour_3*yday*coeff_yday_hour_3+
                hour_4*yday*coeff_yday_hour_4+
                hour_5*yday*coeff_yday_hour_5+
                hour_6*yday*coeff_yday_hour_6+
                hour_7*yday*coeff_yday_hour_7+
                hour_8*yday*coeff_yday_hour_8+
                hour_9*yday*coeff_yday_hour_9+
                hour_10*yday*coeff_yday_hour_10+
                hour_11*yday*coeff_yday_hour_11+
                hour_12*yday*coeff_yday_hour_12+
                hour_13*yday*coeff_yday_hour_13+
                hour_14*yday*coeff_yday_hour_14+
                hour_15*yday*coeff_yday_hour_15+
                hour_16*yday*coeff_yday_hour_16+
                hour_17*yday*coeff_yday_hour_17+
                hour_18*yday*coeff_yday_hour_18+
                hour_19*yday*coeff_yday_hour_19+
                hour_20*yday*coeff_yday_hour_20+
                hour_21*yday*coeff_yday_hour_21+
                hour_22*yday*coeff_yday_hour_22+
                hour_23*yday*coeff_yday_hour_23+
                (-1*(coeff_yday_hour_1+coeff_yday_hour_2+
                       coeff_yday_hour_3+coeff_yday_hour_4+
                       coeff_yday_hour_5+coeff_yday_hour_6+
                       coeff_yday_hour_7+coeff_yday_hour_8+
                       coeff_yday_hour_9+coeff_yday_hour_10+
                       coeff_yday_hour_11+coeff_yday_hour_12+
                       coeff_yday_hour_13+coeff_yday_hour_14+
                       coeff_yday_hour_15+coeff_yday_hour_16+
                       coeff_yday_hour_17+coeff_yday_hour_18+
                       coeff_yday_hour_19+coeff_yday_hour_20+
                       coeff_yday_hour_21+coeff_yday_hour_22+
                       coeff_yday_hour_23))*hour_24*yday + # end hour_ yday interaction --------------
              hour_1*yday2*coeff_yday2_hour_1+
                hour_2*yday2*coeff_yday2_hour_2+
                hour_3*yday2*coeff_yday2_hour_3+
                hour_4*yday2*coeff_yday2_hour_4+
                hour_5*yday2*coeff_yday2_hour_5+
                hour_6*yday2*coeff_yday2_hour_6+
                hour_7*yday2*coeff_yday2_hour_7+
                hour_8*yday2*coeff_yday2_hour_8+
                hour_9*yday2*coeff_yday2_hour_9+
                hour_10*yday2*coeff_yday2_hour_10+
                hour_11*yday2*coeff_yday2_hour_11+
                hour_12*yday2*coeff_yday2_hour_12+
                hour_13*yday2*coeff_yday2_hour_13+
                hour_14*yday2*coeff_yday2_hour_14+
                hour_15*yday2*coeff_yday2_hour_15+
                hour_16*yday2*coeff_yday2_hour_16+
                hour_17*yday2*coeff_yday2_hour_17+
                hour_18*yday2*coeff_yday2_hour_18+
                hour_19*yday2*coeff_yday2_hour_19+
                hour_20*yday2*coeff_yday2_hour_20+
                hour_21*yday2*coeff_yday2_hour_21+
                hour_22*yday2*coeff_yday2_hour_22+
                hour_23*yday2*coeff_yday2_hour_23+
                (-1*(coeff_yday2_hour_1+coeff_yday2_hour_2+
                       coeff_yday2_hour_3+coeff_yday2_hour_4+
                       coeff_yday2_hour_5+coeff_yday2_hour_6+
                       coeff_yday2_hour_7+coeff_yday2_hour_8+
                       coeff_yday2_hour_9+coeff_yday2_hour_10+
                       coeff_yday2_hour_11+coeff_yday2_hour_12+
                       coeff_yday2_hour_13+coeff_yday2_hour_14+
                       coeff_yday2_hour_15+coeff_yday2_hour_16+
                       coeff_yday2_hour_17+coeff_yday2_hour_18+
                       coeff_yday2_hour_19+coeff_yday2_hour_20+
                       coeff_yday2_hour_21+coeff_yday2_hour_22+
                       coeff_yday2_hour_23))*hour_24*yday2 + # end hour_ yday2 interaction --------------
              hour_1*yday3*coeff_yday3_hour_1+
                hour_2*yday3*coeff_yday3_hour_2+
                hour_3*yday3*coeff_yday3_hour_3+
                hour_4*yday3*coeff_yday3_hour_4+
                hour_5*yday3*coeff_yday3_hour_5+
                hour_6*yday3*coeff_yday3_hour_6+
                hour_7*yday3*coeff_yday3_hour_7+
                hour_8*yday3*coeff_yday3_hour_8+
                hour_9*yday3*coeff_yday3_hour_9+
                hour_10*yday3*coeff_yday3_hour_10+
                hour_11*yday3*coeff_yday3_hour_11+
                hour_12*yday3*coeff_yday3_hour_12+
                hour_13*yday3*coeff_yday3_hour_13+
                hour_14*yday3*coeff_yday3_hour_14+
                hour_15*yday3*coeff_yday3_hour_15+
                hour_16*yday3*coeff_yday3_hour_16+
                hour_17*yday3*coeff_yday3_hour_17+
                hour_18*yday3*coeff_yday3_hour_18+
                hour_19*yday3*coeff_yday3_hour_19+
                hour_20*yday3*coeff_yday3_hour_20+
                hour_21*yday3*coeff_yday3_hour_21+
                hour_22*yday3*coeff_yday3_hour_22+
                hour_23*yday3*coeff_yday3_hour_23+
                (-1*(coeff_yday3_hour_1+coeff_yday3_hour_2+
                       coeff_yday3_hour_3+coeff_yday3_hour_4+
                       coeff_yday3_hour_5+coeff_yday3_hour_6+
                       coeff_yday3_hour_7+coeff_yday3_hour_8+
                       coeff_yday3_hour_9+coeff_yday3_hour_10+
                       coeff_yday3_hour_11+coeff_yday3_hour_12+
                       coeff_yday3_hour_13+coeff_yday3_hour_14+
                       coeff_yday3_hour_15+coeff_yday3_hour_16+
                       coeff_yday3_hour_17+coeff_yday3_hour_18+
                       coeff_yday3_hour_19+coeff_yday3_hour_20+
                       coeff_yday3_hour_21+coeff_yday3_hour_22+
                       coeff_yday3_hour_23))*hour_24*yday3 + # end hour_ yday3 interaction --------------
              hour_1*value_gas*coeff_value_gas_hour_1+
                hour_2*value_gas*coeff_value_gas_hour_2+
                hour_3*value_gas*coeff_value_gas_hour_3+
                hour_4*value_gas*coeff_value_gas_hour_4+
                hour_5*value_gas*coeff_value_gas_hour_5+
                hour_6*value_gas*coeff_value_gas_hour_6+
                hour_7*value_gas*coeff_value_gas_hour_7+
                hour_8*value_gas*coeff_value_gas_hour_8+
                hour_9*value_gas*coeff_value_gas_hour_9+
                hour_10*value_gas*coeff_value_gas_hour_10+
                hour_11*value_gas*coeff_value_gas_hour_11+
                hour_12*value_gas*coeff_value_gas_hour_12+
                hour_13*value_gas*coeff_value_gas_hour_13+
                hour_14*value_gas*coeff_value_gas_hour_14+
                hour_15*value_gas*coeff_value_gas_hour_15+
                hour_16*value_gas*coeff_value_gas_hour_16+
                hour_17*value_gas*coeff_value_gas_hour_17+
                hour_18*value_gas*coeff_value_gas_hour_18+
                hour_19*value_gas*coeff_value_gas_hour_19+
                hour_20*value_gas*coeff_value_gas_hour_20+
                hour_21*value_gas*coeff_value_gas_hour_21+
                hour_22*value_gas*coeff_value_gas_hour_22+
                hour_23*value_gas*coeff_value_gas_hour_23+
                (-1*(coeff_value_gas_hour_1+coeff_value_gas_hour_2+
                       coeff_value_gas_hour_3+coeff_value_gas_hour_4+
                       coeff_value_gas_hour_5+coeff_value_gas_hour_6+
                       coeff_value_gas_hour_7+coeff_value_gas_hour_8+
                       coeff_value_gas_hour_9+coeff_value_gas_hour_10+
                       coeff_value_gas_hour_11+coeff_value_gas_hour_12+
                       coeff_value_gas_hour_13+coeff_value_gas_hour_14+
                       coeff_value_gas_hour_15+coeff_value_gas_hour_16+
                       coeff_value_gas_hour_17+coeff_value_gas_hour_18+
                       coeff_value_gas_hour_19+coeff_value_gas_hour_20+
                       coeff_value_gas_hour_21+coeff_value_gas_hour_22+
                       coeff_value_gas_hour_23))*hour_24*value_gas + # end hour_ value_gas interaction --------------
              hour_1*value_gas2*coeff_value_gas2_hour_1+
                hour_2*value_gas2*coeff_value_gas2_hour_2+
                hour_3*value_gas2*coeff_value_gas2_hour_3+
                hour_4*value_gas2*coeff_value_gas2_hour_4+
                hour_5*value_gas2*coeff_value_gas2_hour_5+
                hour_6*value_gas2*coeff_value_gas2_hour_6+
                hour_7*value_gas2*coeff_value_gas2_hour_7+
                hour_8*value_gas2*coeff_value_gas2_hour_8+
                hour_9*value_gas2*coeff_value_gas2_hour_9+
                hour_10*value_gas2*coeff_value_gas2_hour_10+
                hour_11*value_gas2*coeff_value_gas2_hour_11+
                hour_12*value_gas2*coeff_value_gas2_hour_12+
                hour_13*value_gas2*coeff_value_gas2_hour_13+
                hour_14*value_gas2*coeff_value_gas2_hour_14+
                hour_15*value_gas2*coeff_value_gas2_hour_15+
                hour_16*value_gas2*coeff_value_gas2_hour_16+
                hour_17*value_gas2*coeff_value_gas2_hour_17+
                hour_18*value_gas2*coeff_value_gas2_hour_18+
                hour_19*value_gas2*coeff_value_gas2_hour_19+
                hour_20*value_gas2*coeff_value_gas2_hour_20+
                hour_21*value_gas2*coeff_value_gas2_hour_21+
                hour_22*value_gas2*coeff_value_gas2_hour_22+
                hour_23*value_gas2*coeff_value_gas2_hour_23+
                (-1*(coeff_value_gas2_hour_1+coeff_value_gas2_hour_2+
                       coeff_value_gas2_hour_3+coeff_value_gas2_hour_4+
                       coeff_value_gas2_hour_5+coeff_value_gas2_hour_6+
                       coeff_value_gas2_hour_7+coeff_value_gas2_hour_8+
                       coeff_value_gas2_hour_9+coeff_value_gas2_hour_10+
                       coeff_value_gas2_hour_11+coeff_value_gas2_hour_12+
                       coeff_value_gas2_hour_13+coeff_value_gas2_hour_14+
                       coeff_value_gas2_hour_15+coeff_value_gas2_hour_16+
                       coeff_value_gas2_hour_17+coeff_value_gas2_hour_18+
                       coeff_value_gas2_hour_19+coeff_value_gas2_hour_20+
                       coeff_value_gas2_hour_21+coeff_value_gas2_hour_22+
                       coeff_value_gas2_hour_23))*hour_24*value_gas2 + # end hour_ value_gas2 interaction --------------
              hour_1*weekend*coeff_weekend_hour_1+
                hour_2*weekend*coeff_weekend_hour_2+
                hour_3*weekend*coeff_weekend_hour_3+
                hour_4*weekend*coeff_weekend_hour_4+
                hour_5*weekend*coeff_weekend_hour_5+
                hour_6*weekend*coeff_weekend_hour_6+
                hour_7*weekend*coeff_weekend_hour_7+
                hour_8*weekend*coeff_weekend_hour_8+
                hour_9*weekend*coeff_weekend_hour_9+
                hour_10*weekend*coeff_weekend_hour_10+
                hour_11*weekend*coeff_weekend_hour_11+
                hour_12*weekend*coeff_weekend_hour_12+
                hour_13*weekend*coeff_weekend_hour_13+
                hour_14*weekend*coeff_weekend_hour_14+
                hour_15*weekend*coeff_weekend_hour_15+
                hour_16*weekend*coeff_weekend_hour_16+
                hour_17*weekend*coeff_weekend_hour_17+
                hour_18*weekend*coeff_weekend_hour_18+
                hour_19*weekend*coeff_weekend_hour_19+
                hour_20*weekend*coeff_weekend_hour_20+
                hour_21*weekend*coeff_weekend_hour_21+
                hour_22*weekend*coeff_weekend_hour_22+
                hour_23*weekend*coeff_weekend_hour_23+
                (-1*(coeff_weekend_hour_1+coeff_weekend_hour_2+
                       coeff_weekend_hour_3+coeff_weekend_hour_4+
                       coeff_weekend_hour_5+coeff_weekend_hour_6+
                       coeff_weekend_hour_7+coeff_weekend_hour_8+
                       coeff_weekend_hour_9+coeff_weekend_hour_10+
                       coeff_weekend_hour_11+coeff_weekend_hour_12+
                       coeff_weekend_hour_13+coeff_weekend_hour_14+
                       coeff_weekend_hour_15+coeff_weekend_hour_16+
                       coeff_weekend_hour_17+coeff_weekend_hour_18+
                       coeff_weekend_hour_19+coeff_weekend_hour_20+
                       coeff_weekend_hour_21+coeff_weekend_hour_22+
                       coeff_weekend_hour_23))*hour_24*weekend + # end hour_ weekend interaction --------------
              hour_1*break_h*coeff_break_h_hour_1+
                hour_2*break_h*coeff_break_h_hour_2+
                hour_3*break_h*coeff_break_h_hour_3+
                hour_4*break_h*coeff_break_h_hour_4+
                hour_5*break_h*coeff_break_h_hour_5+
                hour_6*break_h*coeff_break_h_hour_6+
                hour_7*break_h*coeff_break_h_hour_7+
                hour_8*break_h*coeff_break_h_hour_8+
                hour_9*break_h*coeff_break_h_hour_9+
                hour_10*break_h*coeff_break_h_hour_10+
                hour_11*break_h*coeff_break_h_hour_11+
                hour_12*break_h*coeff_break_h_hour_12+
                hour_13*break_h*coeff_break_h_hour_13+
                hour_14*break_h*coeff_break_h_hour_14+
                hour_15*break_h*coeff_break_h_hour_15+
                hour_16*break_h*coeff_break_h_hour_16+
                hour_17*break_h*coeff_break_h_hour_17+
                hour_18*break_h*coeff_break_h_hour_18+
                hour_19*break_h*coeff_break_h_hour_19+
                hour_20*break_h*coeff_break_h_hour_20+
                hour_21*break_h*coeff_break_h_hour_21+
                hour_22*break_h*coeff_break_h_hour_22+
                hour_23*break_h*coeff_break_h_hour_23+
                (-1*(coeff_break_h_hour_1+coeff_break_h_hour_2+
                       coeff_break_h_hour_3+coeff_break_h_hour_4+
                       coeff_break_h_hour_5+coeff_break_h_hour_6+
                       coeff_break_h_hour_7+coeff_break_h_hour_8+
                       coeff_break_h_hour_9+coeff_break_h_hour_10+
                       coeff_break_h_hour_11+coeff_break_h_hour_12+
                       coeff_break_h_hour_13+coeff_break_h_hour_14+
                       coeff_break_h_hour_15+coeff_break_h_hour_16+
                       coeff_break_h_hour_17+coeff_break_h_hour_18+
                       coeff_break_h_hour_19+coeff_break_h_hour_20+
                       coeff_break_h_hour_21+coeff_break_h_hour_22+
                       coeff_break_h_hour_23))*hour_24*break_h, # end hour_ break_group_h interaction --------------
              start=list(coeff_season_winter=0, coeff_season_spring=0, coeff_season_summer=0, coeff_season_fall=0, coeff_hour_1=0,coeff_hour_2=0,coeff_hour_3=0,coeff_hour_4=0,coeff_hour_5=0,coeff_hour_6=0,coeff_hour_7=0,coeff_hour_8=0,coeff_hour_9=0,coeff_hour_10=0,coeff_hour_11=0,coeff_hour_12=0,coeff_hour_13=0,
                         coeff_hour_14=0,coeff_hour_15=0,coeff_hour_16=0,coeff_hour_17=0,coeff_hour_18=0,coeff_hour_19=0,coeff_hour_20=0,coeff_hour_21=0,coeff_hour_22=0,coeff_hour_23=0,
                         coeff_bl_hour_1=0,coeff_bl_hour_2=0,coeff_bl_hour_3=0,coeff_bl_hour_4=0,coeff_bl_hour_5=0,coeff_bl_hour_6=0,coeff_bl_hour_7=0,coeff_bl_hour_8=0,coeff_bl_hour_9=0,coeff_bl_hour_10=0,coeff_bl_hour_11=0,coeff_bl_hour_12=0,coeff_bl_hour_13=0,
                         coeff_bl_hour_14=0,coeff_bl_hour_15=0,coeff_bl_hour_16=0,coeff_bl_hour_17=0,coeff_bl_hour_18=0,coeff_bl_hour_19=0,coeff_bl_hour_20=0,coeff_bl_hour_21=0,coeff_bl_hour_22=0,coeff_bl_hour_23=0,
                         coeff_bl2_hour_1=0,coeff_bl2_hour_2=0,coeff_bl2_hour_3=0,coeff_bl2_hour_4=0,coeff_bl2_hour_5=0,coeff_bl2_hour_6=0,coeff_bl2_hour_7=0,coeff_bl2_hour_8=0,coeff_bl2_hour_9=0,coeff_bl2_hour_10=0,coeff_bl2_hour_11=0,coeff_bl2_hour_12=0,coeff_bl2_hour_13=0,
                         coeff_bl2_hour_14=0,coeff_bl2_hour_15=0,coeff_bl2_hour_16=0,coeff_bl2_hour_17=0,coeff_bl2_hour_18=0,coeff_bl2_hour_19=0,coeff_bl2_hour_20=0,coeff_bl2_hour_21=0,coeff_bl2_hour_22=0,coeff_bl2_hour_23=0,
                         coeff_bl3_hour_1=0,coeff_bl3_hour_2=0,coeff_bl3_hour_3=0,coeff_bl3_hour_4=0,coeff_bl3_hour_5=0,coeff_bl3_hour_6=0,coeff_bl3_hour_7=0,coeff_bl3_hour_8=0,coeff_bl3_hour_9=0,coeff_bl3_hour_10=0,coeff_bl3_hour_11=0,coeff_bl3_hour_12=0,coeff_bl3_hour_13=0,
                         coeff_bl3_hour_14=0,coeff_bl3_hour_15=0,coeff_bl3_hour_16=0,coeff_bl3_hour_17=0,coeff_bl3_hour_18=0,coeff_bl3_hour_19=0,coeff_bl3_hour_20=0,coeff_bl3_hour_21=0,coeff_bl3_hour_22=0,coeff_bl3_hour_23=0,
                         coeff_yday_hour_1=0,coeff_yday_hour_2=0,coeff_yday_hour_3=0,coeff_yday_hour_4=0,coeff_yday_hour_5=0,coeff_yday_hour_6=0,coeff_yday_hour_7=0,coeff_yday_hour_8=0,coeff_yday_hour_9=0,coeff_yday_hour_10=0,coeff_yday_hour_11=0,coeff_yday_hour_12=0,coeff_yday_hour_13=0,
                         coeff_yday_hour_14=0,coeff_yday_hour_15=0,coeff_yday_hour_16=0,coeff_yday_hour_17=0,coeff_yday_hour_18=0,coeff_yday_hour_19=0,coeff_yday_hour_20=0,coeff_yday_hour_21=0,coeff_yday_hour_22=0,coeff_yday_hour_23=0,
                         coeff_yday2_hour_1=0,coeff_yday2_hour_2=0,coeff_yday2_hour_3=0,coeff_yday2_hour_4=0,coeff_yday2_hour_5=0,coeff_yday2_hour_6=0,coeff_yday2_hour_7=0,coeff_yday2_hour_8=0,coeff_yday2_hour_9=0,coeff_yday2_hour_10=0,coeff_yday2_hour_11=0,coeff_yday2_hour_12=0,coeff_yday2_hour_13=0,
                         coeff_yday2_hour_14=0,coeff_yday2_hour_15=0,coeff_yday2_hour_16=0,coeff_yday2_hour_17=0,coeff_yday2_hour_18=0,coeff_yday2_hour_19=0,coeff_yday2_hour_20=0,coeff_yday2_hour_21=0,coeff_yday2_hour_22=0,coeff_yday2_hour_23=0,
                         coeff_yday3_hour_1=0,coeff_yday3_hour_2=0,coeff_yday3_hour_3=0,coeff_yday3_hour_4=0,coeff_yday3_hour_5=0,coeff_yday3_hour_6=0,coeff_yday3_hour_7=0,coeff_yday3_hour_8=0,coeff_yday3_hour_9=0,coeff_yday3_hour_10=0,coeff_yday3_hour_11=0,coeff_yday3_hour_12=0,coeff_yday3_hour_13=0,
                         coeff_yday3_hour_14=0,coeff_yday3_hour_15=0,coeff_yday3_hour_16=0,coeff_yday3_hour_17=0,coeff_yday3_hour_18=0,coeff_yday3_hour_19=0,coeff_yday3_hour_20=0,coeff_yday3_hour_21=0,coeff_yday3_hour_22=0,coeff_yday3_hour_23=0,
                         coeff_value_gas_hour_1=0,coeff_value_gas_hour_2=0,coeff_value_gas_hour_3=0,coeff_value_gas_hour_4=0,coeff_value_gas_hour_5=0,coeff_value_gas_hour_6=0,coeff_value_gas_hour_7=0,coeff_value_gas_hour_8=0,coeff_value_gas_hour_9=0,coeff_value_gas_hour_10=0,coeff_value_gas_hour_11=0,coeff_value_gas_hour_12=0,coeff_value_gas_hour_13=0,
                         coeff_value_gas_hour_14=0,coeff_value_gas_hour_15=0,coeff_value_gas_hour_16=0,coeff_value_gas_hour_17=0,coeff_value_gas_hour_18=0,coeff_value_gas_hour_19=0,coeff_value_gas_hour_20=0,coeff_value_gas_hour_21=0,coeff_value_gas_hour_22=0,coeff_value_gas_hour_23=0,
                         coeff_value_gas2_hour_1=0,coeff_value_gas2_hour_2=0,coeff_value_gas2_hour_3=0,coeff_value_gas2_hour_4=0,coeff_value_gas2_hour_5=0,coeff_value_gas2_hour_6=0,coeff_value_gas2_hour_7=0,coeff_value_gas2_hour_8=0,coeff_value_gas2_hour_9=0,coeff_value_gas2_hour_10=0,coeff_value_gas2_hour_11=0,coeff_value_gas2_hour_12=0,coeff_value_gas2_hour_13=0,
                         coeff_value_gas2_hour_14=0,coeff_value_gas2_hour_15=0,coeff_value_gas2_hour_16=0,coeff_value_gas2_hour_17=0,coeff_value_gas2_hour_18=0,coeff_value_gas2_hour_19=0,coeff_value_gas2_hour_20=0,coeff_value_gas2_hour_21=0,coeff_value_gas2_hour_22=0,coeff_value_gas2_hour_23=0,
                         coeff_weekend_hour_1=0,coeff_weekend_hour_2=0,coeff_weekend_hour_3=0,coeff_weekend_hour_4=0,coeff_weekend_hour_5=0,coeff_weekend_hour_6=0,coeff_weekend_hour_7=0,coeff_weekend_hour_8=0,coeff_weekend_hour_9=0,coeff_weekend_hour_10=0,coeff_weekend_hour_11=0,coeff_weekend_hour_12=0,coeff_weekend_hour_13=0,
                         coeff_weekend_hour_14=0,coeff_weekend_hour_15=0,coeff_weekend_hour_16=0,coeff_weekend_hour_17=0,coeff_weekend_hour_18=0,coeff_weekend_hour_19=0,coeff_weekend_hour_20=0,coeff_weekend_hour_21=0,coeff_weekend_hour_22=0,coeff_weekend_hour_23=0,
                         coeff_break_h_hour_1=0,coeff_break_h_hour_2=0,coeff_break_h_hour_3=0,coeff_break_h_hour_4=0,coeff_break_h_hour_5=0,coeff_break_h_hour_6=0,coeff_break_h_hour_7=0,coeff_break_h_hour_8=0,coeff_break_h_hour_9=0,coeff_break_h_hour_10=0,coeff_break_h_hour_11=0,coeff_break_h_hour_12=0,coeff_break_h_hour_13=0,
                         coeff_break_h_hour_14=0,coeff_break_h_hour_15=0,coeff_break_h_hour_16=0,coeff_break_h_hour_17=0,coeff_break_h_hour_18=0,coeff_break_h_hour_19=0,coeff_break_h_hour_20=0,coeff_break_h_hour_21=0,coeff_break_h_hour_22=0,coeff_break_h_hour_23=0),
              data = DTW,
              control=  nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
                                    printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
                                    nDcentral = FALSE))
  
  return(model_h)
  
}





#' Predict Short-Term model - PWR
#'
#' This function fits a nonlinear regression model to estimate short-term effects in power prices.
#' The model captures hourly patterns, seasonal effects, and interactions with gas prices.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `value_h` (numeric): Detrended power price.
#'   - `value_gas` (numeric): Gas price variable.
#'   - `hour_1` to `hour_24` (numeric): Hourly dummies.
#'   - `weekend`, `break_h` (numeric): Weekend and market regime indicators.
#' @return A nonlinear least squares (NLS) model.
#' @import data.table
#' @importFrom stats nls nls.control
#' @export

train_st_model_pwr_v1 = function(DT){
  
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Required columns for the model
  required_cols = c("value_h", "value_gas", paste0("hour_", 1:24), "weekend", "break_h")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Copy DT to avoid modifying the original
  DTW = copy(DT)
  
  DTW[, value_gas2 := value_gas^2]
  
  nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
              printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
              nDcentral = FALSE)
  
  model_h=nls(value_h ~
                hour_1*coeff_hour_1+
                hour_2*coeff_hour_2+
                hour_3*coeff_hour_3+
                hour_4*coeff_hour_4+
                hour_5*coeff_hour_5+
                hour_6*coeff_hour_6+
                hour_7*coeff_hour_7+
                hour_8*coeff_hour_8+
                hour_9*coeff_hour_9+
                hour_10*coeff_hour_10+
                hour_11*coeff_hour_11+
                hour_12*coeff_hour_12+
                hour_13*coeff_hour_13+
                hour_14*coeff_hour_14+
                hour_15*coeff_hour_15+
                hour_16*coeff_hour_16+
                hour_17*coeff_hour_17+
                hour_18*coeff_hour_18+
                hour_19*coeff_hour_19+
                hour_20*coeff_hour_20+
                hour_21*coeff_hour_21+
                hour_22*coeff_hour_22+
                hour_23*coeff_hour_23+
                (-1*(coeff_hour_1+coeff_hour_2+
                       coeff_hour_3+coeff_hour_4+
                       coeff_hour_5+coeff_hour_6+
                       coeff_hour_7+coeff_hour_8+
                       coeff_hour_9+coeff_hour_10+
                       coeff_hour_11+coeff_hour_12+
                       coeff_hour_13+coeff_hour_14+
                       coeff_hour_15+coeff_hour_16+
                       coeff_hour_17+coeff_hour_18+
                       coeff_hour_19+coeff_hour_20+
                       coeff_hour_21+coeff_hour_22+
                       coeff_hour_23))*hour_24+ # end hour_ dummy --------------
              hour_1*bl*coeff_bl_hour_1+
                hour_2*bl*coeff_bl_hour_2+
                hour_3*bl*coeff_bl_hour_3+
                hour_4*bl*coeff_bl_hour_4+
                hour_5*bl*coeff_bl_hour_5+
                hour_6*bl*coeff_bl_hour_6+
                hour_7*bl*coeff_bl_hour_7+
                hour_8*bl*coeff_bl_hour_8+
                hour_9*bl*coeff_bl_hour_9+
                hour_10*bl*coeff_bl_hour_10+
                hour_11*bl*coeff_bl_hour_11+
                hour_12*bl*coeff_bl_hour_12+
                hour_13*bl*coeff_bl_hour_13+
                hour_14*bl*coeff_bl_hour_14+
                hour_15*bl*coeff_bl_hour_15+
                hour_16*bl*coeff_bl_hour_16+
                hour_17*bl*coeff_bl_hour_17+
                hour_18*bl*coeff_bl_hour_18+
                hour_19*bl*coeff_bl_hour_19+
                hour_20*bl*coeff_bl_hour_20+
                hour_21*bl*coeff_bl_hour_21+
                hour_22*bl*coeff_bl_hour_22+
                hour_23*bl*coeff_bl_hour_23+
                (-1*(coeff_bl_hour_1+coeff_bl_hour_2+
                       coeff_bl_hour_3+coeff_bl_hour_4+
                       coeff_bl_hour_5+coeff_bl_hour_6+
                       coeff_bl_hour_7+coeff_bl_hour_8+
                       coeff_bl_hour_9+coeff_bl_hour_10+
                       coeff_bl_hour_11+coeff_bl_hour_12+
                       coeff_bl_hour_13+coeff_bl_hour_14+
                       coeff_bl_hour_15+coeff_bl_hour_16+
                       coeff_bl_hour_17+coeff_bl_hour_18+
                       coeff_bl_hour_19+coeff_bl_hour_20+
                       coeff_bl_hour_21+coeff_bl_hour_22+
                       coeff_bl_hour_23))*hour_24*bl + # end hour_ bl interaction --------------
              hour_1*bl2*coeff_bl2_hour_1+
                hour_2*bl2*coeff_bl2_hour_2+
                hour_3*bl2*coeff_bl2_hour_3+
                hour_4*bl2*coeff_bl2_hour_4+
                hour_5*bl2*coeff_bl2_hour_5+
                hour_6*bl2*coeff_bl2_hour_6+
                hour_7*bl2*coeff_bl2_hour_7+
                hour_8*bl2*coeff_bl2_hour_8+
                hour_9*bl2*coeff_bl2_hour_9+
                hour_10*bl2*coeff_bl2_hour_10+
                hour_11*bl2*coeff_bl2_hour_11+
                hour_12*bl2*coeff_bl2_hour_12+
                hour_13*bl2*coeff_bl2_hour_13+
                hour_14*bl2*coeff_bl2_hour_14+
                hour_15*bl2*coeff_bl2_hour_15+
                hour_16*bl2*coeff_bl2_hour_16+
                hour_17*bl2*coeff_bl2_hour_17+
                hour_18*bl2*coeff_bl2_hour_18+
                hour_19*bl2*coeff_bl2_hour_19+
                hour_20*bl2*coeff_bl2_hour_20+
                hour_21*bl2*coeff_bl2_hour_21+
                hour_22*bl2*coeff_bl2_hour_22+
                hour_23*bl2*coeff_bl2_hour_23+
                (-1*(coeff_bl2_hour_1+coeff_bl2_hour_2+
                       coeff_bl2_hour_3+coeff_bl2_hour_4+
                       coeff_bl2_hour_5+coeff_bl2_hour_6+
                       coeff_bl2_hour_7+coeff_bl2_hour_8+
                       coeff_bl2_hour_9+coeff_bl2_hour_10+
                       coeff_bl2_hour_11+coeff_bl2_hour_12+
                       coeff_bl2_hour_13+coeff_bl2_hour_14+
                       coeff_bl2_hour_15+coeff_bl2_hour_16+
                       coeff_bl2_hour_17+coeff_bl2_hour_18+
                       coeff_bl2_hour_19+coeff_bl2_hour_20+
                       coeff_bl2_hour_21+coeff_bl2_hour_22+
                       coeff_bl2_hour_23))*hour_24*bl2 + # end hour_ bl2 interaction --------------
              hour_1*bl3*coeff_bl3_hour_1+
                hour_2*bl3*coeff_bl3_hour_2+
                hour_3*bl3*coeff_bl3_hour_3+
                hour_4*bl3*coeff_bl3_hour_4+
                hour_5*bl3*coeff_bl3_hour_5+
                hour_6*bl3*coeff_bl3_hour_6+
                hour_7*bl3*coeff_bl3_hour_7+
                hour_8*bl3*coeff_bl3_hour_8+
                hour_9*bl3*coeff_bl3_hour_9+
                hour_10*bl3*coeff_bl3_hour_10+
                hour_11*bl3*coeff_bl3_hour_11+
                hour_12*bl3*coeff_bl3_hour_12+
                hour_13*bl3*coeff_bl3_hour_13+
                hour_14*bl3*coeff_bl3_hour_14+
                hour_15*bl3*coeff_bl3_hour_15+
                hour_16*bl3*coeff_bl3_hour_16+
                hour_17*bl3*coeff_bl3_hour_17+
                hour_18*bl3*coeff_bl3_hour_18+
                hour_19*bl3*coeff_bl3_hour_19+
                hour_20*bl3*coeff_bl3_hour_20+
                hour_21*bl3*coeff_bl3_hour_21+
                hour_22*bl3*coeff_bl3_hour_22+
                hour_23*bl3*coeff_bl3_hour_23+
                (-1*(coeff_bl3_hour_1+coeff_bl3_hour_2+
                       coeff_bl3_hour_3+coeff_bl3_hour_4+
                       coeff_bl3_hour_5+coeff_bl3_hour_6+
                       coeff_bl3_hour_7+coeff_bl3_hour_8+
                       coeff_bl3_hour_9+coeff_bl3_hour_10+
                       coeff_bl3_hour_11+coeff_bl3_hour_12+
                       coeff_bl3_hour_13+coeff_bl3_hour_14+
                       coeff_bl3_hour_15+coeff_bl3_hour_16+
                       coeff_bl3_hour_17+coeff_bl3_hour_18+
                       coeff_bl3_hour_19+coeff_bl3_hour_20+
                       coeff_bl3_hour_21+coeff_bl3_hour_22+
                       coeff_bl3_hour_23))*hour_24*bl3 + # end hour_ bl3 interaction --------------
              # NEW SEASONAL REGRESSORS
              season_winter*coeff_season_winter +
                season_summer*coeff_season_summer +
                hour_1*yday*coeff_yday_hour_1+
                hour_2*yday*coeff_yday_hour_2+
                hour_3*yday*coeff_yday_hour_3+
                hour_4*yday*coeff_yday_hour_4+
                hour_5*yday*coeff_yday_hour_5+
                hour_6*yday*coeff_yday_hour_6+
                hour_7*yday*coeff_yday_hour_7+
                hour_8*yday*coeff_yday_hour_8+
                hour_9*yday*coeff_yday_hour_9+
                hour_10*yday*coeff_yday_hour_10+
                hour_11*yday*coeff_yday_hour_11+
                hour_12*yday*coeff_yday_hour_12+
                hour_13*yday*coeff_yday_hour_13+
                hour_14*yday*coeff_yday_hour_14+
                hour_15*yday*coeff_yday_hour_15+
                hour_16*yday*coeff_yday_hour_16+
                hour_17*yday*coeff_yday_hour_17+
                hour_18*yday*coeff_yday_hour_18+
                hour_19*yday*coeff_yday_hour_19+
                hour_20*yday*coeff_yday_hour_20+
                hour_21*yday*coeff_yday_hour_21+
                hour_22*yday*coeff_yday_hour_22+
                hour_23*yday*coeff_yday_hour_23+
                (-1*(coeff_yday_hour_1+coeff_yday_hour_2+
                       coeff_yday_hour_3+coeff_yday_hour_4+
                       coeff_yday_hour_5+coeff_yday_hour_6+
                       coeff_yday_hour_7+coeff_yday_hour_8+
                       coeff_yday_hour_9+coeff_yday_hour_10+
                       coeff_yday_hour_11+coeff_yday_hour_12+
                       coeff_yday_hour_13+coeff_yday_hour_14+
                       coeff_yday_hour_15+coeff_yday_hour_16+
                       coeff_yday_hour_17+coeff_yday_hour_18+
                       coeff_yday_hour_19+coeff_yday_hour_20+
                       coeff_yday_hour_21+coeff_yday_hour_22+
                       coeff_yday_hour_23))*hour_24*yday + # end hour_ yday interaction --------------
              hour_1*yday2*coeff_yday2_hour_1+
                hour_2*yday2*coeff_yday2_hour_2+
                hour_3*yday2*coeff_yday2_hour_3+
                hour_4*yday2*coeff_yday2_hour_4+
                hour_5*yday2*coeff_yday2_hour_5+
                hour_6*yday2*coeff_yday2_hour_6+
                hour_7*yday2*coeff_yday2_hour_7+
                hour_8*yday2*coeff_yday2_hour_8+
                hour_9*yday2*coeff_yday2_hour_9+
                hour_10*yday2*coeff_yday2_hour_10+
                hour_11*yday2*coeff_yday2_hour_11+
                hour_12*yday2*coeff_yday2_hour_12+
                hour_13*yday2*coeff_yday2_hour_13+
                hour_14*yday2*coeff_yday2_hour_14+
                hour_15*yday2*coeff_yday2_hour_15+
                hour_16*yday2*coeff_yday2_hour_16+
                hour_17*yday2*coeff_yday2_hour_17+
                hour_18*yday2*coeff_yday2_hour_18+
                hour_19*yday2*coeff_yday2_hour_19+
                hour_20*yday2*coeff_yday2_hour_20+
                hour_21*yday2*coeff_yday2_hour_21+
                hour_22*yday2*coeff_yday2_hour_22+
                hour_23*yday2*coeff_yday2_hour_23+
                (-1*(coeff_yday2_hour_1+coeff_yday2_hour_2+
                       coeff_yday2_hour_3+coeff_yday2_hour_4+
                       coeff_yday2_hour_5+coeff_yday2_hour_6+
                       coeff_yday2_hour_7+coeff_yday2_hour_8+
                       coeff_yday2_hour_9+coeff_yday2_hour_10+
                       coeff_yday2_hour_11+coeff_yday2_hour_12+
                       coeff_yday2_hour_13+coeff_yday2_hour_14+
                       coeff_yday2_hour_15+coeff_yday2_hour_16+
                       coeff_yday2_hour_17+coeff_yday2_hour_18+
                       coeff_yday2_hour_19+coeff_yday2_hour_20+
                       coeff_yday2_hour_21+coeff_yday2_hour_22+
                       coeff_yday2_hour_23))*hour_24*yday2 + # end hour_ yday2 interaction --------------
              hour_1*yday3*coeff_yday3_hour_1+
                hour_2*yday3*coeff_yday3_hour_2+
                hour_3*yday3*coeff_yday3_hour_3+
                hour_4*yday3*coeff_yday3_hour_4+
                hour_5*yday3*coeff_yday3_hour_5+
                hour_6*yday3*coeff_yday3_hour_6+
                hour_7*yday3*coeff_yday3_hour_7+
                hour_8*yday3*coeff_yday3_hour_8+
                hour_9*yday3*coeff_yday3_hour_9+
                hour_10*yday3*coeff_yday3_hour_10+
                hour_11*yday3*coeff_yday3_hour_11+
                hour_12*yday3*coeff_yday3_hour_12+
                hour_13*yday3*coeff_yday3_hour_13+
                hour_14*yday3*coeff_yday3_hour_14+
                hour_15*yday3*coeff_yday3_hour_15+
                hour_16*yday3*coeff_yday3_hour_16+
                hour_17*yday3*coeff_yday3_hour_17+
                hour_18*yday3*coeff_yday3_hour_18+
                hour_19*yday3*coeff_yday3_hour_19+
                hour_20*yday3*coeff_yday3_hour_20+
                hour_21*yday3*coeff_yday3_hour_21+
                hour_22*yday3*coeff_yday3_hour_22+
                hour_23*yday3*coeff_yday3_hour_23+
                (-1*(coeff_yday3_hour_1+coeff_yday3_hour_2+
                       coeff_yday3_hour_3+coeff_yday3_hour_4+
                       coeff_yday3_hour_5+coeff_yday3_hour_6+
                       coeff_yday3_hour_7+coeff_yday3_hour_8+
                       coeff_yday3_hour_9+coeff_yday3_hour_10+
                       coeff_yday3_hour_11+coeff_yday3_hour_12+
                       coeff_yday3_hour_13+coeff_yday3_hour_14+
                       coeff_yday3_hour_15+coeff_yday3_hour_16+
                       coeff_yday3_hour_17+coeff_yday3_hour_18+
                       coeff_yday3_hour_19+coeff_yday3_hour_20+
                       coeff_yday3_hour_21+coeff_yday3_hour_22+
                       coeff_yday3_hour_23))*hour_24*yday3 + # end hour_ yday3 interaction --------------
              hour_1*value_gas*coeff_value_gas_hour_1+
                hour_2*value_gas*coeff_value_gas_hour_2+
                hour_3*value_gas*coeff_value_gas_hour_3+
                hour_4*value_gas*coeff_value_gas_hour_4+
                hour_5*value_gas*coeff_value_gas_hour_5+
                hour_6*value_gas*coeff_value_gas_hour_6+
                hour_7*value_gas*coeff_value_gas_hour_7+
                hour_8*value_gas*coeff_value_gas_hour_8+
                hour_9*value_gas*coeff_value_gas_hour_9+
                hour_10*value_gas*coeff_value_gas_hour_10+
                hour_11*value_gas*coeff_value_gas_hour_11+
                hour_12*value_gas*coeff_value_gas_hour_12+
                hour_13*value_gas*coeff_value_gas_hour_13+
                hour_14*value_gas*coeff_value_gas_hour_14+
                hour_15*value_gas*coeff_value_gas_hour_15+
                hour_16*value_gas*coeff_value_gas_hour_16+
                hour_17*value_gas*coeff_value_gas_hour_17+
                hour_18*value_gas*coeff_value_gas_hour_18+
                hour_19*value_gas*coeff_value_gas_hour_19+
                hour_20*value_gas*coeff_value_gas_hour_20+
                hour_21*value_gas*coeff_value_gas_hour_21+
                hour_22*value_gas*coeff_value_gas_hour_22+
                hour_23*value_gas*coeff_value_gas_hour_23+
                (-1*(coeff_value_gas_hour_1+coeff_value_gas_hour_2+
                       coeff_value_gas_hour_3+coeff_value_gas_hour_4+
                       coeff_value_gas_hour_5+coeff_value_gas_hour_6+
                       coeff_value_gas_hour_7+coeff_value_gas_hour_8+
                       coeff_value_gas_hour_9+coeff_value_gas_hour_10+
                       coeff_value_gas_hour_11+coeff_value_gas_hour_12+
                       coeff_value_gas_hour_13+coeff_value_gas_hour_14+
                       coeff_value_gas_hour_15+coeff_value_gas_hour_16+
                       coeff_value_gas_hour_17+coeff_value_gas_hour_18+
                       coeff_value_gas_hour_19+coeff_value_gas_hour_20+
                       coeff_value_gas_hour_21+coeff_value_gas_hour_22+
                       coeff_value_gas_hour_23))*hour_24*value_gas + # end hour_ value_gas interaction --------------
              hour_1*value_gas2*coeff_value_gas2_hour_1+
                hour_2*value_gas2*coeff_value_gas2_hour_2+
                hour_3*value_gas2*coeff_value_gas2_hour_3+
                hour_4*value_gas2*coeff_value_gas2_hour_4+
                hour_5*value_gas2*coeff_value_gas2_hour_5+
                hour_6*value_gas2*coeff_value_gas2_hour_6+
                hour_7*value_gas2*coeff_value_gas2_hour_7+
                hour_8*value_gas2*coeff_value_gas2_hour_8+
                hour_9*value_gas2*coeff_value_gas2_hour_9+
                hour_10*value_gas2*coeff_value_gas2_hour_10+
                hour_11*value_gas2*coeff_value_gas2_hour_11+
                hour_12*value_gas2*coeff_value_gas2_hour_12+
                hour_13*value_gas2*coeff_value_gas2_hour_13+
                hour_14*value_gas2*coeff_value_gas2_hour_14+
                hour_15*value_gas2*coeff_value_gas2_hour_15+
                hour_16*value_gas2*coeff_value_gas2_hour_16+
                hour_17*value_gas2*coeff_value_gas2_hour_17+
                hour_18*value_gas2*coeff_value_gas2_hour_18+
                hour_19*value_gas2*coeff_value_gas2_hour_19+
                hour_20*value_gas2*coeff_value_gas2_hour_20+
                hour_21*value_gas2*coeff_value_gas2_hour_21+
                hour_22*value_gas2*coeff_value_gas2_hour_22+
                hour_23*value_gas2*coeff_value_gas2_hour_23+
                (-1*(coeff_value_gas2_hour_1+coeff_value_gas2_hour_2+
                       coeff_value_gas2_hour_3+coeff_value_gas2_hour_4+
                       coeff_value_gas2_hour_5+coeff_value_gas2_hour_6+
                       coeff_value_gas2_hour_7+coeff_value_gas2_hour_8+
                       coeff_value_gas2_hour_9+coeff_value_gas2_hour_10+
                       coeff_value_gas2_hour_11+coeff_value_gas2_hour_12+
                       coeff_value_gas2_hour_13+coeff_value_gas2_hour_14+
                       coeff_value_gas2_hour_15+coeff_value_gas2_hour_16+
                       coeff_value_gas2_hour_17+coeff_value_gas2_hour_18+
                       coeff_value_gas2_hour_19+coeff_value_gas2_hour_20+
                       coeff_value_gas2_hour_21+coeff_value_gas2_hour_22+
                       coeff_value_gas2_hour_23))*hour_24*value_gas2 + # end hour_ value_gas2 interaction --------------
              hour_1*weekend*coeff_weekend_hour_1+
                hour_2*weekend*coeff_weekend_hour_2+
                hour_3*weekend*coeff_weekend_hour_3+
                hour_4*weekend*coeff_weekend_hour_4+
                hour_5*weekend*coeff_weekend_hour_5+
                hour_6*weekend*coeff_weekend_hour_6+
                hour_7*weekend*coeff_weekend_hour_7+
                hour_8*weekend*coeff_weekend_hour_8+
                hour_9*weekend*coeff_weekend_hour_9+
                hour_10*weekend*coeff_weekend_hour_10+
                hour_11*weekend*coeff_weekend_hour_11+
                hour_12*weekend*coeff_weekend_hour_12+
                hour_13*weekend*coeff_weekend_hour_13+
                hour_14*weekend*coeff_weekend_hour_14+
                hour_15*weekend*coeff_weekend_hour_15+
                hour_16*weekend*coeff_weekend_hour_16+
                hour_17*weekend*coeff_weekend_hour_17+
                hour_18*weekend*coeff_weekend_hour_18+
                hour_19*weekend*coeff_weekend_hour_19+
                hour_20*weekend*coeff_weekend_hour_20+
                hour_21*weekend*coeff_weekend_hour_21+
                hour_22*weekend*coeff_weekend_hour_22+
                hour_23*weekend*coeff_weekend_hour_23+
                (-1*(coeff_weekend_hour_1+coeff_weekend_hour_2+
                       coeff_weekend_hour_3+coeff_weekend_hour_4+
                       coeff_weekend_hour_5+coeff_weekend_hour_6+
                       coeff_weekend_hour_7+coeff_weekend_hour_8+
                       coeff_weekend_hour_9+coeff_weekend_hour_10+
                       coeff_weekend_hour_11+coeff_weekend_hour_12+
                       coeff_weekend_hour_13+coeff_weekend_hour_14+
                       coeff_weekend_hour_15+coeff_weekend_hour_16+
                       coeff_weekend_hour_17+coeff_weekend_hour_18+
                       coeff_weekend_hour_19+coeff_weekend_hour_20+
                       coeff_weekend_hour_21+coeff_weekend_hour_22+
                       coeff_weekend_hour_23))*hour_24*weekend + # end hour_ weekend interaction --------------
              hour_1*break_h*coeff_break_h_hour_1+
                hour_2*break_h*coeff_break_h_hour_2+
                hour_3*break_h*coeff_break_h_hour_3+
                hour_4*break_h*coeff_break_h_hour_4+
                hour_5*break_h*coeff_break_h_hour_5+
                hour_6*break_h*coeff_break_h_hour_6+
                hour_7*break_h*coeff_break_h_hour_7+
                hour_8*break_h*coeff_break_h_hour_8+
                hour_9*break_h*coeff_break_h_hour_9+
                hour_10*break_h*coeff_break_h_hour_10+
                hour_11*break_h*coeff_break_h_hour_11+
                hour_12*break_h*coeff_break_h_hour_12+
                hour_13*break_h*coeff_break_h_hour_13+
                hour_14*break_h*coeff_break_h_hour_14+
                hour_15*break_h*coeff_break_h_hour_15+
                hour_16*break_h*coeff_break_h_hour_16+
                hour_17*break_h*coeff_break_h_hour_17+
                hour_18*break_h*coeff_break_h_hour_18+
                hour_19*break_h*coeff_break_h_hour_19+
                hour_20*break_h*coeff_break_h_hour_20+
                hour_21*break_h*coeff_break_h_hour_21+
                hour_22*break_h*coeff_break_h_hour_22+
                hour_23*break_h*coeff_break_h_hour_23+
                (-1*(coeff_break_h_hour_1+coeff_break_h_hour_2+
                       coeff_break_h_hour_3+coeff_break_h_hour_4+
                       coeff_break_h_hour_5+coeff_break_h_hour_6+
                       coeff_break_h_hour_7+coeff_break_h_hour_8+
                       coeff_break_h_hour_9+coeff_break_h_hour_10+
                       coeff_break_h_hour_11+coeff_break_h_hour_12+
                       coeff_break_h_hour_13+coeff_break_h_hour_14+
                       coeff_break_h_hour_15+coeff_break_h_hour_16+
                       coeff_break_h_hour_17+coeff_break_h_hour_18+
                       coeff_break_h_hour_19+coeff_break_h_hour_20+
                       coeff_break_h_hour_21+coeff_break_h_hour_22+
                       coeff_break_h_hour_23))*hour_24*break_h, # end hour_ break_group_h interaction --------------
              start=list(coeff_season_winter=0, coeff_season_summer=0, coeff_hour_1=0,coeff_hour_2=0,coeff_hour_3=0,coeff_hour_4=0,coeff_hour_5=0,coeff_hour_6=0,coeff_hour_7=0,coeff_hour_8=0,coeff_hour_9=0,coeff_hour_10=0,coeff_hour_11=0,coeff_hour_12=0,coeff_hour_13=0,
                         coeff_hour_14=0,coeff_hour_15=0,coeff_hour_16=0,coeff_hour_17=0,coeff_hour_18=0,coeff_hour_19=0,coeff_hour_20=0,coeff_hour_21=0,coeff_hour_22=0,coeff_hour_23=0,
                         coeff_bl_hour_1=0,coeff_bl_hour_2=0,coeff_bl_hour_3=0,coeff_bl_hour_4=0,coeff_bl_hour_5=0,coeff_bl_hour_6=0,coeff_bl_hour_7=0,coeff_bl_hour_8=0,coeff_bl_hour_9=0,coeff_bl_hour_10=0,coeff_bl_hour_11=0,coeff_bl_hour_12=0,coeff_bl_hour_13=0,
                         coeff_bl_hour_14=0,coeff_bl_hour_15=0,coeff_bl_hour_16=0,coeff_bl_hour_17=0,coeff_bl_hour_18=0,coeff_bl_hour_19=0,coeff_bl_hour_20=0,coeff_bl_hour_21=0,coeff_bl_hour_22=0,coeff_bl_hour_23=0,
                         coeff_bl2_hour_1=0,coeff_bl2_hour_2=0,coeff_bl2_hour_3=0,coeff_bl2_hour_4=0,coeff_bl2_hour_5=0,coeff_bl2_hour_6=0,coeff_bl2_hour_7=0,coeff_bl2_hour_8=0,coeff_bl2_hour_9=0,coeff_bl2_hour_10=0,coeff_bl2_hour_11=0,coeff_bl2_hour_12=0,coeff_bl2_hour_13=0,
                         coeff_bl2_hour_14=0,coeff_bl2_hour_15=0,coeff_bl2_hour_16=0,coeff_bl2_hour_17=0,coeff_bl2_hour_18=0,coeff_bl2_hour_19=0,coeff_bl2_hour_20=0,coeff_bl2_hour_21=0,coeff_bl2_hour_22=0,coeff_bl2_hour_23=0,
                         coeff_bl3_hour_1=0,coeff_bl3_hour_2=0,coeff_bl3_hour_3=0,coeff_bl3_hour_4=0,coeff_bl3_hour_5=0,coeff_bl3_hour_6=0,coeff_bl3_hour_7=0,coeff_bl3_hour_8=0,coeff_bl3_hour_9=0,coeff_bl3_hour_10=0,coeff_bl3_hour_11=0,coeff_bl3_hour_12=0,coeff_bl3_hour_13=0,
                         coeff_bl3_hour_14=0,coeff_bl3_hour_15=0,coeff_bl3_hour_16=0,coeff_bl3_hour_17=0,coeff_bl3_hour_18=0,coeff_bl3_hour_19=0,coeff_bl3_hour_20=0,coeff_bl3_hour_21=0,coeff_bl3_hour_22=0,coeff_bl3_hour_23=0,
                         coeff_yday_hour_1=0,coeff_yday_hour_2=0,coeff_yday_hour_3=0,coeff_yday_hour_4=0,coeff_yday_hour_5=0,coeff_yday_hour_6=0,coeff_yday_hour_7=0,coeff_yday_hour_8=0,coeff_yday_hour_9=0,coeff_yday_hour_10=0,coeff_yday_hour_11=0,coeff_yday_hour_12=0,coeff_yday_hour_13=0,
                         coeff_yday_hour_14=0,coeff_yday_hour_15=0,coeff_yday_hour_16=0,coeff_yday_hour_17=0,coeff_yday_hour_18=0,coeff_yday_hour_19=0,coeff_yday_hour_20=0,coeff_yday_hour_21=0,coeff_yday_hour_22=0,coeff_yday_hour_23=0,
                         coeff_yday2_hour_1=0,coeff_yday2_hour_2=0,coeff_yday2_hour_3=0,coeff_yday2_hour_4=0,coeff_yday2_hour_5=0,coeff_yday2_hour_6=0,coeff_yday2_hour_7=0,coeff_yday2_hour_8=0,coeff_yday2_hour_9=0,coeff_yday2_hour_10=0,coeff_yday2_hour_11=0,coeff_yday2_hour_12=0,coeff_yday2_hour_13=0,
                         coeff_yday2_hour_14=0,coeff_yday2_hour_15=0,coeff_yday2_hour_16=0,coeff_yday2_hour_17=0,coeff_yday2_hour_18=0,coeff_yday2_hour_19=0,coeff_yday2_hour_20=0,coeff_yday2_hour_21=0,coeff_yday2_hour_22=0,coeff_yday2_hour_23=0,
                         coeff_yday3_hour_1=0,coeff_yday3_hour_2=0,coeff_yday3_hour_3=0,coeff_yday3_hour_4=0,coeff_yday3_hour_5=0,coeff_yday3_hour_6=0,coeff_yday3_hour_7=0,coeff_yday3_hour_8=0,coeff_yday3_hour_9=0,coeff_yday3_hour_10=0,coeff_yday3_hour_11=0,coeff_yday3_hour_12=0,coeff_yday3_hour_13=0,
                         coeff_yday3_hour_14=0,coeff_yday3_hour_15=0,coeff_yday3_hour_16=0,coeff_yday3_hour_17=0,coeff_yday3_hour_18=0,coeff_yday3_hour_19=0,coeff_yday3_hour_20=0,coeff_yday3_hour_21=0,coeff_yday3_hour_22=0,coeff_yday3_hour_23=0,
                         coeff_value_gas_hour_1=0,coeff_value_gas_hour_2=0,coeff_value_gas_hour_3=0,coeff_value_gas_hour_4=0,coeff_value_gas_hour_5=0,coeff_value_gas_hour_6=0,coeff_value_gas_hour_7=0,coeff_value_gas_hour_8=0,coeff_value_gas_hour_9=0,coeff_value_gas_hour_10=0,coeff_value_gas_hour_11=0,coeff_value_gas_hour_12=0,coeff_value_gas_hour_13=0,
                         coeff_value_gas_hour_14=0,coeff_value_gas_hour_15=0,coeff_value_gas_hour_16=0,coeff_value_gas_hour_17=0,coeff_value_gas_hour_18=0,coeff_value_gas_hour_19=0,coeff_value_gas_hour_20=0,coeff_value_gas_hour_21=0,coeff_value_gas_hour_22=0,coeff_value_gas_hour_23=0,
                         coeff_value_gas2_hour_1=0,coeff_value_gas2_hour_2=0,coeff_value_gas2_hour_3=0,coeff_value_gas2_hour_4=0,coeff_value_gas2_hour_5=0,coeff_value_gas2_hour_6=0,coeff_value_gas2_hour_7=0,coeff_value_gas2_hour_8=0,coeff_value_gas2_hour_9=0,coeff_value_gas2_hour_10=0,coeff_value_gas2_hour_11=0,coeff_value_gas2_hour_12=0,coeff_value_gas2_hour_13=0,
                         coeff_value_gas2_hour_14=0,coeff_value_gas2_hour_15=0,coeff_value_gas2_hour_16=0,coeff_value_gas2_hour_17=0,coeff_value_gas2_hour_18=0,coeff_value_gas2_hour_19=0,coeff_value_gas2_hour_20=0,coeff_value_gas2_hour_21=0,coeff_value_gas2_hour_22=0,coeff_value_gas2_hour_23=0,
                         coeff_weekend_hour_1=0,coeff_weekend_hour_2=0,coeff_weekend_hour_3=0,coeff_weekend_hour_4=0,coeff_weekend_hour_5=0,coeff_weekend_hour_6=0,coeff_weekend_hour_7=0,coeff_weekend_hour_8=0,coeff_weekend_hour_9=0,coeff_weekend_hour_10=0,coeff_weekend_hour_11=0,coeff_weekend_hour_12=0,coeff_weekend_hour_13=0,
                         coeff_weekend_hour_14=0,coeff_weekend_hour_15=0,coeff_weekend_hour_16=0,coeff_weekend_hour_17=0,coeff_weekend_hour_18=0,coeff_weekend_hour_19=0,coeff_weekend_hour_20=0,coeff_weekend_hour_21=0,coeff_weekend_hour_22=0,coeff_weekend_hour_23=0,
                         coeff_break_h_hour_1=0,coeff_break_h_hour_2=0,coeff_break_h_hour_3=0,coeff_break_h_hour_4=0,coeff_break_h_hour_5=0,coeff_break_h_hour_6=0,coeff_break_h_hour_7=0,coeff_break_h_hour_8=0,coeff_break_h_hour_9=0,coeff_break_h_hour_10=0,coeff_break_h_hour_11=0,coeff_break_h_hour_12=0,coeff_break_h_hour_13=0,
                         coeff_break_h_hour_14=0,coeff_break_h_hour_15=0,coeff_break_h_hour_16=0,coeff_break_h_hour_17=0,coeff_break_h_hour_18=0,coeff_break_h_hour_19=0,coeff_break_h_hour_20=0,coeff_break_h_hour_21=0,coeff_break_h_hour_22=0,coeff_break_h_hour_23=0),
              data = DTW,
              control=  nls.control(maxiter = 50, tol = 1e-05, minFactor = 1/2100,
                                    printEval = FALSE, warnOnly = FALSE, scaleOffset = 0,
                                    nDcentral = FALSE))
  
  return(model_h)
  
}


