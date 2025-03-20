#' Create regressors in the daily DT for gas model estimation
#'
#' This function generates regressors for time series modeling, incorporating seasonal,
#' long-term trends, and break group interactions.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `date` (Date): The date of each observation.
#'   - `break_group_p` (integer): Grouping variable for breakpoints.
#'   - `holiday` (numeric): Binary indicator for holidays.
#'   - `hp_trend` (numeric): HP filter trend component.
#' @param alpha A numeric weight parameter for exponential smoothing (must be between 0 and 1).
#' @return A `data.table` with additional regressors for modeling.
#' @import data.table
#' @importFrom chron is.weekend
#' @export

regressors_lt_model_gas = function(DT, alpha) {
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Check if required columns exist
  required_cols = c("date", "break_group_p", "holiday", "hp_trend")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha > 1) {
    stop("Error: `alpha` must be a numeric value between 0 and 1.")
  }
  
  # Copy DT to avoid modifying the original
  DTW = copy(DT)
  
  DTW[, `:=`(
    yday = data.table::yday(date),
    wday = data.table::wday(date),
    quarter = data.table::quarter(date),
    month = data.table::month(date),
    weekend = as.numeric(chron::is.weekend(date)),
    obs = .I
  )]
  
  # Create weight with smoothing
  DTW[, time_distance := (max(obs) + 1 - obs) / max(obs)]
  DTW[, weight := exp(-alpha * time_distance)]
  
  # Create day dummies (intra-week seasonality)
  DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) fifelse(wday == i, 1, 0))]
  
  # Create summer dummy
  DTW[, summer := as.numeric(quarter %in% c(2, 3))]
  
  # Define season start date and compute distance to season start
  DTW[, begining_season := as.Date(
    fifelse(quarter == 4, paste0(year(date), "-10-01"),
            fifelse(quarter == 1, paste0(year(date) - 1, "-10-01"),
                    paste0(year(date), "-04-01")))
  )]
  
  DTW[, dist := as.numeric(date - begining_season)]
  DTW[, yday_season := dist / max(dist)]
  
  # Create season-year label
  DTW[, season := fifelse(summer == 1, paste0("summer-", year(date)),
                          fifelse(quarter == 4, paste0("winter-", year(date)),
                                  paste0("winter-", year(date) - 1)))]
  
  # Generate long-term seasonal components
  DTW[, `:=`(
    cos_long_term = cos((2 * pi) * yday / 365),
    cos_season = cos((2 * pi) * yday_season),
    cos_season_summer = cos((2 * pi) * yday_season) * summer,
    sin_long_term = sin((2 * pi) * yday / 365),
    sin_season = sin((2 * pi) * yday_season),
    sin_season_summer = sin((2 * pi) * yday_season) * summer
  )]
  
  # Generate polynomial features for yday
  DTW[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) yday^i)]
  
  # Number of break groups
  n_groups = max(DTW$break_group_p) + 1
  
  # Create break group dummies
  DTW[, (paste("break_group", 1:n_groups, sep = "_")) :=
        lapply(1:n_groups, function(i) as.numeric(break_group_p == (i - 1)))]
  
  # Generate interaction terms for each break group
  for (x in 1:n_groups) {
    group_col = paste("break_group", x, sep = "_")
    
    DTW[, paste('cos_long_term', x, sep = '_') := get(group_col) * cos_long_term]
    DTW[, paste('cos_season', x, sep = '_') := get(group_col) * cos_season]
    DTW[, paste('cos_season_summer', x, sep = '_') := get(group_col) * cos_season_summer]
    DTW[, paste('sin_long_term', x, sep = '_') := get(group_col) * sin_long_term]
    DTW[, paste('sin_season', x, sep = '_') := get(group_col) * sin_season]
    DTW[, paste('sin_season_summer', x, sep = '_') := get(group_col) * sin_season_summer]
    DTW[, paste('holiday', x, sep = '_') := get(group_col) * holiday]
    DTW[, paste('summer', x, sep = '_') := get(group_col) * summer]
    
    for (d in 2:7) {
      DTW[, paste('day', d, x, sep = '_') := get(group_col) * get(paste("day", d, sep = "_"))]
    }
    
    DTW[, (paste("yday", 1:10, x, sep = "_")) :=
          lapply(1:10, function(i) get(group_col) * get(paste("yday", i, sep = "_")))]
  }
  
  return(DTW)
}


#' Long Term model - PWR
#'
#' This function generates regressors for time series modeling, incorporating seasonal and long-term trends.
#' The macro trend is derived using the HP filter.
#'
#' @param DT A `data.table` with at least the following columns:
#'   - `date` (Date): The date of each observation.
#'   - `hp_trend` (numeric): The HP trend component.
#' @param alpha A numeric weight parameter for exponential smoothing (must be between 0 and 1).
#' @return A `data.table` with 41 columns containing original data and additional regressors.
#' @import data.table
#' @importFrom chron is.weekend
#' @export

regressors_lt_model_pwr = function(DT, alpha) {
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Check if required columns exist
  required_cols = c("date", "hp_trend")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha > 1) {
    stop("Error: `alpha` must be a numeric value between 0 and 1.")
  }
  
  DTW = copy(DT)
  
  DTW[, `:=`(
    yday = data.table::yday(date),
    wday = data.table::wday(date),
    quarter = data.table::quarter(date),
    month = data.table::month(date),
    weekend = as.numeric(chron::is.weekend(date)),
    obs = .I
  )]
  
  # Create weight with smoothing
  DTW[, time_distance := (max(obs) + 1 - obs) / max(obs)]
  DTW[, weight := exp(-alpha * time_distance)]
  
  # Create day dummies (intra-week seasonality)
  DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) fifelse(wday == i, 1, 0))]
  
  # Create summer dummy
  DTW[, summer := as.numeric(quarter %in% c(2, 3))]
  
  # Define season start date and compute distance to season start
  DTW[, begining_season := as.Date(
    fifelse(quarter == 4, paste0(year(date), "-10-01"),
            fifelse(quarter == 1, paste0(year(date) - 1, "-10-01"),
                    paste0(year(date), "-04-01"))))
  ]
  
  DTW[, dist := as.numeric(date - begining_season)]
  DTW[, yday_season := dist / max(dist)]
  
  # Create season-year label
  DTW[, season := fifelse(summer == 1, paste0("summer-", year(date)),
                          fifelse(quarter == 4, paste0("winter-", year(date)),
                                  paste0("winter-", year(date) - 1)))]
  
  # Generate long-term seasonal components
  DTW[, `:=`(
    cos_long_term = cos((2 * pi) * yday / 365),
    cos_season = cos((2 * pi) * yday_season),
    cos_season_summer = cos((2 * pi) * yday_season) * summer,
    sin_long_term = sin((2 * pi) * yday / 365),
    sin_season = sin((2 * pi) * yday_season),
    sin_season_summer = sin((2 * pi) * yday_season) * summer
  )]
  
  # Generate polynomial features for yday
  DTW[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) yday^i)]
  
  return(DTW)
}


#' SHort Term model - PWR
#'
#' This function creates regressors for hourly time series analysis, incorporating seasonality, 
#' dummies for hours, weekdays, and additional transformations.
#'
#' @param DT A `data.table` containing at least the following columns:
#'   - `date` (Date): The date of each observation.
#'   - `hour` (integer): The hour of the observation (1-24).
#'   - `value` (numeric): Observed values for baseline calculations.
#'   - `break_group_p` (integer): Grouping variable for breakpoints.
#' @return A `data.table` with additional regressors for modeling.
#' @import data.table
#' @importFrom chron is.weekend
#' @export

regressors_st_model_pwr = function(DT) {
  
  # Check if DT is a data.table
  if (!"data.table" %in% class(DT)) {
    stop("Error: Input `DT` must be a data.table.")
  }
  
  # Check if required columns exist
  required_cols = c("date", "hour", "value", "break_group_p")
  missing_cols = setdiff(required_cols, names(DT))
  if (length(missing_cols) > 0) {
    stop(paste("Error: Missing required columns in `DT`:", paste(missing_cols, collapse = ", ")))
  }
  
  # Copy DT to avoid modifying the original
  DTW = copy(DT)
  
  # Create seasonal, day, and time variables
  DTW[, `:=` (
    yday = data.table::yday(date),
    wday = data.table::wday(date),
    quarter = data.table::quarter(date),
    month = data.table::month(date),
    weekend = as.numeric(chron::is.weekend(date)),
    obs = .I
  )]
  
  # Assign seasons based on quarter
  DTW[, season := fcase(
    quarter == 1, "winter",
    quarter == 2, "spring",
    quarter == 3, "summer",
    quarter == 4, "fall"
  )]
  
  # Create season dummies
  DTW[, (paste0("season_", c("winter", "spring", "summer", "fall"))) := 
        lapply(c("winter", "spring", "summer", "fall"), function(s) fifelse(season == s, 1, 0))]
  
  ## Hourly Dummies ---------------------------------------------------------------------------------------------
  
  # Create weekday dummies
  DTW[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) fifelse(wday == i, 1, 0))]
  
  # Create hourly dummies
  DTW[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) fifelse(hour == i, 1, 0))]
  
  # Create baseload (daily mean value)
  DTW[, bl := mean(value), by = date]
  
  # Identify last market regime period
  DTW[, break_h := fifelse(break_group_p == max(break_group_p), 1, 0)]
  
  # Compute hourly deviation from baseload
  DTW[, value_h := value - bl]
  
  # Generate polynomial transformations
  DTW[, `:=` (
    yday2 = yday^2,
    yday3 = yday^3,
    bl2 = bl^2,
    bl3 = bl^3
  )]
  
  return(DTW)
}
