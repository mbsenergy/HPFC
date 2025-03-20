
#' Create a Calendar Data Table
#'
#' This function adds various time-based features to a given data.table, including seasonal indicators,
#' weekday dummies, and polynomial transformations of the day-of-year.
#'
#' @param dataframe A `data.table` containing at least:
#'   - `date` (Date): The date column.
#'   - `holiday` (numeric): Binary indicator for holidays.
#' @return A `data.table` with additional time-based features.
#' @import data.table
#' @importFrom data.table month year quarter yday wday fifelse fcase
#' @importFrom chron is.weekend
#' @export

create_calendar_dd = function(dataframe) {
  
  # Ensure input is a data.table
  if (!"data.table" %in% class(dataframe)) {
    stop("Error: Input `dataframe` must be a data.table.")
  }
  
  # Validate 'date' column
  if (!("date" %in% names(dataframe)) || !inherits(dataframe$date, "Date")) {
    stop("Error: Column `date` must be of class Date.")
  }
  
  # Validate 'holiday' column
  if (!("holiday" %in% names(dataframe)) || !is.numeric(dataframe$holiday)) {
    stop("Error: Column `holiday` must be numeric.")
  }
  
  # Copy dataset
  calendar = copy(dataframe)
  
  #### Generate time features
  calendar[, `:=`(
    month = month(date),
    year = year(date),
    quarter = quarter(date),
    yday = yday(date),
    weekend = as.numeric(chron::is.weekend(date)),
    wday = weekdays(date), # Weekday label (Mon, Tue, etc.)
    wday_num = wday(date) # Numeric weekday (1=Sunday, 2=Monday, ...)
  )]
  
  # Assign seasons
  calendar[, season := fcase(
    quarter == 1, "winter",
    quarter == 2, "spring",
    quarter == 3, "summer",
    quarter == 4, "fall"
  )]
  
  # Create seasonal dummy variables
  season_labels = c("winter", "spring", "summer", "fall")
  calendar[, (paste0("season_", season_labels)) :=
             lapply(season_labels, function(s) fifelse(season == s, 1, 0))]
  
  # Create weekday dummy variables
  calendar[, paste0("day_", 1:7) := lapply(1:7, function(i) fifelse(wday_num == i, 1, 0))]
  
  # Define summer period (Q2 & Q3)
  calendar[, summer := as.numeric(quarter %in% c(2, 3))]
  
  # Define seasonal grouping (winter spans two years)
  calendar[, season_group := fcase(
    summer == 1, paste0("summer-", year),
    quarter == 1, paste0("winter-", year - 1),
    quarter == 4, paste0("winter-", year)
  )]
  
  # Define season start dates
  calendar[, season_start := as.Date(fcase(
    summer == 1, paste0(year, "-04-01"),
    quarter == 1, paste0(year - 1, "-10-01"),
    quarter == 4, paste0(year, "-10-01")
  ))]
  
  # Days since season start
  calendar[, days_since_season_start := as.numeric(date - season_start)]
  calendar[, yday_season := days_since_season_start / max(days_since_season_start, na.rm = TRUE)]
  
  # Create polynomial terms for yday (1st to 10th power)
  calendar[, paste0("yday_", 1:10) := lapply(1:10, function(i) yday^i)]
  
  return(calendar)
}


#' Create Hourly Calendar Regressors
#'
#' This function expands a daily calendar into an hourly format, adding hourly time-based features.
#'
#' @param dataframe A daily `data.table` containing at least:
#'   - `date` (Date): The date column.
#'   - `holiday` (numeric): Binary indicator for holidays.
#'   - Other seasonal and categorical indicators.
#' @return A `data.table` with hourly time-based features.
#' @import data.table
#' @export

create_calendar_ddhh = function(dataframe) {
  
  # Ensure input is a data.table
  if (!"data.table" %in% class(dataframe)) {
    stop("Error: Input `dataframe` must be a data.table.")
  }
  
  # Validate 'date' column
  if (!("date" %in% names(dataframe)) || !inherits(dataframe$date, "Date")) {
    stop("Error: Column `date` must be of class Date.")
  }
  
  # Validate 'holiday' column
  if (!("holiday" %in% names(dataframe)) || !is.numeric(dataframe$holiday)) {
    stop("Error: Column `holiday` must be numeric.")
  }
  
  # Copy dataset
  calendar = copy(dataframe)
  
  ### Expand to hourly data
  calendar_hourly = calendar[rep(seq_len(.N), each = 24)]
  calendar_hourly[, hour := rep(1:24, times = nrow(calendar))]
  
  #### Create weekend/holiday feature
  calendar_hourly[, weekend_h := fifelse(holiday == 1, 1, weekend)]
  
  #### Convert quarter to string format
  calendar_hourly[, quarter := paste0('Q', quarter)]
  
  #### Create date-hour identifier
  calendar_hourly[, ddhh := paste(date, sprintf("%02dH", hour), sep = "_")]
  
  # Reorder by date and hour
  setorder(calendar_hourly, date, hour)
  
  # Keep only required columns
  cols_vec = c("date", "hour", "ddhh", "period", "value_gas",
               "weekend", "weekend_h", "quarter", "holiday", "month", "year", "yday",
               "season", "season_winter", "season_spring", "season_summer", "season_fall",
               "L_t", "spot_forward_month_BL", "spot_forward_month_PL",
               "value_day", "value_day2", "forward_month_BL", "forward_quarter_BL",
               "BL_prev_m", "history_forecast")
  
  set(calendar_hourly, j = setdiff(names(calendar_hourly), cols_vec), value = NULL)
  
  return(calendar_hourly)
}
