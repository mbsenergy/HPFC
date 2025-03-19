#' Filter Hourly Outliers in Power Data
#'
#' This function detects and filters outliers in hourly power data by computing 
#' deviations from the daily mean and checking against a threshold of three standard deviations.
#'
#' @param DT A `data.table` containing the following columns:
#'   - `date`: Date of the observation.
#'   - `hour`: Hour of the observation.
#'   - `value`: The power value.
#'   - `value_day`: The daily average value.
#'   - `break_group_p`: A grouping variable for detected break periods.
#'
#' @return A `data.table` with the following columns:
#'   - `date`: The original date.
#'   - `hour`: The original hour.
#'   - `ddhh`: A combined date-hour identifier.
#'   - `value`: The cleaned value.
#'   - `break_group_p`: The original break period.
#'
#' @details The function performs the following steps:
#'   - Computes deviations from the daily average.
#'   - Adjusts for weekend effects.
#'   - Identifies outliers based on three standard deviations from the mean deviation.
#'   - Removes outliers exceeding a daily threshold.
#'
#' @import data.table
#' @importFrom chron is.weekend
#' @importFrom crayon red yellow
#'
#' @examples
#' # Example usage with a sample data.table `DT`
#' DT <- data.table(date = as.Date('2020-01-01') + rep(0:1, each = 24),
#'                  hour = rep(0:23, 2),
#'                  value = rnorm(48, 100, 10),
#'                  value_day = rep(rnorm(2, 100, 5), each = 24),
#'                  break_group_p = sample(1:2, 48, replace = TRUE))
#' result <- filter_outlier_ddhh(DT)
#'
#' @export
filter_outlier_ddhh = function(DT) {
    
    # Error handling
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    
    required_cols = c("date", "hour", "value", "value_day", "break_group_p")
    missing_cols = setdiff(required_cols, names(DT))
    if (length(missing_cols) > 0) {
        stop(red(paste0("Error: Missing required columns: ", paste(missing_cols, collapse = ", "))))
    }
    
    if (any(is.na(DT$date))) {
        warning(yellow("Warning: NA values found in 'date' column"))
    }
    
    if (any(is.na(DT$hour))) {
        warning(yellow("Warning: NA values found in 'hour' column"))
    }
    
    if (any(is.na(DT$value))) {
        warning(yellow("Warning: NA values found in 'value' column"))
    }
    
    if (any(is.na(DT$value_day))) {
        warning(yellow("Warning: NA values found in 'value_day' column"))
    }
    
    # Convert to data.table
    setDT(DT)
    
    # Create unique date-hour identifier
    DT[, ddhh := paste(date, hour, sep = "H")]
    
    # Compute hourly deviation from daily mean
    DT[, value_h := value - value_day]
    
    # Identify weekends
    DT[, we := chron::is.weekend(date)]
    
    # Compute mean deviation per group
    DT[, mean_h := mean(value_h), by = c('break_group_p', 'hour', 'we')]
    
    # Compute standard deviation and deviation from mean
    DT[, delta_h := value_h - mean_h]
    DT[, sd_delta_h := sd(delta_h), by = c('break_group_p', 'hour', 'we')]
    
    # Identify outliers
    DT[, out_dummy := as.numeric(abs(delta_h) > 3 * sd_delta_h)]
    DT[, out_values := fifelse(out_dummy == 1, value_h, NA_real_)]
    
    # Outlier threshold per day
    DT[, out_perday := sum(out_dummy), by = date]
    DT[, out_dummy := fifelse(out_perday > 4, 1, out_dummy)]
    
    # Remove detected outliers
    DT = DT[out_dummy == 0, .(date, hour, ddhh, value, break_group_p)]
    
    return(DT)
}
