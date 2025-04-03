#' Filter Outliers in Daily Gas Data
#'
#' This function applies the Hamilton filter to a given time series data to remove cyclical components 
#' and detect outliers based on the deviation from the estimated trend. The function calculates 
#' the difference between the observed values and the trend, and flags values that deviate by more 
#' than three standard deviations as outliers. The flagged outliers are then removed from the returned data.
#'
#' @param DT A `data.table` containing at least three columns: 
#'   - `date`: Date of the observation.
#'   - `value`: The corresponding value for each date.
#'   - `break_group_p`: A grouping factor for breaks in the time series.
#'
#' @return A `data.table` with the following columns:
#'   - `date`: The original date.
#'   - `value`: The original value, with outliers removed.
#'   - `out_dummy`: A binary flag indicating whether the value was an outlier (1) or not (0).
#'   - `break_group_p`: The original break group.
#'
#' @details The function performs the following steps:
#'   - Applies the Hamilton filter to the time series to obtain the trend component.
#'   - Calculates the difference between the original values and the trend.
#'   - Flags values that deviate from the trend by more than three standard deviations as outliers.
#'   - Removes the outliers and returns the modified data.table.
#'
#' @import data.table
#' @import neverhpfilter
#' @importFrom xts as.xts
#' @importFrom crayon red yellow
#'
#' @examples
#' # Example usage with a sample data.table `DT`
#' DT <- data.table(date = seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 100),
#'                  value = rnorm(100), break_group_p = rep(1:5, each = 20))
#' result <- filter_outlier_dd(DT)
#' 
#' @export
filter_outlier_dd = function(DT) {
    
    # Check for errors in input data
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    
    if (!all(c("date", "value", "break_group_p") %in% names(DT))) {
        stop(red("Error: Data.table must contain 'date', 'value', and 'break_group_p' columns"))
    }
    
    if (any(is.na(DT$date))) {
        stop(yellow("Warning: NA values found in 'date' column"))
    }
    
    if (any(is.na(DT$value))) {
        stop(yellow("Warning: NA values found in 'value' column"))
    }
    
    if (any(is.na(DT$break_group_p))) {
        stop(yellow("Warning: NA values found in 'break_group_p' column"))
    }
    
    # Convert data.table to a time series-friendly format
    setDT(DT)
    
    # Rename columns for compatibility
    DT_ham = DT[, .(date, value)]
    
    # Apply Hamilton filter to extract trend and cycle components
    hamilton_filter = neverhpfilter::yth_filter(xts::as.xts(DT_ham), h = 24, p = 4, output = c("x", "trend", "cycle"))
    dt_hamilton_filter = as.data.table(hamilton_filter)
    
    # Rename columns after applying the Hamilton filter
    setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
    
    # Fill missing values in 'x', 'trend', and 'cycle' using the 'nocb' method
    dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
    dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
    dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]
    
    # Add the Hamilton trend to the original data.table
    DT[, hp_trend := dt_hamilton_filter$trend]
    
    # Calculate the difference between the observed values and the trend
    DT[, delta := value - (hp_trend)]
    DT[, sd_delta := sd(delta), by = break_group_p]
    
    # Identify outliers as those with a delta greater than 3 standard deviations
    DT[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
    DT[, out_values := fifelse(out_dummy == 1, value, NA_real_)]
    
    # Remove outliers and return the modified data.table
    DT = DT[out_dummy == 0, .(date, value, out_dummy, break_group_p)]
    
    return(DT)
    
}



#' Filter Outliers in Daily Power Data
#'
#' This function filters outliers in daily power data using the Hamilton filter. It computes 
#' daily averages, applies a trend filter, and removes values that deviate more than three 
#' standard deviations from the trend.
#'
#' @param DT A `data.table` containing at least two columns:
#'   - `date`: Date of the observation.
#'   - `value`: The power value associated with each date.
#'   - `break_group_p` (optional): A grouping variable for detected break periods.
#'
#' @return A `data.table` with the following columns:
#'   - `date`: The original date.
#'   - `value_day`: The daily mean value.
#'   - `out_dummy`: Binary indicator (1 if an outlier, 0 otherwise).
#'
#' @details The function performs the following steps:
#'   - Computes daily mean values.
#'   - Applies the `neverhpfilter::yth_filter` Hamilton filter to extract the trend.
#'   - Identifies outliers as values that deviate more than three standard deviations from the trend.
#'   - Filters out detected outliers from the dataset.
#'
#' @import data.table
#' @import neverhpfilter
#' @importFrom xts as.xts
#' @importFrom crayon red yellow
#'
#' @examples
#' # Example usage with a sample data.table `DT`
#' DT <- data.table(date = as.Date('2020-01-01') + 0:9, 
#'                  value = rnorm(10, 100, 10), 
#'                  break_group_p = sample(1:2, 10, replace = TRUE))
#' result <- filter_outlier_dd_pwr(DT, "value")
#' 
#' @export
filter_outlier_dd_pwr = function(DT) {
    
    # Error handling
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    
    if (!"date" %in% names(DT)) {
        stop(red("Error: 'date' column is missing in the input data.table"))
    }
    
    if (any(is.na(DT$date))) {
        warning(yellow("Warning: NA values found in 'date' column"))
    }
    
    if (any(is.na(DT$value))) {
        warning(yellow("Warning: NA values found in 'value' column"))
    }
    
    # Convert to data.table
    setDT(DT)
    
    DT_day = copy(DT)
    
    # Compute daily mean value
    DT_day = DT_day[, value_day := mean(value, na.rm = TRUE), by = date]
    DT_day = DT_day[, .(date, value_day)] |> unique()
    
    ## HAMILTON FILTER
    hamilton_filter = neverhpfilter::yth_filter(
        xts::as.xts(DT_day), h = 24, p = 4, output = c("x", "trend", "cycle")
    )
    
    dt_hamilton_filter = as.data.table(hamilton_filter)
    setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
    
    dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
    dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
    dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]
    
    # Merge with break group information (if available)
    if ("break_group_p" %in% names(DT)) {
        DT_day = unique(DT[, .(date, break_group_p)])[DT_day, on = 'date']
    } else {
        DT_day[, break_group_p := 1] # Default to one group if missing
    }
    
    ## Add Hamilton trend
    DT_day[, hp_trend := dt_hamilton_filter$trend]
    
    # Compute deviations from trend
    DT_day[, delta := value_day - hp_trend]
    DT_day[, sd_delta := sd(delta), by = break_group_p]
    
    # Identify outliers
    DT_day[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
    DT_day[, out_values := fifelse(out_dummy == 1, value_day, NA_real_)]
    
    # Remove outliers
    DT_day = DT_day[out_dummy == 0, .(date, value_day, out_dummy)]
    
    return(DT_day)
}



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

