#' Filter Outliers in Time Series Data Using the Hamilton Filter
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
#' @import xts
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
