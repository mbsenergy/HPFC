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
