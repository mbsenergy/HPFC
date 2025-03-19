#' Detect Breakpoints in Time Series Data
#'
#' This function identifies potential breakpoints in a time series of daily price data. 
#' It performs a check for missing values, handles them by applying 'locf' and 'nocb' methods, 
#' and then applies the 'changepoint' package's binary segmentation method to detect breaks. 
#' The function uses a loop to test for a sufficient number of breakpoints and returns a 
#' modified data.table with a break group assigned to each row based on the detected breaks.
#'
#' @param DT A `data.table` containing at least two columns: 
#'   - `date`: Date of the observation.
#'   - `value`: The corresponding value for each date.
#'
#' @return A `data.table` with three columns:
#'   - `date`: The original date.
#'   - `value`: The original value, with missing values filled.
#'   - `break_group_p`: The period group assigned to each observation based on detected breaks.
#'
#' @details The function performs the following steps:
#'   - Checks for missing values in the `date` and `value` columns.
#'   - Fills missing `value` entries using last observation carried forward ('locf') and 
#'     next observation carried backward ('nocb').
#'   - Converts the `value` column into a time series object and applies a changepoint detection 
#'     method to find breaks in the series.
#'   - Iteratively reduces the number of breaks until a break is detected with a period of 
#'     at least 3 months within the last 5 years.
#'   - Assigns a break group to each observation based on the detected breaks.
#'
#' @import data.table
#' @importFrom changepoint cpt.var cpts
#' @importFrom crayon red yellow
#' 
#' @examples
#' # Example usage with a sample data.table `DT`
#' DT <- data.table(date = seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 100),
#'                  value = rnorm(100))
#' result <- break_detection_dd(DT)
#' 
#' @export
#' 
break_detection_dd = function(DT) {
    
    # Check for errors in input data
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    
    if (!all(c("date", "value") %in% names(DT))) {
        stop(red("Error: Data.table must contain 'date' and 'value' columns"))
    }
    
    if (any(is.na(DT$date))) {
        stop(yellow("Warning: NA values found in 'date' column"))
    }
    
    if (any(is.na(DT$value))) {
        stop(yellow("Warning: NA values found in 'value' column"))
    }
    
    # Process data table
    setDT(DT)
    
    # Remove any non-'date' or 'value' columns
    set(DT, , names(DT)[!names(DT) %in% c('date', 'value')], NULL)
    
    # Find minimum date
    min_date = as.Date(min(DT$date))
    
    # Sort the data by 'date'
    setorderv(DT, cols = c('date'), order = 1L)
    
    # Fill missing 'value' using last observation carried forward and next observation carried backward
    DT[, value := nafill(value, 'locf')]
    DT[, value := nafill(value, 'nocb')]
    
    # Create time series of daily prices
    DT_ts_detr = ts(DT$value,
                    start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
                    frequency = 365)
    
    # Start from n breaks equal to number of years, check for a period with less than 3 months in the last 5 years
    for (j in 1:round(length(DT_ts_detr) / (365))){
        
        n_breaks = 1 + round(length(DT_ts_detr) / (365)) - j
        
        # Suppress warnings from changepoint::cpt.var
        value_h = suppressWarnings(changepoint::cpt.var(DT_ts_detr, Q = n_breaks, method = 'BinSeg'))
        
        cutoff = length(DT_ts_detr) - ((365) * 5)
        
        # Calculate change points
        vec = c(0, changepoint::cpts(value_h)[changepoint::cpts(value_h) > cutoff], length(DT_ts_detr))
        
        difs = diff(vec)
        
        # If no period with less than 3 months, break the loop
        if (!any(24 * 90 > difs)) { break } 
        
    }
    
    # Create variable with period group
    discv_date_h = DT$date[changepoint::cpts(value_h)]
    
    DT[, break_group_p := findInterval(date, discv_date_h)]
    
    # Return the updated data table with the 'break_group_p' variable
    DT = DT[, .(date, value, break_group_p)]
    
    return(DT)
    
}
