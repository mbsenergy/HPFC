#' Detrend Daily Data Using HP Filter
#'
#' This function applies the Hodrick-Prescott (HP) filter to detrend daily data.
#'
#' @param DT A `data.table` containing:
#'   - `date`: Date of the observation.
#'   - `value_name`: The column name representing the value to be detrended (either `value_day` or `value`).
#'   - `break_group_p`: A grouping variable for detected break periods.
#'
#' @return A `data.table` with:
#'   - `date`: The original date.
#'   - `value_name`: The original value (either `value_day` or `value`).
#'   - `hp_trend`: The estimated trend component from the HP filter.
#'   - `detr_value`: The detrended value.
#'
#' @details
#' The Hodrick-Prescott filter is used to remove cyclical fluctuations and extract the long-term trend.
#' The smoothing parameter (`lambda = 8.322 * 10^7`) follows Caldana et al. (2017).
#'
#' @import data.table
#' @importFrom mFilter hpfilter
#' @importFrom crayon red yellow
#'
#' @examples
#' DT <- data.table(date = as.Date('2020-01-01') + 0:100,
#'                  value_day = rnorm(101, 100, 10),
#'                  break_group_p = sample(1:3, 101, replace = TRUE))
#' result <- detrend_dd(DT, value_name = "value_day")
#'
#' @export
detrend_dd = function(DT, value_name) {
    
    # Error handling
    if (!"data.table" %in% class(DT)) {
        stop(red("Error: Input is not a data.table"))
    }
    
    required_cols = c("date", value_name)
    missing_cols = setdiff(required_cols, names(DT))
    if (length(missing_cols) > 0) {
        stop(red(paste0("Error: Missing required columns: ", paste(missing_cols, collapse = ", "))))
    }
    
    if (any(is.na(DT$date))) {
        warning(yellow("Warning: NA values found in 'date' column"))
    }
    
    if (any(is.na(DT[[value_name]]))) {
        warning(yellow(paste0("Warning: NA values found in '", value_name, "' column")))
    }
    
    # Ensure DT is a data.table
    setDT(DT)
    
    min_date = as.Date(min(DT$date))
    
    # Convert data to time series format
    df_dam_dd_ts = ts(DT[[value_name]],
                      start = c(as.numeric(format(min_date, '%Y')), 
                                as.numeric(format(min_date, '%m')), 
                                as.numeric(format(min_date, '%d'))),
                      frequency = 365)
    
    # Apply HP filter
    hp_filter2 = mFilter::hpfilter(df_dam_dd_ts,
                                   freq = 8.322 * 10^7,  ### 8.322 * 10 ^ 7
                                   type = "lambda",
                                   drift = FALSE)
    
    # Store trend component
    DT[, hp_trend := hp_filter2$trend]
    
    # Compute detrended values
    DT[, detr_value := DT[[value_name]] - hp_trend]
    
    return(DT)
}
