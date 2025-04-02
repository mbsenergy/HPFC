## Eikon Wrappers -------------------------------------------------------------------------------------

#' Retrieve Historical Market Data for a Given RIC
#'
#' @description This function fetches historical market data for a given RIC (identifier) over a specified date range. It uses `reikonapi::get_series()` to download the data and formats the results into a `data.table`.
#'
#' @param rics A string representing the RIC (identifier) whose historical data is to be retrieved.
#' @param from_date A Date object or character string representing the start date for data retrieval. Defaults to 10 years before the current date.
#' @param to_date A Date object or character string representing the end date for data retrieval. Defaults to the current date.
#' @param interval A string specifying the time interval for the data (e.g., "daily"). Defaults to "daily".
#' 
#' @return A `data.table` containing the historical data for the specified RIC. The columns included in the returned table are:
#' - `TIMESTAMP`: The date of the data point.
#' - `HIGH`: The highest price of the day.
#' - `LOW`: The lowest price of the day.
#' - `OPEN`: The opening price.
#' - `CLOSE`: The closing price.
#' - `VOLUME`: The trading volume.
#' - `ric_column`: The RIC identifier.
#'
#' If no data is available, the function returns a `data.table` with `NA` values.
#'
#' @details This function:
#' - Calls `reikonapi::get_series()` to fetch market data for the specified RIC.
#' - Checks if the returned data is valid; if not, it creates a placeholder `data.table` with `NA` values.
#' - Extracts and formats the necessary columns (`TIMESTAMP`, `HIGH`, `LOW`, `OPEN`, `CLOSE`, `VOLUME`, `ric_column`).
#' - Converts the `TIMESTAMP` column to a proper date format (`YYYY-MM-DD`).
#' - Prints the RIC being processed.
#'
#' @examples
#' # Example usage of get_rics function
#' data <- get_rics("AAPL.O", start_date = "2020-01-01", end_date = "2023-01-01")
#'
#' @import data.table
#' @importFrom reikonapi get_series
#' @export
get_rics_d = function(rics, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date(), interval = 'daily', sleep = 0) {
    
    ### Download Data
    db =
        reikonapi::get_series(
            rics =  rics,
            # fields = list("TIMESTAMP", "VOLUME", "CLOSE"),
            start_date = from_date,
            end_date  = to_date,
            interval = "daily")
    
    ### Adjust data.frame shape
    if (is.na(db[[1]][1])) {
        db = data.table::data.table(TIMESTAMP = as.Date(paste0(Sys.Date())),
                                    HIGH = NA,
                                    LOW = NA,
                                    OPEN = NA,
                                    CLOSE = NA,
                                    VOLUME = NA,
                                    ric_column = NA
        )
    } else {
        db = db[, .(TIMESTAMP = as.Date(substr(TIMESTAMP, 1, 10)), HIGH, LOW, OPEN, CLOSE, VOLUME, ric_column)]
    }
    
    Sys.sleep(sleep)
    print_retrieval_message(rics = rics, from_date = from_date, to_date = to_date)
    return(db)
}



#' Retrieve Hourly Data for a List of RICS
#'
#' @description This function retrieves hourly time-series data for a list of RICS from the Eikon API over a specified date range and interval. It aggregates data for 24 hours and returns it as a data table.
#'
#' @param rics A string or vector of strings representing the RICs (identifiers) for which to retrieve data.
#' @param from_date A Date object or character string representing the start date of the data range (default is 7 years before the current date).
#' @param to_date A Date object or character string representing the end date of the data range (default is the current date).
#' @param interval A string specifying the data interval (default is `'daily'`).
#'
#' @return A data.table containing time-series data for each RIC across the specified date range. The data includes `TIMESTAMP`, `CLOSE`, `ric_column`, and `hour`.
#'
#' @details This function:
#' - Loops through 24 hours to collect data for each RIC.
#' - Downloads the data using the `reikonapi::get_series()` function.
#' - Adjusts the data format and handles missing or empty data.
#' 
#' @examples
#' # Example usage of get_rics_h function
#' data <- get_rics_h(rics = "RIC_ABC", from_date = "2020-01-01", to_date = "2020-12-31")
#'
#' @import data.table
#' @importFrom reikonapi get_series
#' @export
get_rics_h = function(rics, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date(), interval = 'daily', sleep = 0) {
    
    rics_id_24 = c('01','02','03','04','05','06','07','08','09','10','11','12',
                   '13','14','15','16','17','18','19','20','21','22','23','24')
    
    db_24h = data.table::rbindlist(lapply(1:24, function(i) {
        rics_id_h = paste0(rics, rics_id_24[i])
        ### Download Data
        db =
            reikonapi::get_series( #eikonapir::get_timeseries
                rics =  rics_id_h,
                start_date = from_date,
                end_date  = to_date,
                interval = "daily")
        
        ### Adjust data.frame shape
        if (!is.null(db)) {
            if (is.na(db[[1]][[1]])) {
                
                db = data.table(TIMESTAMP = as.Date(paste0(Sys.Date())),
                                CLOSE = NA,
                                ric_column = NA
                                # RIC = rics,
                                # TRADED = 0
                )
                cat(paste(rics, 'founded, no data.'))
            } else {
                db = db[, .(TIMESTAMP = as.Date(substr(TIMESTAMP, 1, 10)), CLOSE, ric_column)]
            }
        } else {
            db = data.table::data.table(TIMESTAMP = as.Date(paste0(Sys.Date())),
                                        CLOSE = NA,
                                        ric_column = NA
                                        # RIC = rics,
                                        # TRADED = 0
            )
            cat(paste(rics, 'not founded.'))
        }
        
        db[, hour := i]
        print(i)
        Sys.sleep(sleep)
        return(db)
        
    }))
    
    data.table::setorderv(db_24h, cols = c('TIMESTAMP','hour'), order = 1L)
    print_retrieval_message(rics = rics, from_date = from_date, to_date = to_date)
    return(db_24h)
}

#' Retrieve Spot Data for a Given Date Range
#'
#' @description This function retrieves spot data for a given RIC (identifier) and date range. It merges the data, performs necessary data cleaning and transformation.
#'
#' @param ric A string representing the RIC (identifier) for the spot data.
#' @param from_date A Date object or character string representing the start date of the data range.
#' @param to_date A Date object or character string representing the end date of the data range.
#' @param type Either PWR or GAS
#' 
#' @return A data.table containing the spot price data (`date`, `smp`, and `RIC`) for the specified date range. The prices are cleaned by filling missing values.
#'
#' @examples
#' # Example usage of retrieve_gas function
#' data <- retrieve_gas("TTF", "2020-01-01", "2020-12-31")
#'
#' @import data.table
#' @export
retrieve_spot = function(ric, from_date, to_date, type = 'PWR', sleep = 0) {
    
    if(type == 'GAS') {
        
        rics_db = data.table::rbindlist(lapply(ric, get_rics_d, from_date = from_date, to_date = to_date, sleep = sleep))
        data.table::setDT(rics_db)
        rics_db = rics_db[, .(date = TIMESTAMP, value = CLOSE, RIC = ric_column)]
        rics_db[, date := as.Date(substr(date, 1, 10))]
        
        downloaded_spot = data.table::copy(rics_db)
        
        # Download and merge data for the given RIC
        # downloaded_spot = refenergy::merge_rics(ric)
        # downloaded_spot = downloaded_spot[, .(date = TIMESTAMP, trade_close = PRICE, RIC)]
        
        # Filter data from the 'from_date'
        history_ttf_all_s = downloaded_spot[date > from_date]
        data.table::setorderv(history_ttf_all_s, cols = 'date', order = -1L)
        
        # Filter data until the 'to_date'
        history_ttf_s = history_ttf_all_s[date <= to_date]
        
        # Clean data (convert 'trade_close' to numeric and fill missing values)
        history_ttf_s[, value := as.numeric(value)]
        history_ttf_s[, value := data.table::nafill(value, 'locf'), by = 'RIC']
        history_ttf_s[, value := data.table::nafill(value, 'nocb'), by = 'RIC']
        
        print_retrieval_done(message = 'Spot Gas retrieval finished.')
        
        return(history_ttf_s)
        
    } else if(type == 'PWR') {
        
        rics_db = data.table::rbindlist(lapply(ric, get_rics_h, from_date = from_date, to_date = to_date, sleep = sleep))
        data.table::setDT(rics_db)
        
        rics_db = rics_db[, .(TIMESTAMP, PRICE = CLOSE, RIC = ric_column, HOUR = hour)]
        rics_db[, TIMESTAMP := as.Date(substr(TIMESTAMP, 1, 10))]
        
        downloaded_spot = data.table::copy(rics_db)
        downloaded_spot = downloaded_spot[, .(date = TIMESTAMP, hour = HOUR, value = PRICE, RIC)]
        
        history_pwr_all_s = downloaded_spot[date > from_date]
        data.table::setorderv(history_pwr_all_s, cols =c('date','hour'), order = -1L)
        
        history_ttf_s = history_pwr_all_s[date <= to_date]
        
        history_pwr_all_s[, value := as.numeric(value)]
        history_pwr_all_s[, value := data.table::nafill(value, 'locf'), by = 'RIC']
        history_pwr_all_s[, value := data.table::nafill(value, 'nocb'), by = 'RIC']
        
        history_pwr_all_s[, hour := as.numeric(hour)]
        history_pwr_all_s[, RIC := substr(RIC, 1, nchar(RIC) - 2)]
        
        print_retrieval_done(message = 'Spot Power retrieval finished.')
        
        return(history_pwr_all_s)
        
    }
    
}


## Multiple Retrieve by Commodity Type -------------------------------------------------------------------------------------

#' Retrieve FWD Data for a Given Date Range
#'
#' @description This function retrieves gas spot data for a given RIC (identifier) and date range. It merges the data using `refenergy::merge_rics()` and performs necessary data cleaning and transformation.
#'
#' @param ric A string representing the RIC (identifier) for the spot data.
#' @param from_date A Date object or character string representing the start date of the data range.
#' @param to_date A Date object or character string representing the end date of the data range.
#' 
#' @return A data.table containing the spot price data (`date`, `smp`, and `RIC`) for the specified date range. The prices are cleaned by filling missing values.
#'
#' @examples
#' # Example usage of retrieve_gas function
#' data <- retrieve_gas("TTF", "2020-01-01", "2020-12-31")
#'
#' @import data.table
#' @export
retrieve_fwd = function(ric, from_date, to_date) {
    
    rics_db = data.table::rbindlist(lapply(ric, get_rics))
    data.table::setDT(rics_db)
    rics_db = rics_db[, .(date = TIMESTAMP, trade_close = CLOSE, RIC = ric_column)]
    rics_db[, date := as.Date(substr(date, 1, 10))]
    
    downloaded_fwd = data.table::copy(rics_db)
    
    # downloaded_RICS = refenergy::merge_rics(lst_rics)
    downloaded_fwd = downloaded_fwd[!(is.na(RIC)) & !(is.na(trade_close))]
    downloaded_fwd = downloaded_fwd[downloaded_fwd[, .I[date == max(date)], by = RIC]$V1]
    
    print_retrieval_done(message = 'FWD retrieval finished.')
    
    return(downloaded_fwd)
    
}



## Commenting -------------------------------------------------------------------------------------

#' Print Retrieval Message with Colors
#'
#' @description This function prints a retrieval message with colored output using the `crayon` and `glue` packages. It highlights key elements such as the RIC, start date, and end date.
#'
#' @param rics A character string representing the RIC (Reuters Instrument Code) or a vector of RICs.
#' @param from_date A character string or Date object representing the start date.
#' @param to_date A character string or Date object representing the end date.
#'
#' @return Prints a formatted message to the console but does not return a value.
#'
#' @examples
#' print_retrieval_message("AAPL.O", "2020-01-01", "2023-01-01")
#'
#' @import crayon
#' @importFrom glue glue
#' @export
print_retrieval_message = function(rics, from_date, to_date) {
    message = glue::glue("- {crayon::blue('Retrieving:')} {crayon::green(rics)}, {crayon::blue('from')} {crayon::yellow(from_date)}, {crayon::blue('to')} {crayon::yellow(to_date)}")
    cat(message, "\n")
}



#' Print Gas Retrieval Completion Message with Colors
#'
#' @description This function prints a message indicating that gas retrieval has finished. The message is formatted with colors using the `crayon` package.
#' @param message A character string representing the message to display after the ✔ symbol.
#' 
#' @return Prints a formatted message to the console but does not return a value.
#'
#' @examples
#' print_gas_retrieval_done()
#'
#' @import crayon
#' @importFrom glue glue
#' @export
print_retrieval_done = function(message) {
    formatted_message = glue::glue("{crayon::green('✔')} {crayon::green$bold(message)}")
    cat(formatted_message, "\n")
}