## Eikon Wrappers -------------------------------------------------------------------------------------

#' Retrieve Daily Historical Market Data for a Given RIC
#'
#' @description
#' Fetches historical daily market data for a given Reuters Instrument Code (RIC) over a specified date range.
#' It uses `eikonapir::get_timeseries()` to retrieve the data and formats it into a `data.table`.
#'
#' @param rics A character string representing the RIC (identifier) for which historical data is to be retrieved.
#' @param from_date A Date object or character string specifying the start date for data retrieval. Defaults to 10 years before the current date.
#' @param to_date A Date object or character string specifying the end date for data retrieval. Defaults to the current date.
#'
#' @return A `data.table` containing the historical data for the specified RIC with the following columns:
#' \itemize{
#'   \item \code{date} - The date of the observation (YYYY-MM-DD).
#'   \item \code{ric} - The RIC identifier.
#'   \item \code{value} - The closing price of the RIC.
#'   \item \code{volume} - The trading volume.
#' }
#'
#' If no data is available, the function returns an empty `data.table`.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Calls `eikonapir::get_timeseries()` to fetch market data for the specified RIC.
#'   \item Removes unnecessary columns (`HIGH`, `LOW`, `OPEN`, `COUNT`).
#'   \item Converts the `DATE` column to the `YYYY-MM-DD` format.
#'   \item Orders the dataset by `DATE`.
#'   \item Prints a message summarizing the data retrieval.
#' }
#'
#' @examples
#' \dontrun{
#' data <- get_rics_d("AAPL.O", from_date = "2020-01-01", to_date = "2023-01-01")
#' }
#'
#' @import data.table
#' @importFrom eikonapir get_timeseries
#' @export
get_rics_d = function(rics, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date()) {
    
    start_date = paste0(from_date, 'T00:00:00')
    end_date = paste0(to_date, 'T00:00:00')
    
    # Download Data
    db = eikonapir::get_timeseries(
        rics = list(rics),
        fields = list('TIMESTAMP', 'CLOSE', 'VOLUME'),
        start_date = start_date,
        end_date = end_date,
        interval = "daily"
    )
    
    # Rename and process columns
    colnames(db) = c('DATE', 'CLOSE','VOLUME', 'RIC')
    setDT(db)
    
    db[, DATE := substr(DATE, 1, 10)]
    
    colnames(db) = c('DATE', 'VALUE', 'VOLUME', 'RIC')
    setcolorder(db, c('DATE', 'RIC', 'VALUE', 'VOLUME'))
    
    # Order data
    data.table::setorderv(db, cols = c('DATE'), order = 1L)
    colnames(db) = tolower(names(db))
    
    # Print retrieval message
    rows_count = nrow(db)
    print_retrieval_message(rics = rics, from_date = from_date, to_date = to_date, nrows = rows_count)
    
    return(db)
}



#' Retrieve Hourly Time Series Data for a Given RIC
#'
#' This function retrieves 24-hour time series data for a given Reuters Instrument Code (RIC).
#' It fetches data for each hour individually, processes it, and returns a structured data.table.
#'
#' @param rics Character string representing the base RIC (without the hour suffix).
#' @param from_date Character or Date, the start date of the time series (default: 10 years ago).
#' @param to_date Character or Date, the end date of the time series (default: today).
#' @param interval Character, the data frequency (default: "daily").
#' @param sleep Numeric, time (in seconds) to wait between API calls (default: 0).
#'
#' @return A `data.table` with columns:
#' \itemize{
#'   \item \code{date} - Date of the observation (YYYY-MM-DD).
#'   \item \code{hour} - Hour of the day (integer from 1 to 24).
#'   \item \code{ric} - The base RIC.
#'   \item \code{value} - The retrieved value for the RIC.
#'   \item \code{volume} - Trading volume or relevant metric.
#' }
#'
#' @details
#' The function constructs RICs for each of the 24 hours by appending `01` to `24` to the base RIC.
#' It then retrieves time series data using `eikonapir::get_timeseries()` and processes it into a unified table.
#'
#' @note If not all 24 hours are present in the dataset, a warning is issued.
#'
#' @examples
#' \dontrun{
#' get_rics_h("HEEGRAUCH", from_date = "2020-01-01", to_date = "2023-01-01")
#' }
#'
#' @import data.table
#' @importFrom eikonapir get_timeseries
#' @export
get_rics_h = function(rics, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date()) {
    
    rics_id_24 = c('01','02','03','04','05','06','07','08','09','10','11','12',
                   '13','14','15','16','17','18','19','20','21','22','23','24')
    rics_id_h = paste0(rics, rics_id_24)
    
    # Format dates
    start_date = paste0(from_date, 'T00:00:00')
    end_date = paste0(to_date, 'T00:00:00')
    
    # Fetch data
    db = lapply(rics_id_h, eikonapir::get_timeseries, start_date = start_date, end_date = end_date)
    
    # Combine results
    db_24h = rbindlist(db, use.names = TRUE, fill = TRUE)
    colnames(db_24h) = c('DATE', 'VALUE', 'VOLUME', 'RIC_H')
    
    db_24h[, DATE := substr(DATE, 1, 10)]
    db_24h[, HOUR := as.integer(substr(RIC_H, nchar(RIC_H) - 1, nchar(RIC_H)))]
    db_24h[, RIC := rics]
    db_24h[, RIC_H := NULL]
    
    setcolorder(db_24h, c('DATE', 'HOUR', 'RIC', 'VALUE', 'VOLUME'))
    
    # Order data
    data.table::setorderv(db_24h, cols = c('DATE','HOUR'), order = 1L)
    colnames(db_24h) = tolower(names(db_24h))
    
    # Check if all 24 hours are present
    if (uniqueN(db_24h$hour) < 24) {
        warning("Not all 24 hours are present in the data.")
    }
    
    # Print retrieval message
    rows_count = nrow(db_24h)
    print_retrieval_message(rics = rics, from_date = from_date, to_date = to_date, nrows = rows_count)
    
    return(db_24h)
}


#' Retrieve Forward Market Data for a Given RIC
#'
#' @description This function fetches forward market data for a given RIC (identifier) over a specified date range. 
#' It uses `eikonapir::get_timeseries()` to download the data and formats the results into a `data.table`.
#'
#' @param rics A string representing the RIC (identifier) whose forward market data is to be retrieved.
#' @param from_date A Date object or character string representing the start date for data retrieval. Defaults to 10 years before the current date.
#' @param to_date A Date object or character string representing the end date for data retrieval. Defaults to the current date.
#'
#' @return A `data.table` containing the forward market data for the specified RIC. The table includes the following columns:
#' - `date`: The date of the forward contract.
#' - `ric`: The RIC identifier.
#' - `value`: The forward contract price.
#' - `volume`: The trading volume.
#'
#' If no data is available, the function returns a `data.table` with `NA` values.
#'
#' @details This function:
#' - Calls `eikonapir::get_timeseries()` to fetch forward market data.
#' - Handles cases where no valid data is found, returning a placeholder table with `NA` values.
#' - Renames and formats the columns (`date`, `value`, `volume`, `ric`).
#' - Converts the `date` column to `YYYY-MM-DD` format.
#' - Orders the data chronologically by `date`.
#' - Prints a retrieval summary message.
#'
#' @examples
#' # Example usage:
#' data <- get_rics_f("TTF=", from_date = "2023-01-01", to_date = "2023-12-31")
#'
#' @import data.table
#' @importFrom eikonapir get_timeseries
#' @export
get_rics_f = function(rics, from_date = Sys.Date() - (365 * 10), to_date = Sys.Date()) {
    
    start_date = paste0(from_date, 'T00:00:00')
    end_date = paste0(to_date, 'T00:00:00')
    
    # Download Data
    db = eikonapir::get_timeseries(
        rics = list(rics),
        fields = list('TIMESTAMP', 'CLOSE', 'VOLUME'),
        start_date = start_date,
        end_date = end_date,
        interval = "daily"
    )
    
    if (is.null(db) || nrow(db) == 0 || all(is.na(db[[1]]))) {
        warning(sprintf("No valid data found for RIC: %s", rics))
        db = data.table(date = NA_character_, ric = rics, value = NA_real_, volume = NA_real_)
        print_retrieval_message(rics = rics, from_date = 'NO DATA', to_date = 'NO DATA', nrows = 'NO DATA')
    } else {
        
        # Rename and process columns
        colnames(db) = c('DATE', 'CLOSE', 'VOLUME', 'RIC')
        setDT(db)
        
        db[, DATE := substr(DATE, 1, 10)]
        
        colnames(db) = c('DATE', 'VALUE', 'VOLUME', 'RIC')
        setcolorder(db, c('DATE', 'RIC', 'VALUE', 'VOLUME'))
        
        # Order data
        data.table::setorderv(db, cols = c('DATE'), order = 1L)
        colnames(db) = tolower(names(db))
        
        # Print retrieval message
        rows_count = nrow(db)
        print_retrieval_message(rics = rics, from_date = from_date, to_date = to_date, nrows = rows_count)
        
    }
    
    return(db)
    
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
retrieve_spot = function(ric, from_date, to_date, type = 'PWR') {
    
    if(type == 'GAS') {
        
        rics_db = data.table::rbindlist(lapply(ric, get_rics_d, from_date = from_date, to_date = to_date))
        data.table::setDT(rics_db)
        rics_db = rics_db[, .(date = as.Date(date), value = value, RIC = ric)]
        
        downloaded_spot = data.table::copy(rics_db)
        
        # Download and merge data for the given RIC
        # downloaded_spot = refenergy::merge_rics(ric)
        # downloaded_spot = downloaded_spot[, .(date = TIMESTAMP, trade_close = PRICE, RIC)]
        
        # Filter data from the 'from_date'
        history_ttf_all_s = downloaded_spot[date >= from_date]
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
        
        rics_db = data.table::rbindlist(lapply(ric, get_rics_h, from_date = from_date, to_date = to_date))
        data.table::setDT(rics_db)
        
        rics_db = rics_db[, .(date = as.Date(date), hour = hour, value = value, RIC = ric)]
        
        downloaded_spot = data.table::copy(rics_db)
        
        history_pwr_all_s = downloaded_spot[date >= from_date]
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
    
    rics_db = data.table::rbindlist(lapply(ric, get_rics_f, from_date = from_date, to_date = to_date))
    data.table::setDT(rics_db)
    rics_db = rics_db[, .(date = as.Date(date), value, RIC = ric)]
    
    downloaded_fwd = data.table::copy(rics_db)
    
    # downloaded_RICS = refenergy::merge_rics(lst_rics)
    downloaded_fwd = downloaded_fwd[!(is.na(RIC)) & !(is.na(value))]
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
print_retrieval_message = function(rics, from_date, to_date, nrows = NULL) {
    message = glue::glue("- {crayon::blue('Retrieving:')} {crayon::green(rics)}, {crayon::blue('from')} {crayon::yellow(from_date)}, {crayon::blue('to')} {crayon::yellow(to_date)}, {crayon::blue('with nrows>')} {crayon::yellow(nrows)}")
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