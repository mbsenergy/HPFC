## Eikon Wrappers -------------------------------------------------------------------------------------

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
#' @importFrom eikonapir get_rics_d get_rics_h
#' @export
retrieve_spot = function(ric, from_date, to_date, type = 'PWR') {
    
    if(type == 'GAS') {
        
        rics_db = data.table::rbindlist(lapply(ric, eikonapir::get_rics_d, from_date = from_date, to_date = to_date))
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
        
        rics_db = data.table::rbindlist(lapply(ric, eikonapir::get_rics_h, from_date = from_date, to_date = to_date))
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
        history_pwr_all_s[, RIC := RIC]
        
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
#' @importFrom eikonapir get_rics_f
#' @export
retrieve_fwd = function(ric, from_date, to_date) {
    
    rics_db = data.table::rbindlist(lapply(ric, eikonapir::get_rics_f, from_date = from_date, to_date = to_date))
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