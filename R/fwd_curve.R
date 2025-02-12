#' Prepares Forward Curve
#'
#' This function extracts and processes forward quotes from a given data table.
#' It filters quotes based on selected RIC codes, processes them into different time 
#' intervals (month, quarter, calendar year), and merges them with a predefined 
#' calendar to produce structured forward curves.
#'
#' @param DT A `data.table` containing forward quotes.
#' @param list_rics A character vector of RIC codes to filter forward quotes.
#' @param type A character string indicating the type of forward quotes. Defaults to `'GAS'`.
#' @param start_date The start date for the forecast period.
#' @param end_date The end date for the forecast period.
#'
#' @import data.table
#' @return A `data.table` containing structured forward quotes for gas.
#' @export
prep_fwd_curve = function(DT, list_rics, type = 'PWR', start_date = Sys.Date() - 365, end_date = Sys.Date(), calendar_sim = ENV_FWD$calendar) {
    
    calendar_sim = ENV_FWD$calendar
    
    if(type == 'GAS') {
        
        ## GAS 
        
        gas_forward_quotes_BL = copy(DT)
        gas_forward_quotes_BL = gas_forward_quotes_BL[quote %like% paste(list_rics, collapse = '|')]
        
        # Create key to merge on Reuter
        gas_forward_quotes_BL[, `:=` (
            year = paste0(
                20, fcase(
                    substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                    paste0(substr(quote, nchar(quote), nchar(quote)), substr(quote, nchar(quote) - 2, nchar(quote) - 2)),
                    substr(quote, nchar(quote), nchar(quote)) >= substr(year(Sys.Date()), 4, 4),
                    paste0(as.integer(substr(year(Sys.Date()), 3, 3)), substr(quote, nchar(quote), nchar(quote))),
                    default = paste0(as.integer(substr(year(Sys.Date()), 3, 3)) + 1, substr(quote, nchar(quote), nchar(quote)))
                )
            ),
            code = fcase(
                substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                substr(quote, nchar(quote) - 3, nchar(quote) - 3),
                default = substr(quote, nchar(quote) - 1, nchar(quote) - 1)
            )
        )]
        
        # Define matching strings
        string_gas_month = paste(paste0(list_rics, 'M'), collapse = '|')
        string_gas_quarter = paste(paste0(list_rics, 'Q'), collapse = '|')
        string_gas_cal = paste(paste0(list_rics, 'Y'), collapse = '|')
        
        # Merge with HPFC tables
        gas_forward_quotes_month_BL = HPFC::reuters_months[gas_forward_quotes_BL[quote %like% string_gas_month], on = 'code']
        gas_forward_quotes_quarter_BL = HPFC::reuters_quarters_GAS[gas_forward_quotes_BL[quote %like% string_gas_quarter], on = 'code']
        gas_forward_quotes_cal_BL = HPFC::reuters_quarters_GAS[gas_forward_quotes_BL[quote %like% string_gas_cal], on = 'code']
        
        # Clean up
        setnames(gas_forward_quotes_month_BL, 'value', 'forward_month_BL_gas')
        setnames(gas_forward_quotes_quarter_BL, 'value', 'forward_quarter_BL_gas')
        setnames(gas_forward_quotes_cal_BL, 'value', 'forward_cal_BL_gas')
        
        forward_quotes_gas = calendar_sim[date >= start_date & date <= end_date, .(year, month, quarter)] |> unique()
        
        # Process each RIC
        lst_forward_quotes_gas = lapply(list_rics, function(i) {
            gas_month = gas_forward_quotes_month_BL[quote %like% as.character(i)][, .(month, year, forward_month_BL_gas)]
            gas_quarter = gas_forward_quotes_quarter_BL[quote %like% as.character(i)][, .(quarter, year, forward_quarter_BL_gas)]
            gas_cal = gas_forward_quotes_cal_BL[quote %like% as.character(i)][, .(year, forward_cal_BL_gas)]
            
            forward_quotes_gas = gas_month[forward_quotes_gas, on = c('month', 'year')]
            forward_quotes_gas = gas_quarter[forward_quotes_gas, on = c('quarter', 'year')]
            forward_quotes_gas = gas_cal[forward_quotes_gas, on = c('year')]
            
            forward_quotes_gas[, forward_cal_BL_gas := ifelse(
                is.na(forward_cal_BL_gas) & is.na(forward_quarter_BL_gas) & is.na(forward_month_BL_gas), 
                max(
                    tail(forward_quotes_gas$forward_quarter_BL_gas[!is.na(forward_quotes_gas$forward_quarter_BL_gas)], 1),
                    tail(forward_quotes_gas$forward_month_BL_gas[!is.na(forward_quotes_gas$forward_month_BL_gas)], 1),
                    tail(forward_quotes_gas$forward_cal_BL_gas[!is.na(forward_quotes_gas$forward_cal_BL_gas)], 1),
                    na.rm = TRUE
                ),
                forward_cal_BL_gas
            )]
            
            forward_quotes_gas[, RIC := as.character(i)]
            forward_quotes_gas[, `:=` (year = as.numeric(year), quarter = as.numeric(quarter), month = as.numeric(month))]
            return(forward_quotes_gas)
        })
        
        dt_fwd_gas = rbindlist(lst_forward_quotes_gas)
        dt_fwd_gas[, `:=` (year = as.numeric(year), quarter = as.numeric(quarter), month = as.numeric(month))]
        
        return(dt_fwd_gas)
        
        
    } else if (type == 'PWR') {
        
        ## POWER
        
        # Copy the input data table
        forward_quotes = copy(DT)
        
        # Filtering based on the 'B' or 'P' suffix for different types
        forward_quotes_BL = forward_quotes[quote %like% paste0(list_rics, 'B', collapse = '|')]
        forward_quotes_PL = forward_quotes[quote %like% paste0(list_rics, 'P', collapse = '|')]
        
        #### Baseload Processing -------------------------------------------
        forward_quotes_BL[, `:=` (
            year = paste0(
                20, fcase(
                    substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                    paste0(
                        substr(quote, nchar(quote), nchar(quote)), 
                        substr(quote, nchar(quote) - 2, nchar(quote) - 2)
                    ),
                    substr(quote, nchar(quote), nchar(quote)) >= substr(year(Sys.Date()), 4, 4),
                    paste0(
                        as.integer(substr(year(Sys.Date()), 3, 3)), 
                        substr(quote, nchar(quote), nchar(quote))
                    ),
                    default = paste0(
                        as.integer(substr(year(Sys.Date()), 3, 3)) + 1, 
                        substr(quote, nchar(quote), nchar(quote))
                    )
                )
            ),
            code = fcase(
                substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                substr(quote, nchar(quote) - 3, nchar(quote) - 3),
                default = substr(quote, nchar(quote) - 1, nchar(quote) - 1)
            )
        )]
        
        string_pwr_month_BL = paste(paste0(list_rics, 'BM'), collapse = '|')
        string_pwr_quarter_BL = paste(paste0(list_rics, 'BQ'), collapse = '|')
        string_pwr_cal_BL = paste(paste0(list_rics, 'BY'), collapse = '|')
        
        forward_quotes_month_BL = HPFC::reuters_months[forward_quotes_BL[quote %like% string_pwr_month_BL], on = 'code']
        forward_quotes_quarter_BL = HPFC::reuters_quarters_PWR[forward_quotes_BL[quote %like% string_pwr_quarter_BL], on = 'code']
        forward_quotes_cal_BL = HPFC::reuters_quarters_PWR[forward_quotes_BL[quote %like% string_pwr_cal_BL], on = 'code'][, year := paste0(ifelse(substr(year(Sys.Date()), 4, 4) > substr(quote, 6, 6), paste0(substr(year(Sys.Date()), 1, 2), "3"), substr(year(Sys.Date()), 1, 3)), substr(quote, 6, 6))][, quarter := NULL]
        
        setcolorder(forward_quotes_cal_BL, c("year", names(forward_quotes_cal_BL)[1:(ncol(forward_quotes_cal_BL) - 1)]))
        
        setnames(forward_quotes_month_BL, 'value', 'forward_month_BL_pwr')
        setnames(forward_quotes_quarter_BL, 'value', 'forward_quarter_BL_pwr')
        setnames(forward_quotes_cal_BL, 'value', 'forward_cal_BL_pwr')
        
        rm(string_pwr_month_BL, string_pwr_quarter_BL, string_pwr_cal_BL, forward_quotes_BL)
        
        #### Peakload Processing -------------------------------------------
        forward_quotes_PL[, `:=` (
            year = paste0(
                20, fcase(
                    substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                    paste0(
                        substr(quote, nchar(quote), nchar(quote)), 
                        substr(quote, nchar(quote) - 2, nchar(quote) - 2)
                    ),
                    substr(quote, nchar(quote), nchar(quote)) >= substr(year(Sys.Date()), 4, 4),
                    paste0(
                        as.integer(substr(year(Sys.Date()), 3, 3)), 
                        substr(quote, nchar(quote), nchar(quote))
                    ),
                    default = paste0(
                        as.integer(substr(year(Sys.Date()), 3, 3)) + 1, 
                        substr(quote, nchar(quote), nchar(quote))
                    )
                )
            ),
            code = fcase(
                substr(quote, nchar(quote) - 1, nchar(quote) - 1) == "^",
                substr(quote, nchar(quote) - 3, nchar(quote) - 3),
                default = substr(quote, nchar(quote) - 1, nchar(quote) - 1)
            )
        )]
        
        string_pwr_month_PL = paste(paste0(list_rics, 'PM'), collapse = '|')
        string_pwr_quarter_PL = paste(paste0(list_rics, 'PQ'), collapse = '|')
        string_pwr_cal_PL = paste(paste0(list_rics, 'PY'), collapse = '|')
        
        forward_quotes_month_PL = HPFC::reuters_months[forward_quotes_PL[quote %like% string_pwr_month_PL], on = 'code']
        forward_quotes_quarter_PL = HPFC::reuters_quarters_PWR[forward_quotes_PL[quote %like% string_pwr_quarter_PL], on = 'code']
        forward_quotes_cal_PL = HPFC::reuters_quarters_PWR[forward_quotes_PL[quote %like% string_pwr_cal_PL], on = 'code'][, year := paste0(ifelse(substr(year(Sys.Date()), 4, 4) > substr(quote, 6, 6), paste0(substr(year(Sys.Date()), 1, 2), "3"), substr(year(Sys.Date()), 1, 3)), substr(quote, 6, 6))][, quarter := NULL]
        
        setcolorder(forward_quotes_cal_PL, c("year", names(forward_quotes_cal_PL)[1:(ncol(forward_quotes_cal_PL) - 1)]))
        
        setnames(forward_quotes_month_PL, 'value', 'forward_month_PL_pwr')
        setnames(forward_quotes_quarter_PL, 'value', 'forward_quarter_PL_pwr')
        setnames(forward_quotes_cal_PL, 'value', 'forward_cal_PL_pwr')
        
        rm(string_pwr_month_PL, string_pwr_quarter_PL, string_pwr_cal_PL, forward_quotes_PL)
        
        forward_quotes_pwr = calendar_sim[date >= start_date & date <= end_date, .(year, month, quarter)] |> unique()
        
        lst_forward_quotes_pwr = lapply(list_rics, function(i) {
            
            forward_quotes_month_BL   = forward_quotes_month_BL[quote %like% paste0(as.character(i),'B')]
            forward_quotes_quarter_BL = forward_quotes_quarter_BL[quote %like% paste0(as.character(i),'B')]
            forward_quotes_cal_BL     = forward_quotes_cal_BL[quote %like% paste0(as.character(i),'B')]
            
            set(forward_quotes_month_BL, j   = setdiff(names(forward_quotes_month_BL), c("month", "year", "forward_month_BL_pwr")), value = NULL)
            set(forward_quotes_quarter_BL, j = setdiff(names(forward_quotes_quarter_BL), c("quarter", "year", "forward_quarter_BL_pwr")), value = NULL)
            set(forward_quotes_cal_BL, j     = setdiff(names(forward_quotes_cal_BL), c("quarter", "year", "forward_cal_BL_pwr")), value = NULL)
            
            forward_quotes_month_PL   = forward_quotes_month_PL[quote %like% paste0(as.character(i),'P')]
            forward_quotes_quarter_PL = forward_quotes_quarter_PL[quote %like% paste0(as.character(i),'P')]
            forward_quotes_cal_PL     = forward_quotes_cal_PL[quote %like% paste0(as.character(i),'P')]
            
            set(forward_quotes_month_PL, j   = setdiff(names(forward_quotes_month_PL), c("month", "year", "forward_month_PL_pwr")), value = NULL)
            set(forward_quotes_quarter_PL, j = setdiff(names(forward_quotes_quarter_PL), c("quarter", "year", "forward_quarter_PL_pwr")), value = NULL)
            set(forward_quotes_cal_PL, j     = setdiff(names(forward_quotes_cal_PL), c("quarter", "year", "forward_cal_PL_pwr")), value = NULL)
            
            # Merging forward curves
            forward_quotes_pwr = forward_quotes_month_BL[forward_quotes_pwr, on = c('month', 'year')]
            forward_quotes_pwr = forward_quotes_quarter_BL[forward_quotes_pwr, on = c('quarter', 'year')]
            forward_quotes_pwr = forward_quotes_cal_BL[forward_quotes_pwr, on = c('year')]
            forward_quotes_pwr = forward_quotes_month_PL[forward_quotes_pwr, on = c('month', 'year')]
            forward_quotes_pwr = forward_quotes_quarter_PL[forward_quotes_pwr, on = c('quarter', 'year')]
            forward_quotes_pwr = forward_quotes_cal_PL[forward_quotes_pwr, on = c('year')]
            
            suppressWarnings(
                forward_quotes_pwr[, forward_cal_BL_pwr := ifelse(is.na(forward_cal_BL_pwr) & is.na(forward_quarter_BL_pwr) & is.na(forward_month_BL_pwr),
                                                                  max(tail(forward_quotes_pwr$forward_quarter_BL_pwr[!is.na(forward_quotes_pwr$forward_quarter_BL_pwr)], 1),
                                                                      tail(forward_quotes_pwr$forward_month_BL_pwr[!is.na(forward_quotes_pwr$forward_month_BL_pwr)], 1),
                                                                      tail(forward_quotes_pwr$forward_cal_BL_pwr[!is.na(forward_quotes_pwr$forward_cal_BL_pwr)], 1),
                                                                      na.rm = T), forward_cal_BL_pwr)]
            )
            suppressWarnings(
                forward_quotes_pwr[, forward_cal_PL_pwr := ifelse(is.na(forward_cal_PL_pwr) & is.na(forward_quarter_PL_pwr) & is.na(forward_month_PL_pwr),
                                                                  max(tail(forward_quotes_pwr$forward_quarter_PL_pwr[!is.na(forward_quotes_pwr$forward_quarter_PL_pwr)], 1),
                                                                      tail(forward_quotes_pwr$forward_month_PL_pwr[!is.na(forward_quotes_pwr$forward_month_PL_pwr)], 1),
                                                                      tail(forward_quotes_pwr$forward_cal_PL_pwr[!is.na(forward_quotes_pwr$forward_cal_PL_pwr)], 1),
                                                                      na.rm = T), forward_cal_PL_pwr)]
            )
            
            forward_quotes_pwr[, RIC := as.character(i)]
            
            return(forward_quotes_pwr)
            
        })
        
        dt_fwd_pwr = do.call(rbind, lst_forward_quotes_pwr)
        dt_fwd_pwr = dt_fwd_pwr[, `:=` (year = as.numeric(year), quarter = as.numeric(quarter), month = as.numeric(month))]
        
        return(dt_fwd_pwr)
        
    }
}
