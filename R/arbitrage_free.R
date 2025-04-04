#' Calculate Arbitrage-Free Power Prices
#'
#' This function calculates arbitrage-free power prices using forward and historical data.
#'
#' @param DT A data.table containing the current forward price data.
#' @param DT_history A data.table containing the historical daily data.
#' @param colnames_fwd A vector of column names to exclude from `DT`.
#' @return A data.table with arbitrage-free power prices.
#' @import data.table
#' @export

arbitrage_free_power = function(DT, DT_history, colnames_fwd) {
  
  # Copy input data
  DTW = copy(DT)
  
  # Remove unnecessary columns and rename others
  set(DTW, j = names(DTW)[!names(DTW) %in% colnames_fwd], value = NULL)
  setnames(DTW, old = names(DT), new = c('year', 'quarter', 'month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))
  
  # Process historical data
  DTS = copy(DT_history)
  DTS[, `:=`(month = as.numeric(month(date)), quarter = as.numeric(quarter(date)), year = as.numeric(year(date)))]
  
  DTS = DTS[, .(value_day, month, quarter)]
  DTS[, sum_bym := sum(value_day), by = month]
  DTS[, sum_byq := sum(value_day), by = quarter]
  DTS[, m_over_q := sum_bym / sum_byq]
  
  # Share of previous year for each month
  share_prev_year = unique(DTS[, .(month, m_over_q)])
  
  #-------------------------------------------------------------------------------
  
  # Convert to numeric for processing
  DTW = DTW[, lapply(.SD, as.numeric)]
  
  # Merge historical share with forward data
  DTW = share_prev_year[DTW, on = 'month']
  
  # Calculate BL_prev_m for each quarter and year
  DTW[, BL_prev_m := m_over_q * sum(forward_month_BL, na.rm = TRUE) / sum(m_over_q * !is.na(forward_month_BL), na.rm = TRUE), by = c('year', 'quarter')]
  
  #-------------------------------------------------------------------------------
  
  # Create arbitrage-free calendar for BL quotes
  DTW[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = .(quarter, year)]
  DTW[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]
  
  # Use fcase to determine BL quotes
  DTW[, BL_quotes := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
    !is.na(forward_month_BL), forward_month_BL
  )]
  
  #-------------------------------------------------------------------------------
  
  # Create arbitrage-free calendar for PL quotes
  DTW[, PL_empty_m := (sum(forward_quarter_PL, na.rm = TRUE) - sum(forward_month_PL, na.rm = TRUE)) / sum(is.na(forward_month_PL)), by = .(quarter, year)]
  DTW[, PL_empty_q := (sum(forward_cal_PL, na.rm = TRUE) - sum(forward_quarter_PL, na.rm = TRUE)) / sum(is.na(forward_quarter_PL)), by = year]
  DTW[, PL_empty_m := fifelse(PL_empty_m == 0, PL_empty_q, PL_empty_m)]
  
  # Use fcase to determine PL quotes
  DTW[, PL_quotes := fifelse(!is.na(forward_month_PL), forward_month_PL, PL_empty_m)]
  
  return(DTW)
}



#' Calculate Arbitrage-Free Gas Prices
#'
#' This function calculates arbitrage-free gas prices using forward and historical data.
#'
#' @param DT A data.table containing the current forward price data for gas.
#' @param DT_history A data.table containing the historical daily gas data.
#' @param colnames_fwd A vector of column names to exclude from `DT`.
#' @return A data.table with arbitrage-free gas prices.
#' @import data.table
#' @export

arbitrage_free_gas = function(DT, DT_history, colnames_fwd) {
  
  # Ensure all columns are numeric
  if (any(sapply(DT, class) != 'numeric')) { stop("all columns must be format numeric") }
  
  # Copy and process forward data table
  DTW = copy(DT)
  set(DTW, j = names(DTW)[!names(DTW) %in% colnames_fwd], value = NULL)
  setnames(DTW, old = names(DT), new = c('year', 'quarter', 'month', 'forward_cal_BL_gas', 'forward_quarter_BL_gas', 'forward_month_BL_gas'))
  
  # Check historical data format
  
  # Process historical data
  DTS = copy(DT_history)
  DTS[, `:=`(month = as.numeric(data.table::month(date)), quarter = as.numeric(data.table::quarter(date)), year = as.numeric(data.table::year(date)))]
  DTS = DTS[, .(value, month, quarter)]
  DTS[, sum_bym := sum(value), by = month]
  DTS[, sum_byq := sum(value), by = quarter]
  DTS[, m_over_q := sum_bym / sum_byq]
  share_prev_year = unique(DTS[, .(month, m_over_q)])
  
  #-------------------------------------------------------------------------------
  
  # Convert to numeric for processing
  DTW = DTW[, lapply(.SD, as.numeric)]
  
  # Merge historical share with forward data
  DTW = share_prev_year[DTW, on = 'month']
  
  # Calculate BL_gas_prev_m for each quarter and year
  DTW[, BL_gas_prev_m := m_over_q * sum(forward_month_BL_gas, na.rm = TRUE) / sum(m_over_q * !is.na(forward_month_BL_gas), na.rm = TRUE), by = c('year', 'quarter')]
  
  #-------------------------------------------------------------------------------
  
  # Create arbitrage-free calendar for BL quotes
  DTW[, BL_empty_m := (sum(forward_quarter_BL_gas, na.rm = TRUE) - sum(forward_month_BL_gas, na.rm = TRUE)) / sum(is.na(forward_month_BL_gas)), by = .(quarter, year)]
  DTW[, BL_empty_q := (sum(forward_cal_BL_gas, na.rm = TRUE) - sum(forward_quarter_BL_gas, na.rm = TRUE)) / sum(is.na(forward_quarter_BL_gas)), by = year]
  
  # Use fcase to determine BL quotes
  DTW[, BL_quotes_gas := fcase(
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & !is.nan(BL_gas_prev_m), BL_gas_prev_m,
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & is.nan(BL_gas_prev_m), BL_empty_q,
    is.na(forward_month_BL_gas) & !is.na(forward_quarter_BL_gas), BL_empty_m,
    !is.na(forward_month_BL_gas), forward_month_BL_gas
  )]
  
  # Final output
  DTW = DTW[, .(month, year, BL_quotes_gas, BL_gas_prev_m, forward_quarter_BL_gas, forward_month_BL_gas)]
  
  return(DTW)
}
