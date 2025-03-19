
#' Transform forward price in a choerent way to eliminate arbitrage
#'
#' The mean of each period should be equal to the mean price of the same period in other time-frame ...
#'
#' @param x A DT with forward prices.
#' @param y A DT with historic daily prices
#' @param z a vector with 6 column names of DT: year, quarter, month, yearBL, quarterBL, monthBL.
#' @returns A DT with 3 columns: month,year,value
#' @import data.table
#' @export


arbitrage_free_gas = function(DT, DT_history, colnames_fwd) {

  if (any(sapply(DT, class) != 'numeric')) {stop("all columns must be format numeric")}

  DTW = copy(DT)
  set(DTW, , names(DTW)[!names(DTW) %in% colnames_fwd], NULL)
  
  setnames(DTW, colnames(DT), c('year', 'quarter', 'month', 'forward_cal_BL_gas', 'forward_quarter_BL_gas', 'forward_month_BL_gas'))

  if (!('date' %in% colnames(DT_history)) | class(DT_history$date) != 'Date') {stop("date column must be format Date")
  } else if (!('value' %in% colnames(DT_history)) | class(DT_history$value) != 'numeric') {stop("value column must be format numeric")
  }

  DTS=copy(DT_history)

  DTS[, `:=` (month = as.numeric(data.table::month(date)), quarter = as.numeric(data.table::quarter(date)), year = as.numeric(data.table::year(date)))]
  DTS = DTS[, .(value, month, quarter)]
  DTS[, sum_bym := sum(value), by = month]
  DTS[, sum_byq := sum(value), by = quarter]
  DTS[, m_over_q := sum_bym/sum_byq]
  share_prev_year = unique(DTS[,.(month, m_over_q)])

  #-------------------------------------------------------------------------------

  DTW = DTW[, lapply(.SD, as.numeric)]

  DTW = share_prev_year[DTW, on = 'month']
  DTW[, BL_gas_prev_m := m_over_q*sum(forward_month_BL_gas, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL_gas), na.rm=TRUE), by=c('year', 'quarter')]

  #### create arbitrage free calendar
  DTW[, BL_empty_m := (sum(forward_quarter_BL_gas, na.rm = TRUE) - sum(forward_month_BL_gas, na.rm = TRUE)) / sum(is.na(forward_month_BL_gas)), by = list(quarter, year)]
  DTW[, BL_empty_q := (sum(forward_cal_BL_gas, na.rm = TRUE) - sum(forward_quarter_BL_gas, na.rm = TRUE)) / sum(is.na(forward_quarter_BL_gas)), by = year]
  #forward_quotes1[, BL_empty_m := fifelse(BL_empty_m == 0, BL_empty_q, BL_empty_m)]
  DTW[, BL_quotes_gas := fcase(
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & !is.nan(BL_gas_prev_m), BL_gas_prev_m,
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & is.nan(BL_gas_prev_m), BL_empty_q,
    is.na(forward_month_BL_gas) & !is.na(forward_quarter_BL_gas), BL_empty_m,
    !is.na(forward_month_BL_gas), forward_month_BL_gas)
  ]

  DTW = DTW[, .(month, year, BL_quotes_gas, BL_gas_prev_m, forward_quarter_BL_gas, forward_month_BL_gas)]

  return(DTW)

}