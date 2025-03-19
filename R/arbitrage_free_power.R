
#' Transform forward price in a choerent way to eliminate arbitrage
#'
#' The mean of each period should be equal to the mean price of the same period in other time-frame ...
#'
#' @param x A DT with forward prices.
#' @param y A DT with historic daily prices
#' @param z a vector with 9 column names of DT: year, quarter, month, yearBL, quarterBL, monthBL, yearPL, quarterPL, monthPL.
#' @returns A DT with 17 columns
#' @import data.table
#' @export

arbitrage_free_power = function(DT, DT_history, colnames_fwd){

  DTW = copy(DT)
  
  set(DTW, , names(DTW)[!names(DTW) %in% colnames_fwd], NULL)
  setnames(DTW, colnames(DT), c('year', 'quarter', 'month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))

  DTS = copy(DT_history)

  DTS[, `:=` (month = as.numeric(data.table::month(date)),
              quarter = as.numeric(data.table::quarter(date)),
              year = as.numeric(data.table::year(date)))]
  
  DTS = DTS[, .(value_day , month, quarter)]
  DTS[, sum_bym := sum(value_day ), by = month]
  DTS[, sum_byq := sum(value_day ), by = quarter]
  DTS[, m_over_q := sum_bym / sum_byq]
  share_prev_year = unique(DTS[, .(month, m_over_q)])

  #-------------------------------------------------------------------------------

  DTW = DTW[, lapply(.SD, as.numeric)]

  DTW = share_prev_year[DTW, on = 'month']
  DTW[, BL_prev_m := m_over_q * sum(forward_month_BL, na.rm = TRUE) / sum(m_over_q * !is.na(forward_month_BL), na.rm = TRUE), by = c('year', 'quarter')]

  #### create arbitrage free calendar
  DTW[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
  DTW[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]

  DTW[, BL_quotes := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
    !is.na(forward_month_BL), forward_month_BL)
  ]

  DTW[, PL_empty_m := (sum(forward_quarter_PL, na.rm = TRUE) - sum(forward_month_PL, na.rm = TRUE)) / sum(is.na(forward_month_PL)), by = list(quarter, year)]
  DTW[, PL_empty_q := (sum(forward_cal_PL, na.rm = TRUE) - sum(forward_quarter_PL, na.rm = TRUE)) / sum(is.na(forward_quarter_PL)), by = year]
  DTW[, PL_empty_m := fifelse(PL_empty_m == 0, PL_empty_q, PL_empty_m)]

  DTW[, PL_quotes := fifelse(!is.na(forward_month_PL), forward_month_PL, PL_empty_m)]

  return(DTW)

}
