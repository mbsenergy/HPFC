
#' Transform forward price in a choerent way to eliminate arbitrage
#'
#' The mean of each period should be equal to the mean price of the same period in other time-frame ...
#'
#' @param x A dataframe with forward prices.
#' @param y A dataframe with historic daily prices
#' @param z a vector with 6 column names of dataframe: year, quarter, month, yearBL, quarterBL, monthBL.
#' @returns A dataframe with 3 columns: month,year,trade_close
#' @import data.table
#' @export


arbitrage_free_gas = function(dataframe, time_series_past, colnames_forward) {

  if (any(sapply(dataframe, class) != 'numeric')) {stop("all columns must be format numeric")}

  forward_quotes_TTF = copy(dataframe)
  set(forward_quotes_TTF, , names(forward_quotes_TTF)[!names(forward_quotes_TTF) %in% colnames_forward], NULL)
  
  setnames(forward_quotes_TTF, colnames(dataframe), c('year', 'quarter', 'month', 'forward_cal_BL_gas', 'forward_quarter_BL_gas', 'forward_month_BL_gas'))

  if (!('date' %in% colnames(time_series_past)) | class(time_series_past$date) != 'Date') {stop("date column must be format Date")
  } else if (!('trade_close' %in% colnames(time_series_past)) | class(time_series_past$trade_close) != 'numeric') {stop("trade_close column must be format numeric")
  }

  smp_prev_year=copy(time_series_past)


  smp_prev_year[, `:=` (month = as.numeric(data.table::month(date)), quarter = as.numeric(data.table::quarter(date)), year = as.numeric(data.table::year(date)))]
  smp_prev_year=smp_prev_year[, .(trade_close, month, quarter)]
  smp_prev_year[, sum_bym := sum(trade_close), by = month]
  smp_prev_year[, sum_byq := sum(trade_close), by = quarter]
  smp_prev_year[, m_over_q := sum_bym/sum_byq]
  share_prev_year = unique(smp_prev_year[,.(month, m_over_q)])

  #-------------------------------------------------------------------------------

  #TTF_param=80
  #forward_quotes_TTF[,forward_cal_BL:=TTF_param]

  forward_quotes_TTF = forward_quotes_TTF[, lapply(.SD, as.numeric)]

  forward_quotes_TTF = share_prev_year[forward_quotes_TTF, on = 'month']
  forward_quotes_TTF[, BL_gas_prev_m := m_over_q*sum(forward_month_BL_gas, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL_gas), na.rm=TRUE), by=c('year', 'quarter')]

  #### create arbitrage free calendar
  forward_quotes_TTF[, BL_empty_m := (sum(forward_quarter_BL_gas, na.rm = TRUE) - sum(forward_month_BL_gas, na.rm = TRUE)) / sum(is.na(forward_month_BL_gas)), by = list(quarter, year)]
  forward_quotes_TTF[, BL_empty_q := (sum(forward_cal_BL_gas, na.rm = TRUE) - sum(forward_quarter_BL_gas, na.rm = TRUE)) / sum(is.na(forward_quarter_BL_gas)), by = year]
  #forward_quotes1[, BL_empty_m := fifelse(BL_empty_m == 0, BL_empty_q, BL_empty_m)]
  forward_quotes_TTF[, BL_quotes_gas := fcase(
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & !is.nan(BL_gas_prev_m), BL_gas_prev_m,
    is.na(forward_month_BL_gas) & is.na(forward_quarter_BL_gas) & is.nan(BL_gas_prev_m), BL_empty_q,
    is.na(forward_month_BL_gas) & !is.na(forward_quarter_BL_gas), BL_empty_m,
    !is.na(forward_month_BL_gas), forward_month_BL_gas)
  ]

  forward_quotes_TTF = forward_quotes_TTF[, .(month, year, BL_quotes_gas, BL_gas_prev_m, forward_quarter_BL_gas, forward_month_BL_gas)]

  return(forward_quotes_TTF)

}