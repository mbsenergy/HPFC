
#' Transform forward price in a choerent way to eliminate arbitrage
#'
#' The mean of each period should be equal to the mean price of the same period in other time-frame ...
#'
#' @param x A dataframe with forward prices.
#' @param y A dataframe with historic daily prices
#' @param z a vector with 9 column names of dataframe: year, quarter, month, yearBL, quarterBL, monthBL, yearPL, quarterPL, monthPL.
#' @returns A dataframe with 17 columns
#' @export


arbitrage_free_power=function(dataframe,time_series_past,colnames_forward){

  for (i in colnames_forward){
    if (!(i %in% colnames(dataframe)) | class(dataframe[,.(get(i))][[1]]) != 'numeric') {stop(paste(i,"column must be format numeric"))}
  }

  forward_quotes1=copy(dataframe)
  
  set(forward_quotes1, , names(forward_quotes1)[!names(forward_quotes1) %in% colnames_forward], NULL)
  setnames(forward_quotes1, colnames(dataframe), c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))

  if (!('date' %in% colnames(time_series_past)) | class(time_series_past$date) != 'Date') {stop("date column must be format Date")
  } else if (!('smp_day' %in% colnames(time_series_past)) | class(time_series_past$smp_day) != 'numeric') {stop("smp_day column must be format numeric")
  }

  smp_prev_year=copy(time_series_past)

  smp_prev_year[,':='(month=as.numeric(data.table::month(date)), quarter=as.numeric(data.table::quarter(date)), year=as.numeric(data.table::year(date)))]
  smp_prev_year=smp_prev_year[, .(smp_day,month,quarter)]
  smp_prev_year[, sum_bym:=sum(smp_day), by=month]
  smp_prev_year[, sum_byq:=sum(smp_day), by=quarter]
  smp_prev_year[, m_over_q:=sum_bym/sum_byq]
  share_prev_year=unique(smp_prev_year[,.(month,m_over_q)])

  #-------------------------------------------------------------------------------

  forward_quotes1 = forward_quotes1[, lapply(.SD, as.numeric)]

  forward_quotes1 = share_prev_year[forward_quotes1, on = 'month']
  forward_quotes1[, BL_prev_m := m_over_q*sum(forward_month_BL, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL), na.rm=TRUE), by=c('year', 'quarter')]

  #### create arbitrage free calendar
  forward_quotes1[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
  forward_quotes1[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]

  forward_quotes1[, BL_quotes := fcase(
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
    is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
    is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
    !is.na(forward_month_BL), forward_month_BL)
  ]


  forward_quotes1[, PL_empty_m := (sum(forward_quarter_PL, na.rm = TRUE) - sum(forward_month_PL, na.rm = TRUE)) / sum(is.na(forward_month_PL)), by = list(quarter, year)]
  forward_quotes1[, PL_empty_q := (sum(forward_cal_PL, na.rm = TRUE) - sum(forward_quarter_PL, na.rm = TRUE)) / sum(is.na(forward_quarter_PL)), by = year]
  forward_quotes1[, PL_empty_m := fifelse(PL_empty_m == 0, PL_empty_q, PL_empty_m)]

  forward_quotes1[, PL_quotes := fifelse(!is.na(forward_month_PL), forward_month_PL, PL_empty_m)]

  return(forward_quotes1)

}
