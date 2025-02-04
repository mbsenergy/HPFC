
#' Transform forward price in a choerent way to eliminate arbitrage
#'
#' The mean of each period should be equal to the mean price of the same period in other time-frame ...
#'
#' @param x A dataframe with forward prices.
#' @param y A dataframe with historic daily prices
#' @param z a vector with 6 column names of dataframe: year, quarter, month, yearBL, quarterBL, monthBL.
#' @returns A dataframe with 3 columns: month,year,trade_close
#' @export


arbitrage_free_gas=function(dataframe,time_series_past,colnames_forward){



  if (any(sapply(dataframe, class) != 'numeric')) {stop("all columns must be format numeric")}


  forward_quotes_TTF=copy(dataframe)
  kc_cols(forward_quotes_TTF, colnames_forward)
  setnames(forward_quotes_TTF, colnames(dataframe), c('year','quarter','month', 'forward_cal_BL_gas', 'forward_quarter_BL_gas', 'forward_month_BL_gas'))

  if (!('date' %in% colnames(time_series_past)) | class(time_series_past$date) != 'Date') {stop("date column must be format Date")
  } else if (!('trade_close' %in% colnames(time_series_past)) | class(time_series_past$trade_close) != 'numeric') {stop("trade_close column must be format numeric")
  }

  smp_prev_year=copy(time_series_past)


  smp_prev_year[,':='(month=as.numeric(data.table::month(date)), quarter=as.numeric(data.table::quarter(date)), year=as.numeric(data.table::year(date)))]
  smp_prev_year=smp_prev_year[, .(trade_close,month,quarter)]
  smp_prev_year[, sum_bym:=sum(trade_close), by=month]
  smp_prev_year[, sum_byq:=sum(trade_close), by=quarter]
  smp_prev_year[, m_over_q:=sum_bym/sum_byq]
  share_prev_year=unique(smp_prev_year[,.(month,m_over_q)])

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

  kc_cols(forward_quotes_TTF, c('month', 'year', 'BL_quotes_gas', 'BL_gas_prev_m', 'forward_quarter_BL_gas', 'forward_month_BL_gas'))

  return(forward_quotes_TTF)

}

#
# fw_quote_year=2022
# last_hpfc_y=2024
#
# #### import forward BaseLoad
# TTF_forward_quotes_imp_BL = data.frame(t(read.xlsx(file.path('..', 'HPFC','data', '1_data_raw', "PRICES DATABASE_update.xlsm"), sheet = paste0("GAS"), detectDates = TRUE))) |> setDT()
# #### import year forward est. from console
# TTF_forward_quotes_cal_BL = read.xlsx(file.path('..','HPFC', 'data', '1_data_raw', "Console.xlsx"), sheet = "Calendar Gas", detectDates = TRUE) |> setDT()
#
# TTF_forward_quotes_imp_BL = TTF_forward_quotes_imp_BL[X1 != "The record could not be found"]
# TTF_forward_quotes_BL = TTF_forward_quotes_imp_BL[-1, c(2,4)]
# setnames(TTF_forward_quotes_BL, names(TTF_forward_quotes_BL), c("quote", "value"))
# TTF_forward_quotes_BL = TTF_forward_quotes_BL[!is.na(value) & tolower(value) != "invalid ric." & value != "#N/A"]
#
# setnames(TTF_forward_quotes_cal_BL, names(TTF_forward_quotes_cal_BL), c('year', 'forward_cal_BL'))
#
# #### create key to merge on reuter
# TTF_forward_quotes_BL[, `:=` (year = paste(202, substr(quote, nchar(quote), nchar(quote)), sep = ''), code = substr(quote, nchar(quote) - 1, nchar(quote) - 1))]
#
# #### merge on reuter code
# TTF_forward_quotes_month_BL = reuters_months[TTF_forward_quotes_BL[grepl("BM", quote), ], on = 'code']
# TTF_forward_quotes_quarter_BL = reuters_quarters_TTF[TTF_forward_quotes_BL[grepl("BQ", quote), ], on = 'code']
#
# setnames(TTF_forward_quotes_month_BL, 'value', 'forward_month_BL')
# setnames(TTF_forward_quotes_quarter_BL, 'value', 'forward_quarter_BL')
#
# forward_quotes_TTF = calendar_holidays[year >= fw_quote_year & year <= last_hpfc_y, .(year, month, quarter)] |> unique()
# #forward_quotes_TTF[,':='(month=as.character(data.table::month(date)), quarter=as.character(data.table::quarter(date)), year=as.character(data.table::year(date)))]
#
#
# #### merge fwd curve
# forward_quotes_TTF = TTF_forward_quotes_month_BL[forward_quotes_TTF, on = c('month', 'year')]
# forward_quotes_TTF = TTF_forward_quotes_quarter_BL[forward_quotes_TTF, on = c('quarter', 'year')]
# forward_quotes_TTF = TTF_forward_quotes_cal_BL[, year := as.character(year)][forward_quotes_TTF, on = 'year']
#
# kc_cols(forward_quotes_TTF, c('year', 'quarter', 'month', 'forward_cal_BL', 'forward_quarter_BL','forward_month_BL'))
# forward_quotes_TTF = forward_quotes_TTF[, lapply(.SD, as.numeric)]
# setcolorder(forward_quotes_TTF, c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL'))
#
# saveRDS(forward_quotes_TTF, file = file.path('..', 'HPFC','data', 'data_package', "forward_TTF.rds"))
#
# filtered_dam_dd=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "filtered_dam_dd.rds"))
#
# colnames_vec_gas=c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL')
#
#
# arbitrage_free_gas=function(dataframe,time_series_past,colnames_forward){
#
#
#     for (i in colnames_forward){
#       if (!(i %in% colnames(dataframe)) | class(dataframe[,.(get(i))][[1]]) != 'numeric') {stop(paste(i,"column must be format numeric"))}
#     }
#
#     forward_quotes_TTF=copy(dataframe)
#     kc_cols(forward_quotes_TTF, colnames_forward)
#     setnames(forward_quotes_TTF, colnames(dataframe), c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL'))
#
#     if (!('date' %in% colnames(time_series_past)) | class(time_series_past$date) != 'Date') {stop("date column must be format Date")
#     } else if (!('smp_day' %in% colnames(time_series_past)) | class(time_series_past$smp_day) != 'numeric') {stop("smp_day column must be format numeric")
#     }
#
#     smp_prev_year=copy(time_series_past)
#
#
#   smp_prev_year[,':='(month=as.numeric(data.table::month(date)), quarter=as.numeric(data.table::quarter(date)), year=as.numeric(data.table::year(date)))]
#   smp_prev_year=smp_prev_year[, .(smp_day,month,quarter)]
#   smp_prev_year[, sum_bym:=sum(smp_day), by=month]
#   smp_prev_year[, sum_byq:=sum(smp_day), by=quarter]
#   smp_prev_year[, m_over_q:=sum_bym/sum_byq]
#   share_prev_year=unique(smp_prev_year[,.(month,m_over_q)])
#
#   #-------------------------------------------------------------------------------
#
#   #TTF_param=80
#   #forward_quotes_TTF[,forward_cal_BL:=TTF_param]
#
#   forward_quotes_TTF = forward_quotes_TTF[, lapply(.SD, as.numeric)]
#
#   forward_quotes_TTF = share_prev_year[forward_quotes_TTF, on = 'month']
#   forward_quotes_TTF[, BL_prev_m := m_over_q*sum(forward_month_BL, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL), na.rm=TRUE), by=c('year', 'quarter')]
#
#   #### create arbitrage free calendar
#   forward_quotes_TTF[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
#   forward_quotes_TTF[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]
#   #forward_quotes1[, BL_empty_m := fifelse(BL_empty_m == 0, BL_empty_q, BL_empty_m)]
#   forward_quotes_TTF[, trade_close := fcase(
#     is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
#     is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
#     is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
#     !is.na(forward_month_BL), forward_month_BL)
#   ]
#
#   kc_cols(forward_quotes_TTF, c('month', 'year', 'trade_close' ))
#
#   return(forward_quotes_TTF)
#
# }
#
# f1_1=arbitrage_free_ttf(forward_quotes_TTF,filtered_dam_dd,colnames_vec_gas)
# saveRDS(f1_1, file = file.path('..', 'HPFC','data', 'data_package', "free_fwd_gas.rds"))
