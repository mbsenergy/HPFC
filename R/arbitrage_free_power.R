
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
  kc_cols(forward_quotes1, colnames_forward)
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

#
# market='GR'
# fw_quote_year=2022
# last_hpfc_y=2024
#
# #### create reuters code
# reuters_months = as.data.table(cbind(as.numeric(seq(1:12)), c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")))
# setnames(reuters_months, names(reuters_months), c("month", "code"))
#
# reuters_quarters = as.data.table(cbind(as.numeric(seq(1:4)), c("F", "J", "N", "V")))
# setnames(reuters_quarters, names(reuters_quarters), c("quarter", "code"))
#
# reuters_quarters_TTF = as.data.table(cbind(as.numeric(seq(1:4)), c("H", "M", "U", "Z")))
# setnames(reuters_quarters_TTF, names(reuters_quarters_TTF), c("quarter", "code"))
#
# # LOAD BASELOAD
#
#
# #### import forward BaseLoad
# forward_quotes_imp_BL = data.frame(t(read.xlsx(file.path('..','HPFC', 'data', '1_data_raw', 'PRICES DATABASE_update.xlsm'), sheet = paste0("EEX ", market), detectDates = TRUE))) |> setDT()
# #### import year forward est. from console
# forward_quotes_cal_BL = read.xlsx(file.path('..','HPFC', 'data', '1_data_raw', "Console.xlsx"), sheet = "Calendar BL", detectDates = T)[, 1:4] |> setDT()
#
# #### clean
# forward_quotes_imp_BL = forward_quotes_imp_BL[X1 != "The record could not be found"]
# forward_quotes_BL = forward_quotes_imp_BL[-1, c(2,4)]
# setnames(forward_quotes_BL, names(forward_quotes_BL), c("quote", "value"))
# forward_quotes_BL = forward_quotes_BL[!is.na(value) & tolower(value) != "invalid ric." & value != "#N/A"]
#
# kc_cols(forward_quotes_cal_BL, c('Year', paste0("BL_Cal_", market)))
# setnames(forward_quotes_cal_BL,  names(forward_quotes_cal_BL), c('year', 'forward_cal_BL'))
#
# #### create key to merge on reuter
# forward_quotes_BL[, `:=` (year = paste(202, substr(quote, nchar(quote), nchar(quote)), sep = ''), code = substr(quote, nchar(quote) - 1, nchar(quote) - 1))]
#
# #### merge on reuter code
# forward_quotes_month_BL = reuters_months[forward_quotes_BL[grepl("BM", quote), ], on = 'code']
# forward_quotes_quarter_BL = reuters_quarters[forward_quotes_BL[grepl("BQ", quote), ], on = 'code']
#
# setnames(forward_quotes_month_BL, 'value', 'forward_month_BL')
# setnames(forward_quotes_quarter_BL, 'value', 'forward_quarter_BL')
#
# # LOAD PEAKLOAD
#
#
# #### import forward PeakLoad
# forward_quotes_imp_PL = data.frame(t(read.xlsx(file.path('..','HPFC', 'data', '1_data_raw', 'PRICES DATABASE_update.xlsm'), sheet = paste0("EEX ", market, ' PL'), detectDates = TRUE))) |> setDT()
# #### import year forward est. from console
# forward_quotes_cal_PL = read.xlsx(file.path('..','HPFC', 'data', '1_data_raw', "Console.xlsx"), sheet = "Calendar PL", detectDates = TRUE)[, 1:4] |> setDT()
#
# #### clean
# forward_quotes_imp_PL =forward_quotes_imp_PL[X1 != "The record could not be found"]
# if(!('X4' %chin% colnames(forward_quotes_imp_PL))){forward_quotes_imp_PL[,X4 := NA_character_]}
# forward_quotes_PL = forward_quotes_imp_PL[-1, c(2, 4)]
# setnames(forward_quotes_PL, names(forward_quotes_PL), c("quote", "value"))
# forward_quotes_PL = forward_quotes_PL[!is.na(value) & tolower(value) != "invalid ric." & value != "#N/A"]
#
# kc_cols(forward_quotes_cal_PL, c('Year', paste0("PL_Cal_", market)))
# setnames(forward_quotes_cal_PL, names(forward_quotes_cal_PL), c('year', 'forward_cal_PL'))
#
# #### create key to merge on reuter
# forward_quotes_PL[, `:=` (year = paste(202, substr(quote, nchar(quote), nchar(quote)), sep = ''), code = substr(quote, nchar(quote) - 1, nchar(quote) - 1))]
#
# #merge on reuter code
# forward_quotes_month_PL = reuters_months[forward_quotes_PL[grepl("PM", quote)], on = 'code']
# forward_quotes_quarter_PL = reuters_quarters[forward_quotes_PL[grepl("PQ", quote)], on = 'code']
#
# setnames(forward_quotes_month_PL, 'value', 'forward_month_PL')
# setnames(forward_quotes_quarter_PL, 'value', 'forward_quarter_PL')
#
#
#
# # 3.2 MERGE PL_BL -----------------------------------------------------------------------------------------------------------------------------------
# # forward_q=merge_BL_PL(quotes_power_BL,quotes_power_PL,calendar_holidays,2021,2024)
#
# calendar_holidays[,`:=`(month = as.character(data.table::month(date)),
#                         year = as.character(data.table::year(date)),
#                         quarter = as.character(data.table::quarter(date)))]
#
# forward_quotes1 = calendar_holidays[year >= fw_quote_year & year <= last_hpfc_y, .(year, month, quarter)] |> unique()
#
#
# #### merge fwd curve
# forward_quotes1 = forward_quotes_month_BL[forward_quotes1, on = c('month', 'year')]
# forward_quotes1 = forward_quotes_quarter_BL[forward_quotes1, on = c('quarter', 'year')]
#
# forward_quotes1 = forward_quotes_cal_BL[, year := as.character(year)][forward_quotes1, on = 'year']
# forward_quotes1 = forward_quotes_month_PL[forward_quotes1, on = c('month', 'year')]
# forward_quotes1 = forward_quotes_quarter_PL[forward_quotes1, on = c('quarter', 'year')]
# forward_quotes1 = forward_quotes_cal_PL[, year := as.character(year)][forward_quotes1, on = 'year']
#
# kc_cols(forward_quotes1, c('forward_month_PL', 'forward_quarter_PL', 'forward_month_BL', 'forward_quarter_BL', 'forward_cal_PL', 'forward_cal_BL', 'year', 'quarter', 'month'))
# forward_quotes1 = forward_quotes1[, lapply(.SD, as.numeric)]
# setcolorder(forward_quotes1, c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))
#
#
#
# filtered_dam_dd=readRDS(file = file.path('..', 'HPFC','data', 'data_package', "filtered_dam_dd.rds"))
# colnames_vec=c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL')
#
# arbitrage_free_power=function(dataframe,time_series_past,colnames_forward){
#
#   for (i in colnames_forward){
#     if (!(i %in% colnames(dataframe)) | class(dataframe[,.(get(i))][[1]]) != 'numeric') {stop(paste(i,"column must be format numeric"))}
#   }
#
#   forward_quotes1=copy(dataframe)
#   kc_cols(forward_quotes1, colnames_forward)
#   setnames(forward_quotes1, colnames(dataframe), c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))
#
#   if (!('date' %in% colnames(time_series_past)) | class(time_series_past$date) != 'Date') {stop("date column must be format Date")
#   } else if (!('smp_day' %in% colnames(time_series_past)) | class(time_series_past$smp_day) != 'numeric') {stop("smp_day column must be format numeric")
#   }
#
#   smp_prev_year=copy(time_series_past)
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
#   forward_quotes1 = forward_quotes1[, lapply(.SD, as.numeric)]
#
#   forward_quotes1 = share_prev_year[forward_quotes1, on = 'month']
#   forward_quotes1[, BL_prev_m := m_over_q*sum(forward_month_BL, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL), na.rm=TRUE), by=c('year', 'quarter')]
#
#   #### create arbitrage free calendar
#   forward_quotes1[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
#   forward_quotes1[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]
#
#   forward_quotes1[, BL_quotes := fcase(
#     is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
#     is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
#     is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
#     !is.na(forward_month_BL), forward_month_BL)
#   ]
#
#
#   forward_quotes1[, PL_empty_m := (sum(forward_quarter_PL, na.rm = TRUE) - sum(forward_month_PL, na.rm = TRUE)) / sum(is.na(forward_month_PL)), by = list(quarter, year)]
#   forward_quotes1[, PL_empty_q := (sum(forward_cal_PL, na.rm = TRUE) - sum(forward_quarter_PL, na.rm = TRUE)) / sum(is.na(forward_quarter_PL)), by = year]
#   forward_quotes1[, PL_empty_m := fifelse(PL_empty_m == 0, PL_empty_q, PL_empty_m)]
#
#   forward_quotes1[, PL_quotes := fifelse(!is.na(forward_month_PL), forward_month_PL, PL_empty_m)]
#
#   return(forward_quotes1)
#
# }
#
# f1=arbitrage_free_power(forward_BLPL,filtered_dam_dd,colnames_vec)
# saveRDS(f1, file = file.path('..', 'HPFC','data', 'data_package', "free_fwd_power.rds"))
