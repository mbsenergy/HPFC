#
# library(openxlsx)
# library(data.table)
#
#
# # UTILS ========================================================================================
#
# source(file.path('R', "functions.R"))
#
# market='GR'
#
# first_year_spot_sample = '2016-01-01'
#
# last_date = '2022-12-31'
#
# alpha = 0.4 ### weights smoothing (ex Caldana et al 2017)
#
# fw_quote_year = 2023
#
# last_hpfc_y = 2023
#
#
# ## 2.1 LOAD INPUT -------------------------------------------------------------------------------------------------------------------------
#
# # LOAD HOLIDAYS
#
# calendar_holidays = read.xlsx(file.path('..', 'HPFC','data', '1_data_raw', "Console.xlsx"), sheet = "Forward schedule", detectDates = T) |> setDT()
# setnames(calendar_holidays, paste0("holiday_", market), 'holiday')
#
# kc_cols(calendar_holidays,c('date','holiday'))
#
# # LOAD TTF
#
# history_all = read.xlsx(file.path('..','HPFC','data', '1_data_raw', 'TTF DA.xlsx')) |> setDT()
# history_all = innteamUtils::clean_names(history_all)
# history_all$date = as.Date(convertToDateTime(history_all$date), tz = 'CET')
#
# #select relevant
# history_all=history_all[date>first_year_spot_sample & date<=last_date]
#
# #select rolling period
# min_date = as.Date(paste(min(data.table::year(history_all$date)), substr(max(history_all$date), 6, 10), sep = "-"))
# history=history_all[date>min_date & date<=last_date]
#
# # fill na
# history=history[calendar_holidays[date>min_date & date<=last_date,.(date)], on='date']
# setorderv(history, cols = c('date'), order = 1L)
# history[, trade_close := nafill(trade_close, 'locf')]
# history[, trade_close := nafill(trade_close, 'nocb')]
#
# # 2.3 BREAK DETECTION ---------------------------------------------------------------------------------------------------------------------
#
#
# df_ttf=copy(history)
#
#
# df_ttf_ts_detr = ts(df_ttf$trade_close,
#                          start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
#                          frequency = 365)
#
# #### start from n of breaks = to n years, check if there is a period with less than 3 months in the last 5 years,
# #### if not stops, otherwise consider 1 break less
#
# for (j in 1:round(length(df_ttf_ts_detr)/(365))){
#
#   n_breaks=1+round(length(df_ttf_ts_detr)/(365))-j
#
#   Vvalue_h = suppressWarnings(changepoint::cpt.var(df_ttf_ts_detr, Q =n_breaks, method='BinSeg'))
#
#   cutoff=length(df_ttf_ts_detr)-((365)*5)
#
#   vec=c(0,changepoint::cpts(Vvalue_h)[changepoint::cpts(Vvalue_h)>cutoff],length(df_ttf_ts_detr))
#
#   difs=diff(vec)
#   if (!any(400>difs)){break}}
#
# #### create variable with period group
# discv_date_h=df_ttf$date[changepoint::cpts(Vvalue_h)]
# #plot(Vvalue_h, main=paste('hourly', market, discv_date_h))
# #df_ttf[,break_group_p:=findInterval(date,discv_date_h)]
#
# #kc_cols(df_ttf, c('date', 'trade_close', 'break_group_p'))
#
# # 2.4 FILTER OUTLIER ----------------------------------------------------------------------------------------------------------------------
#
#
# ## HAMILTON FILTER
# hamilton_filter = neverhpfilter::yth_filter(xts::as.xts(df_ttf), h = 24, p = 4, output = c("x", "trend", "cycle"))
# dt_hamilton_filter = as.data.table(hamilton_filter)
# setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
# dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
# dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
# dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]
#
# #df_dam_day=unique(df_dam[,.(date,break_group_p)])[df_dam_day, on='date']
#
# df_ttf[,break_group_p:=findInterval(date,discv_date_h)]
#
#
# ## add hp trend
#
# df_ttf[, hp_trend := dt_hamilton_filter$trend]
#
# # find diff over 3 std dev
# df_ttf[, delta := trade_close - (hp_trend)]
#
# df_ttf[, sd_delta := sd(delta), by=break_group_p]
#
# df_ttf[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
# df_ttf[, out_values := fifelse(out_dummy == 1, trade_close, NA_real_)]
#
# filtered_ttf_dd = df_ttf[out_dummy == 0, .(date, trade_close, out_dummy,break_group_p)]
#
#
# # 2.5 DETREND DAILY ----------------------------------------------------------------------------------------------------------------------
#
# min_date = as.Date(min(filtered_ttf_dd$date))
#
# df_ttf_dd_ts = ts(filtered_ttf_dd$trade_close,
#                   start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
#                   frequency = 365)
#
#
# #add HP trend to dataframe daily
# hp_filter2 = mFilter::hpfilter(df_ttf_dd_ts,
#                                freq = 8.322 * 10 ^ 7,  ### 8.322 * 10 ^ 7
#                                type = "lambda",
#                                drift = F) ### lambda as in Caldana et al (2017)
#
# filtered_ttf_dd[, hp_trend := hp_filter2$trend]
#
# # subtract hp_trend from raw data
# filtered_ttf_dd[, detr_smp_day := trade_close - hp_trend]
#
# kc_cols(filtered_ttf_dd, c('date', 'trade_close', 'hp_trend', 'detr_smp_day', 'break_group_p'))
#
# # 2.6 CREATE LONG TERM REGRESSORS --------------------------------------------------------------------------------------------------------
#
# alpha=0.4
#
# filtered_ttf_dd[,`:=` (yday = data.table::yday(date),
#                        wday = data.table::wday(date),
#                        quarter = data.table::quarter(date),
#                        month = data.table::month(date),
#                        weekend = as.numeric(chron::is.weekend(date)),
#                        obs = .I)]
#
# #create weight with smoothing
# filtered_ttf_dd[, time_distance := (max(obs) + 1 - obs) / max(obs)] #TO CHECK / max(obs) su 365*3
# filtered_ttf_dd[, weight := exp(-alpha * (time_distance))]
#
# # create day dummies (intra-week seasonality)
# filtered_ttf_dd[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
#
# #create summer dummy
# filtered_ttf_dd[, summer := as.numeric(quarter == 2 | quarter == 3)]
#
# #create variable beginning season date AND delta date-beginning
# filtered_ttf_dd[, begining_season := as.Date(
#   fifelse(quarter == 4, paste0(format(date, format = "%Y"), "-10-01"),
#           fifelse(quarter == 1, paste0(data.table::year(date) - 1, "-10-01"),
#                   paste0(format(date, format = "%Y"), "-04-01"))))]
#
# filtered_ttf_dd[, dist := as.numeric(date - begining_season)]
# filtered_ttf_dd[, yday_season := dist / max(dist)]
#
# #create variable season.year
# filtered_ttf_dd[, season := fifelse(summer == 1, paste0("summer-", data.table::year(date)),
#                                     fifelse(quarter == 4, paste0("winter-", data.table::year(date)),
#                                             paste0("winter-", data.table::year(date) - 1)))]
#
#
# #### create LT components Caldana 6.2
# filtered_ttf_dd[,`:=`(cos_long_term = cos((2 * pi) * yday / 365),
#                       cos_season = cos((2 * pi) * yday_season),
#                       cos_season_summer = cos((2 * pi) * yday_season) * summer,
#                       sin_long_term = sin((2 * pi) * yday / 365),
#                       sin_season = sin((2 * pi) * yday_season),
#                       sin_season_summer = sin((2 * pi) * yday_season) * summer)]
#
# ### generate polynomial via lapply
# filtered_ttf_dd[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]
#
#
# # 2.7 ESTIMATE LONG TERM MODEL -----------------------------------------------------------------------------------------------------------
#
#
# filtered_ttf_dd=calendar_holidays[filtered_ttf_dd, on='date']         # merge with calendar for variable holiday
#
# # filtered_ttf_dd[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]
# #
# # filtered_ttf_dd[,`:=`(cos_long_term_p = break_group_p*cos_long_term,
# #                       cos_season_p = break_group_p*cos_season,
# #                       cos_season_summer_p = break_group_p*cos_season_summer,
# #                       sin_long_term_p = break_group_p*sin_long_term,
# #                       sin_season_p = break_group_p*sin_season,
# #                       sin_season_summer_p = break_group_p*sin_season_summer)]
#
# ### generate polynomial via lapply
# n_groups=max(filtered_ttf_dd$break_group_p)+1
# filtered_ttf_dd[, (paste("break_group", 1:n_groups, sep = "_")) := lapply(1:n_groups, function(i) {as.numeric(break_group_p==(i-1))})]
#
# for (x in 1:n_groups){
#
#   filtered_ttf_dd[,paste('cos_long_term',x,sep='_') := get(paste("break_group", x, sep = "_"))*cos_long_term]
#   filtered_ttf_dd[,paste('cos_season',x,sep='_') := get(paste("break_group", x, sep = "_"))*cos_season]
#   filtered_ttf_dd[,paste('cos_season_summer',x,sep='_') := get(paste("break_group", x, sep = "_"))*cos_season_summer]
#   filtered_ttf_dd[,paste('sin_long_term',x,sep='_') := get(paste("break_group", x, sep = "_"))*sin_long_term]
#   filtered_ttf_dd[,paste('sin_season',x,sep='_') := get(paste("break_group", x, sep = "_"))*sin_season]
#   filtered_ttf_dd[,paste('sin_season_summer',x,sep='_') := get(paste("break_group", x, sep = "_"))*sin_season_summer]
#   filtered_ttf_dd[,paste('holiday',x,sep='_') := get(paste("break_group", x, sep = "_"))*holiday]
#   filtered_ttf_dd[,paste('summer',x,sep='_') := get(paste("break_group", x, sep = "_"))*summer]
#
#   filtered_ttf_dd[,paste('day_2',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_2]
#   filtered_ttf_dd[,paste('day_3',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_3]
#   filtered_ttf_dd[,paste('day_4',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_4]
#   filtered_ttf_dd[,paste('day_5',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_5]
#   filtered_ttf_dd[,paste('day_6',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_6]
#   filtered_ttf_dd[,paste('day_7',x,sep='_') := get(paste("break_group", x, sep = "_"))*day_7]
#
#   filtered_ttf_dd[, (paste("yday", 1:10, x, sep = "_")) := lapply(1:10, function(i) {get(paste("break_group", x, sep = "_"))*get(paste("yday", i, sep = "_"))})]
#
# }
#
#
# df_reg=copy(filtered_ttf_dd)
#
# #regression ="detr_smp_day~cos_long_term+cos_season+cos_season_summer+holiday+day_2+day_3+day_4+day_5+day_6+day_7+sin_long_term+sin_season+sin_season_summer+summer"
# regression ="detr_smp_day~0"
#
#
# for (x in 1:n_groups){
#
#   regression=paste(regression,"+",paste("break_group", x, sep = "_"))
#
#   regression=paste(regression,"+",paste("cos_long_term", x, sep = "_"))
#   #regression=paste(regression,"+",paste("cos_season", x, sep = "_"))
#   #regression=paste(regression,"+",paste("cos_season_summer", x, sep = "_"))
#   regression=paste(regression,"+",paste("sin_long_term", x, sep = "_"))
#   #regression=paste(regression,"+",paste("sin_season", x, sep = "_"))
#   #regression=paste(regression,"+",paste("sin_season_summer", x, sep = "_"))
#   regression=paste(regression,"+",paste("holiday", x, sep = "_"))
#   #regression=paste(regression,"+",paste("summer", x, sep = "_"))
#
#   regression=paste(regression,"+",paste("day_2", x, sep = "_"))
#   regression=paste(regression,"+",paste("day_3", x, sep = "_"))
#   regression=paste(regression,"+",paste("day_4", x, sep = "_"))
#   regression=paste(regression,"+",paste("day_5", x, sep = "_"))
#   regression=paste(regression,"+",paste("day_6", x, sep = "_"))
#   regression=paste(regression,"+",paste("day_7", x, sep = "_"))
#
#
#   for (i in 1:10 ){
#     regression=paste(regression,"+",paste("yday", i, x, sep = "_"))
#   }
#
# }
#
# #regression=paste(regression,"+",'trade_close',"+",'trade_close2')
# model_1=eval(substitute(step(lm(regression,weights = weight, data = df_reg),direction="both")))
# filtered_ttf_dd[, LT_seas_1 := model_1$fitted.values]
#
#
# forecast_group=fifelse(min(filtered_ttf_dd[break_group_p==max(break_group_p),]$date)<as.Date('2022-01-01'),n_groups-1,n_groups)
# terms=paste(c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10', 'break_group'), forecast_group, sep='_')
#
#
# #### create model_1 coeff vector
# #terms=c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10', 'cos_long_term_p', 'cos_season_p', 'cos_season_summer_p', 'sin_long_term_p', 'sin_season_p', 'sin_season_summer_p','break_group_p')
# missing_terms=terms[!(terms %in% names(model_1$coefficients))]
# profile_matrix=as.data.table(matrix(c(model_1$coefficients,rep(0,length(missing_terms))),nrow=1))
# setnames(profile_matrix,,c(names(model_1$coefficients),missing_terms))
#
# kc_cols(profile_matrix, terms)
# setnames(profile_matrix,,substr(colnames(profile_matrix), 1, nchar(colnames(profile_matrix))-2L))
#
# ggplot(filtered_ttf_dd,
#        aes(x = date,
#            y = detr_smp_day)) +
#   geom_line(color = 'black') +
#   labs(title = "Fit LT 2",
#        y = "Weights",
#        x = "years") +
#   geom_line(aes(y = LT_seas_1), color='red',
#             linewidth = 2, alpha=0.5)
#
#
#
# ### 3
#
#
#
# # 3.1 LOAD FORWARD ---------------------------------------------------------------------------------------------------------------------
#
# #### create reuters code
# reuters_months = as.data.table(cbind(as.numeric(seq(1:12)), c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")))
# setnames(reuters_months, names(reuters_months), c("month", "code"))
#
# reuters_quarters_TTF = as.data.table(cbind(as.numeric(seq(1:4)), c("H", "M", "U", "Z")))
# setnames(reuters_quarters_TTF, names(reuters_quarters_TTF), c("quarter", "code"))
#
# reuters_months = refeHPFC::reuters_months
# reuters_quarters_PWR = refeHPFC::reuters_quarters_PWR
# reuters_quarters_GAS = refeHPFC::reuters_quarters_GAS
#
# # LOAD TTF FORWARD
#
# calendar_holidays[,`:=`(month = as.character(data.table::month(date)),
#                         year = as.character(data.table::year(date)),
#                         quarter = as.character(data.table::quarter(date)))]
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
#
#
# #### merge fwd curve
# forward_quotes_TTF = TTF_forward_quotes_month_BL[forward_quotes_TTF, on = c('month', 'year')]
# forward_quotes_TTF = TTF_forward_quotes_quarter_BL[forward_quotes_TTF, on = c('quarter', 'year')]
# forward_quotes_TTF = TTF_forward_quotes_cal_BL[, year := as.character(year)][forward_quotes_TTF, on = 'year']
#
# kc_cols(forward_quotes_TTF, c('forward_cal_BL','forward_month_BL', 'forward_quarter_BL', 'year', 'quarter', 'month'))
# forward_quotes_TTF = forward_quotes_TTF[, lapply(.SD, as.numeric)]
#
# # 3.3 CREATE ARBITRAGE FREE FWD ------------------------------------------------
#
# setcolorder(forward_quotes_TTF, c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL'))
# names_vec2=colnames(forward_quotes_TTF)
#
# smp_prev_year=copy(filtered_ttf_dd)
#
# smp_prev_year[,':='(month=as.numeric(data.table::month(date)), quarter=as.numeric(data.table::quarter(date)), year=as.numeric(data.table::year(date)))]
# smp_prev_year=smp_prev_year[, .(trade_close,month,quarter)]
# smp_prev_year[, sum_bym:=sum(trade_close), by=month]
# smp_prev_year[, sum_byq:=sum(trade_close), by=quarter]
# smp_prev_year[, m_over_q:=sum_bym/sum_byq]
# share_prev_year=unique(smp_prev_year[,.(month,m_over_q)])
#
# forward_quotes_TTF = forward_quotes_TTF[, lapply(.SD, as.numeric)]
#
# forward_quotes_TTF = share_prev_year[forward_quotes_TTF, on = 'month']
# forward_quotes_TTF[, BL_prev_m := m_over_q*sum(forward_month_BL, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL), na.rm=TRUE), by=c('year', 'quarter')]
#
# #### create arbitrage free calendar
# forward_quotes_TTF[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
# forward_quotes_TTF[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]
# #forward_quotes1[, BL_empty_m := fifelse(BL_empty_m == 0, BL_empty_q, BL_empty_m)]
# forward_quotes_TTF[, BL_quotes := fcase(
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
#   is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
#   !is.na(forward_month_BL), forward_month_BL)
# ]
#
# kc_cols(forward_quotes_TTF, c('month', 'year', 'forward_month_BL', 'forward_quarter_BL', 'BL_prev_m', 'BL_quotes' ))
#
#
# # 3.4 CREATE CALENDAR FOR FORECAST ------------------------------------------------------------------------------------------------------
#
# calendar = calendar_holidays[fw_quote_year <= data.table::year(date) & data.table::year(date) <= last_hpfc_y,]
#
#
# #### prepare forecast horizon grid
# calendar[,`:=`(month = as.numeric(data.table::month(date)),
#                year = as.numeric(data.table::year(date)),
#                quarter = as.numeric(data.table::quarter(date)))]
#
# #### yday (1:365), weekend (1:0) and wday(1:7)
# calendar[, yday := data.table::yday(date)]
# calendar[, weekend := as.numeric(chron::is.weekend(date))]
# calendar[, wday := lubridate::wday(date, label = TRUE)]
# calendar[, wday2 := data.table::wday(date)]
#
# #### filter forecast interval
# #calendar = calendar[fw_quote_year <= year & year <= last_hpfc_y,]
#
# #### create dummy for week day
# calendar[, unlist(lapply(1:7, function(i) {paste("day", i , sep = "_")})) := lapply(1:7, function(i) {fifelse(data.table::wday(date) == i, 1, 0)})]
#
# #### create dummy for summer, season, beginning, dist, yday
# calendar[, summer := as.numeric(quarter == "2" | quarter == "3")]
#
# #### create dummy for season (j1st quarter start from previous year)
# calendar[,season := fcase(
#   summer == 1, paste('summer', year, sep = '-'),
#   quarter == 1, paste('winter', as.numeric(year) - 1, sep = '-'),
#   quarter == 4, paste('winter', year, sep = '-'))]
#
# #### create beginning season
# calendar[, begining_season := as.Date(fcase(
#   summer==1, paste(year, '04-01',sep='-'),
#   quarter==1, paste(as.numeric(year)-1, '10-01',sep='-'),
#   quarter==4, paste(year, '10-01',sep='-')))]
#
# #### create time passed by beginning season
# calendar[, dist := as.numeric(date - begining_season)]
# calendar[, yday_season := dist / max(dist)]
#
# #### create yday polynomial
# calendar[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]
#
#
# # 3.5 LONG TERM CALIBRATION -------------------------------------------------------------------------------------------------------------
#
#
# #free_fwd_power=copy(forward_quotes1)
# free_fwd_gas=copy(forward_quotes_TTF)
# forecast_calendar_daily=copy(calendar)
#
# #### merge calendar with daily spot
# forecast_calendar_daily = history[forecast_calendar_daily, on = 'date']                 # merge
#
# #### merge calendar with forward
# forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]        # merge
#
# last_date=as.Date(max(history$date))
# #### spot before current date and fwd after for BL
# forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= last_date, trade_close, BL_quotes)]
# #### NA before current date and fwd after for PL
# #forecast_calendar_daily[, spot_forward_month_PL := fifelse(date <= last_date | PL_quotes <= 0, as.numeric(NA), PL_quotes)]
#
# Lt_day=copy(forecast_calendar_daily)
# #Lt_day[,trade_close2:=trade_close^2]
#
# Lt_day = Lt_day[, L_t :=
#
#                   # generate long-term seasonality (intra-year trends)  :
#
#                   cos((2 * pi / 365) * yday) * profile_matrix$cos_long_term[1] +
#                   cos((2 * pi) * yday_season) * profile_matrix$cos_season[1] +
#                   cos((2 * pi) * yday_season) * summer * profile_matrix$cos_season_summer[1] +
#                   sin((2 * pi) * yday / 365) * profile_matrix$sin_long_term[1] +
#                   sin((2 * pi) * yday_season) * profile_matrix$sin_season[1] +
#                   sin((2 * pi) * yday_season) * summer * profile_matrix$sin_season_summer[1] +
#                   summer * profile_matrix$summer[1] +
#                   profile_matrix$break_group[1] +
#
#                 # generate day-type deviations :
#
#                 holiday * profile_matrix$holiday[1] +
#                   day_1 * profile_matrix$day_1[1] +
#                   day_2 * profile_matrix$day_2[1] +
#                   day_3 * profile_matrix$day_3[1] +
#                   day_4 * profile_matrix$day_4[1] +
#                   day_5 * profile_matrix$day_5[1] +
#                   day_6 * profile_matrix$day_6[1] +
#                   day_7 * profile_matrix$day_7[1] +
#
#                   yday_1 * profile_matrix$yday_1[1] +
#                   yday_2 * profile_matrix$yday_2[1] +
#                   yday_3 * profile_matrix$yday_3[1] +
#                   yday_4 * profile_matrix$yday_4[1] +
#                   yday_5 * profile_matrix$yday_5[1] +
#                   yday_6 * profile_matrix$yday_6[1] +
#                   yday_7 * profile_matrix$yday_7[1] +
#                   yday_8 * profile_matrix$yday_8[1] +
#                   yday_9 * profile_matrix$yday_9[1] +
#                   yday_10 * profile_matrix$yday_10[1]
#
# ]
#
# # 3.7 ADJUST LONG TERM CALIBRATION BY PERIOD ---------------------------------------------------------------------------------------------
#
# #Lt_day_adjusted=refeHPFC::period_adjusting(Lt_day,last_date)
#
# Lt_day[, period := fcase(
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), paste("mese", month, year, sep = ''),
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), paste('remainder_year', year, sep = ''),
#   is.na(forward_month_BL) & !is.na(forward_quarter_BL), paste('remainder_quarter', quarter, year, sep = ''),
#   !is.na(forward_month_BL), paste("mese", month, year, sep = ''))
# ]
#
# begin_current_period = as.Date(paste(substr(last_date, 1, 7), '01', sep = "-"))
#
# #### detrend L.t by fwd period
# Lt_day[, period := fifelse(date < begin_current_period, 'history', period)]
# Lt_day[, history_forecast := fifelse(date <= last_date, 0, 1)]
#
# Lt_day[, L_t := L_t - mean(L_t), by = period]
#
# Lt_day[,':='
#        (forward_month_BL=NULL,
#          forward_quarter_BL=NULL,
#          BL_prev_m=NULL)
# ]
#
# # 3.9 APPLY SPLINE ------------------------------------------------------------------------------------------------------------------------
#
# #### explicit epsilon u as in Caldana
# Lt_day[, epsilon_u := spot_forward_month_BL]
# Lt_day[, L_e_u := L_t + epsilon_u]
#
# #Lt_lu_hh_spline=refeHPFC::apply_spline(Lt_lu_hh,15)
# smoothig_parameter=15
#
# #### create L_e_u deviation from week mean
# Lt_day[, week_n := lubridate::week(date)]
# Lt_day[, deviation_from_w_wmean := L_e_u - mean(L_e_u, na.rm = TRUE), by = .(week_n, year)]
#
# #### create daily DB with L_e_u mean to find daily spline
# DB_spline = copy(Lt_day)
# DB_spline[, D_mean := mean(L_e_u, na.rm = TRUE), by = date]
# DB_spline = DB_spline[, .(date, D_mean)] |> unique()
#
# #### create spline (smooth param set =15 at the beginning)
# DB_spline[, obs := .I]
# DB_spline[, spline_L_e_u := spline(obs,D_mean, xout = obs)$y]
# DB_spline[, smooth_line := smooth.spline(obs, spline_L_e_u,
#                                          df = dim(DB_spline)[1] / smoothig_parameter)$y]
#
# #### merge spline on hourly
# Lt_day = DB_spline[, .(smooth_line, date)][Lt_day, on = 'date']
#
# #### combine spline and weekly deviation
# Lt_day[, W_mean_adj := mean(smooth_line, na.rm = TRUE), by = .(week_n, year)]
# Lt_day[, L_e_u_adj := W_mean_adj + deviation_from_w_wmean]
#
# #### substitute spline with spot before fwd date
# Lt_day[, L_e_u_adj := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj)]
#
# Lt_day[,final_forecast:=L_e_u_adj]
#
#
# ggplot(Lt_day,
#        aes(x = date, group = 1)) +
#   labs(title = "Forecast new model",
#        y = "euro/MWh",
#        x = "date") +
#   geom_line(aes(y = smooth_line),
#             color = "darkblue", size=1, alpha=0.5)+
#   geom_line(aes(y = L_e_u_adj),
#             color = "salmon", size=1, alpha=0.5)+
#   geom_line(aes(y = L_e_u),
#             color = "goldenrod", size=1, alpha=0.5)+
#   geom_line(aes(y = spot_forward_month_BL),
#             color = "grey",  size=1, alpha=0.5) +
#   geom_line(aes(y = trade_close),
#             color = "black", size=0.5)
#
