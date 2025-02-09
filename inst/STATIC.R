#
# library(openxlsx)
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
# alpha = 0.4 ### weights smoothing (ex Caldana et al 2017)
#
# fw_quote_year = 2023
#
# last_hpfc_y = 2024
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
# # LOAD DAY AHEAD DATA
#
# history = read.xlsx(file.path('..', 'HPFC','data', '1_data_raw', "input_part_1", "DAM data.xlsx"), sheet = paste0(market, "_DAM")) |> setDT()
# history = innteamUtils::clean_names(history)
# history$date = as.Date(convertToDateTime(history$date), tz = 'CET')
#
# history = history[date > first_year_spot_sample]
# setorderv(history, cols = c('date', 'hour'), order = -1L)
#
# ### Sselect rolling period
# min_date = as.Date(paste(min(data.table::year(history$date)), substr(max(history$date), 6, 10), sep = "-"))
#
# ### select sample
# history = history[date > min_date]
# history[, smp := nafill(smp, 'locf')]
#
# ### store last date
# last_date = max(history[, date])
#
# # history_day = history[,smp_day:=mean(smp, na.rm=T)]
# # history_day = unique(history_day[,.(date,smp_day)])
#
# # LOAD TTF
#
# df_ttf = read.xlsx(file.path('..','HPFC','data', '1_data_raw', 'TTF DA.xlsx')) |> setDT()
# df_ttf = innteamUtils::clean_names(df_ttf)
# df_ttf$date = as.Date(convertToDateTime(df_ttf$date), tz = 'CET')
# df_ttf=df_ttf[date>first_year_spot_sample & date<=last_date]
#
#
# # 2.3 BREAK DETECTION ---------------------------------------------------------------------------------------------------------------------
#
#
# df_dam=copy(history)
#
# min_date = as.Date(min(df_dam$date))
# setorderv(df_dam, cols = c('date'), order = 1L)
# df_dam[, smp := nafill(smp, 'locf')]
#
# #### create deviation from daily mean
#
# df_dam[,smp_day:=mean(smp), by='date']
# df_dam[,smp_h:=smp-smp_day]
#
#
# #### create time series of h deviation
#
# df_dam_ddhh_ts_detr = ts(df_dam$smp_h,
#                          start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
#                          frequency = 365*24)
#
# #### start from n of breaks = to n years, check if there is a period with less than 3 months in the last 5 years,
# #### if not stops, otherwise consider 1 break less
#
# for (j in 1:round(length(df_dam_ddhh_ts_detr)/(365*24))){
#
#   n_breaks=1+round(length(df_dam_ddhh_ts_detr)/(365*24))-j
#
#   Vvalue_h = suppressWarnings(changepoint::cpt.var(df_dam_ddhh_ts_detr, Q =n_breaks, method='BinSeg'))
#
#   cutoff=length(df_dam_ddhh_ts_detr)-((365*24)*5)
#
#   vec=c(0,changepoint::cpts(Vvalue_h)[changepoint::cpts(Vvalue_h)>cutoff],length(df_dam_ddhh_ts_detr))
#
#   difs=diff(vec)
#   if (!any(24*90>difs)){break}}
#
# #### create variable with period group
# discv_date_h=df_dam$date[changepoint::cpts(Vvalue_h)]
# #plot(Vvalue_h, main=paste('hourly', market, discv_date_h))
# df_dam[,break_group_p:=findInterval(date,discv_date_h)]
#
# kc_cols(df_dam, c('date', 'hour', 'smp', 'break_group_p'))
#
#
# # 2.4 FILTER OUTLIER ----------------------------------------------------------------------------------------------------------------------
#
# df_dam=copy(df_dam)
#
# df_dam_day = df_dam[, smp_day := mean(smp, na.rm = TRUE), by = date]
# df_dam_day = df_dam_day[, .(date, smp_day)] |> unique()
#
# ## HAMILTON FILTER
# hamilton_filter = neverhpfilter::yth_filter(xts::as.xts(df_dam_day), h = 24, p = 4, output = c("x", "trend", "cycle"))
# dt_hamilton_filter = as.data.table(hamilton_filter)
# setnames(dt_hamilton_filter, old = names(dt_hamilton_filter), new = c('date', 'x', 'trend', 'cycle'))
# dt_hamilton_filter[, x := nafill(x, type = 'nocb')]
# dt_hamilton_filter[, trend := nafill(trend, type = 'nocb')]
# dt_hamilton_filter[, cycle := nafill(cycle, type = 'nocb')]
#
# df_dam_day=unique(df_dam[,.(date,break_group_p)])[df_dam_day, on='date']
#
# ## add hp trend
#
# df_dam_day[, hp_trend := dt_hamilton_filter$trend]
#
# # find diff over 3 std dev
# df_dam_day[, delta := smp_day - (hp_trend)]
#
# df_dam_day[, sd_delta := sd(delta), by=break_group_p]
#
# df_dam_day[, out_dummy := as.numeric(abs(delta) > 3 * sd_delta)]
# df_dam_day[, out_values := fifelse(out_dummy == 1, smp_day, NA_real_)]
#
# filtered_dam_dd = df_dam_day[out_dummy == 0, .(date, smp_day, out_dummy)]
#
#
# history_1st_filter = df_dam[filtered_dam_dd, on = "date"]
#
#
# filtered1_dam_ddhh=copy(history_1st_filter)
#
# filtered1_dam_ddhh[, ddhh := paste(date, hour, sep = "H")]
#
# filtered1_dam_ddhh[,smp_h:=smp-smp_day]
# filtered1_dam_ddhh[, we:=chron::is.weekend(date)]
# filtered1_dam_ddhh[, mean_h:=mean(smp_h), by=c('break_group_p', 'hour', 'we')]
#
# filtered1_dam_ddhh[, delta_h := smp_h - (mean_h)]
# filtered1_dam_ddhh[, sd_delta_h := sd(delta_h), by=c('break_group_p','hour', 'we')]
#
# filtered1_dam_ddhh[, out_dummy := as.numeric(abs(delta_h) > 3 * sd_delta_h)]
# filtered1_dam_ddhh[, out_values := fifelse(out_dummy == 1, smp_h, NA_real_)]
#
# filtered1_dam_ddhh[, out_perday := sum(out_dummy), by=date]
# filtered1_dam_ddhh[, out_dummy := fifelse(out_perday>4,1,out_dummy)]
#
# filtered_dam_ddhh = filtered1_dam_ddhh[out_dummy == 0, .(date, hour, ddhh, smp, break_group_p)]
#
#
# # 2.5 DETREND DAILY ----------------------------------------------------------------------------------------------------------------------
#
# min_date = as.Date(min(filtered_dam_dd$date))
#
# df_dam_dd_ts = ts(filtered_dam_dd$smp_day,
#                   start = c(format(min_date, '%Y'), format(min_date, '%m'), format(min_date, '%d')),
#                   frequency = 365)
#
#
# #add HP trend to dataframe daily
# hp_filter2 = mFilter::hpfilter(df_dam_dd_ts,
#                                freq = 8.322 * 10 ^ 7,  ### 8.322 * 10 ^ 7
#                                type = "lambda",
#                                drift = F) ### lambda as in Caldana et al (2017)
#
# filtered_dam_dd[, hp_trend := hp_filter2$trend]
#
# # subtract hp_trend from raw data
# filtered_dam_dd[, detr_smp_day := smp_day - hp_trend]
#
# kc_cols(filtered_dam_dd, c('date', 'smp_day', 'hp_trend', 'detr_smp_day'))
#
# # 2.6 CREATE LONG TERM REGRESSORS --------------------------------------------------------------------------------------------------------
#
# alpha=0.4
#
# filtered_dam_dd[,`:=` (yday = data.table::yday(date),
#                        wday = data.table::wday(date),
#                        quarter = data.table::quarter(date),
#                        month = data.table::month(date),
#                        weekend = as.numeric(chron::is.weekend(date)),
#                        obs = .I)]
#
# #create weight with smoothing
# filtered_dam_dd[, time_distance := (max(obs) + 1 - obs) / max(obs)] #TO CHECK / max(obs) su 365*3
# filtered_dam_dd[, weight := exp(-alpha * (time_distance))]
#
# # create day dummies (intra-week seasonality)
# filtered_dam_dd[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
#
# #create summer dummy
# filtered_dam_dd[, summer := as.numeric(quarter == 2 | quarter == 3)]
#
# #create variable beginning season date AND delta date-beginning
# filtered_dam_dd[, begining_season := as.Date(
#   fifelse(quarter == 4, paste0(format(date, format = "%Y"), "-10-01"),
#           fifelse(quarter == 1, paste0(data.table::year(date) - 1, "-10-01"),
#                   paste0(format(date, format = "%Y"), "-04-01"))))]
#
# filtered_dam_dd[, dist := as.numeric(date - begining_season)]
# filtered_dam_dd[, yday_season := dist / max(dist)]
#
# #create variable season.year
# filtered_dam_dd[, season := fifelse(summer == 1, paste0("summer-", data.table::year(date)),
#                                     fifelse(quarter == 4, paste0("winter-", data.table::year(date)),
#                                             paste0("winter-", data.table::year(date) - 1)))]
#
#
# #### create LT components Caldana 6.2
# filtered_dam_dd[,`:=`(cos_long_term = cos((2 * pi) * yday / 365),
#                       cos_season = cos((2 * pi) * yday_season),
#                       cos_season_summer = cos((2 * pi) * yday_season) * summer,
#                       sin_long_term = sin((2 * pi) * yday / 365),
#                       sin_season = sin((2 * pi) * yday_season),
#                       sin_season_summer = sin((2 * pi) * yday_season) * summer)]
#
# ### generate polynomial via lapply
# filtered_dam_dd[, (paste("yday", 1:10, sep = "_")) := lapply(1:10, function(i) {yday^i})]
#
#
# # 2.7 ESTIMATE LONG TERM MODEL -----------------------------------------------------------------------------------------------------------
#
# #### merge with gas
# filtered_dam_dd=df_ttf[filtered_dam_dd, on='date']                    # merge with df_ttf for variable gas
# filtered_dam_dd[, trade_close := nafill(trade_close, 'locf')]
# filtered_dam_dd[, trade_close := nafill(trade_close, 'nocb')]
# filtered_dam_dd[,trade_close2:=trade_close^2]
#
# filtered_dam_dd=calendar_holidays[filtered_dam_dd, on='date']         # merge with calendar for variable holiday
#
# df_reg=copy(filtered_dam_dd)
#
# df_reg[,trade_close2:=trade_close^2]
#
# regression ="detr_smp_day~cos_long_term+cos_season+cos_season_summer+holiday+day_2+day_3+day_4+day_5+day_6+day_7+sin_long_term+sin_season+sin_season_summer+summer"
#
# for (i in 1:10 ){
#   regression=paste(regression,"+",paste("yday", i, sep = "_"))
# }
#
# regression=paste(regression,"+",'trade_close',"+",'trade_close2')
# model_1=eval(substitute(step(lm(regression,weights = weight, data = df_reg),direction="both")))
#
#
# #### create model_1 coeff vector
# terms=c('cos_long_term', 'cos_season', 'cos_season_summer', 'holiday', 'sin_long_term', 'sin_season', 'sin_season_summer', 'summer', 'day_1', 'day_2', 'day_3', 'day_4', 'day_5', 'day_6', 'day_7', 'yday_1','yday_2','yday_3','yday_4','yday_5','yday_6','yday_7','yday_8','yday_9','yday_10')
# missing_terms=terms[!(terms %in% names(model_1$coefficients))]
# profile_matrix=as.data.table(matrix(c(model_1$coefficients,rep(0,length(missing_terms))),nrow=1))
# setnames(profile_matrix,,c(names(model_1$coefficients),missing_terms))
#
# # 2.8 CREATE HOURLY REGRESSORS -----------------------------------------------------------------------------------------------------------
#
# #create dummy for monthly, quarterly, weekend seasonality
# filtered_dam_ddhh[,`:=` (yday = data.table::yday(date),
#                          wday = data.table::wday(date),
#                          quarter = data.table::quarter(date),
#                          month = data.table::month(date),
#                          weekend = as.numeric(chron::is.weekend(date)),
#                          obs = .I)]
#
# ####create dummy for weekday
# filtered_dam_ddhh[, (paste("day", 1:7, sep = "_")) := lapply(1:7, function(i) {fifelse(wday == i, 1, 0)})]
#
# ####create dummy for hours
# filtered_dam_ddhh[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
#
# #### create baseload
# filtered_dam_ddhh[, bl := mean(smp), by=date]
#
# #### create last market regime period
# filtered_dam_ddhh[,break_h:=fifelse(break_group_p==max(break_group_p),1,0)]
#
# filtered_dam_ddhh[,smp_h:=smp-bl]
#
# filtered_dam_ddhh[,yday2:=yday^2]
# filtered_dam_ddhh[,yday3:=yday^3]
# filtered_dam_ddhh[,bl2:=bl^2]
# filtered_dam_ddhh[,bl3:=bl^3]
#
# filtered_dam_ddhh=df_ttf[filtered_dam_ddhh, on='date']
# filtered_dam_ddhh[, trade_close := nafill(trade_close, 'locf')]
# filtered_dam_ddhh[, trade_close := nafill(trade_close, 'nocb')]
#
#
# # 2.9 ESTIMATE HOURLY MODEL -------------------------------------------------------------------------------------------------------------
#
# filtered_dam_ddhh[,trade_close2:=trade_close^2]
#
# model_h=nls(smp_h ~
#               hour_1*coeff_hour_1+
#               hour_2*coeff_hour_2+
#               hour_3*coeff_hour_3+
#               hour_4*coeff_hour_4+
#               hour_5*coeff_hour_5+
#               hour_6*coeff_hour_6+
#               hour_7*coeff_hour_7+
#               hour_8*coeff_hour_8+
#               hour_9*coeff_hour_9+
#               hour_10*coeff_hour_10+
#               hour_11*coeff_hour_11+
#               hour_12*coeff_hour_12+
#               hour_13*coeff_hour_13+
#               hour_14*coeff_hour_14+
#               hour_15*coeff_hour_15+
#               hour_16*coeff_hour_16+
#               hour_17*coeff_hour_17+
#               hour_18*coeff_hour_18+
#               hour_19*coeff_hour_19+
#               hour_20*coeff_hour_20+
#               hour_21*coeff_hour_21+
#               hour_22*coeff_hour_22+
#               hour_23*coeff_hour_23+
#               (-1*(coeff_hour_1+coeff_hour_2+
#                      coeff_hour_3+coeff_hour_4+
#                      coeff_hour_5+coeff_hour_6+
#                      coeff_hour_7+coeff_hour_8+
#                      coeff_hour_9+coeff_hour_10+
#                      coeff_hour_11+coeff_hour_12+
#                      coeff_hour_13+coeff_hour_14+
#                      coeff_hour_15+coeff_hour_16+
#                      coeff_hour_17+coeff_hour_18+
#                      coeff_hour_19+coeff_hour_20+
#                      coeff_hour_21+coeff_hour_22+
#                      coeff_hour_23))*hour_24+ # end hour_ dummy --------------
#             hour_1*bl*coeff_bl_hour_1+
#               hour_2*bl*coeff_bl_hour_2+
#               hour_3*bl*coeff_bl_hour_3+
#               hour_4*bl*coeff_bl_hour_4+
#               hour_5*bl*coeff_bl_hour_5+
#               hour_6*bl*coeff_bl_hour_6+
#               hour_7*bl*coeff_bl_hour_7+
#               hour_8*bl*coeff_bl_hour_8+
#               hour_9*bl*coeff_bl_hour_9+
#               hour_10*bl*coeff_bl_hour_10+
#               hour_11*bl*coeff_bl_hour_11+
#               hour_12*bl*coeff_bl_hour_12+
#               hour_13*bl*coeff_bl_hour_13+
#               hour_14*bl*coeff_bl_hour_14+
#               hour_15*bl*coeff_bl_hour_15+
#               hour_16*bl*coeff_bl_hour_16+
#               hour_17*bl*coeff_bl_hour_17+
#               hour_18*bl*coeff_bl_hour_18+
#               hour_19*bl*coeff_bl_hour_19+
#               hour_20*bl*coeff_bl_hour_20+
#               hour_21*bl*coeff_bl_hour_21+
#               hour_22*bl*coeff_bl_hour_22+
#               hour_23*bl*coeff_bl_hour_23+
#               (-1*(coeff_bl_hour_1+coeff_bl_hour_2+
#                      coeff_bl_hour_3+coeff_bl_hour_4+
#                      coeff_bl_hour_5+coeff_bl_hour_6+
#                      coeff_bl_hour_7+coeff_bl_hour_8+
#                      coeff_bl_hour_9+coeff_bl_hour_10+
#                      coeff_bl_hour_11+coeff_bl_hour_12+
#                      coeff_bl_hour_13+coeff_bl_hour_14+
#                      coeff_bl_hour_15+coeff_bl_hour_16+
#                      coeff_bl_hour_17+coeff_bl_hour_18+
#                      coeff_bl_hour_19+coeff_bl_hour_20+
#                      coeff_bl_hour_21+coeff_bl_hour_22+
#                      coeff_bl_hour_23))*hour_24*bl + # end hour_ bl interaction --------------
#             hour_1*bl2*coeff_bl2_hour_1+
#               hour_2*bl2*coeff_bl2_hour_2+
#               hour_3*bl2*coeff_bl2_hour_3+
#               hour_4*bl2*coeff_bl2_hour_4+
#               hour_5*bl2*coeff_bl2_hour_5+
#               hour_6*bl2*coeff_bl2_hour_6+
#               hour_7*bl2*coeff_bl2_hour_7+
#               hour_8*bl2*coeff_bl2_hour_8+
#               hour_9*bl2*coeff_bl2_hour_9+
#               hour_10*bl2*coeff_bl2_hour_10+
#               hour_11*bl2*coeff_bl2_hour_11+
#               hour_12*bl2*coeff_bl2_hour_12+
#               hour_13*bl2*coeff_bl2_hour_13+
#               hour_14*bl2*coeff_bl2_hour_14+
#               hour_15*bl2*coeff_bl2_hour_15+
#               hour_16*bl2*coeff_bl2_hour_16+
#               hour_17*bl2*coeff_bl2_hour_17+
#               hour_18*bl2*coeff_bl2_hour_18+
#               hour_19*bl2*coeff_bl2_hour_19+
#               hour_20*bl2*coeff_bl2_hour_20+
#               hour_21*bl2*coeff_bl2_hour_21+
#               hour_22*bl2*coeff_bl2_hour_22+
#               hour_23*bl2*coeff_bl2_hour_23+
#               (-1*(coeff_bl2_hour_1+coeff_bl2_hour_2+
#                      coeff_bl2_hour_3+coeff_bl2_hour_4+
#                      coeff_bl2_hour_5+coeff_bl2_hour_6+
#                      coeff_bl2_hour_7+coeff_bl2_hour_8+
#                      coeff_bl2_hour_9+coeff_bl2_hour_10+
#                      coeff_bl2_hour_11+coeff_bl2_hour_12+
#                      coeff_bl2_hour_13+coeff_bl2_hour_14+
#                      coeff_bl2_hour_15+coeff_bl2_hour_16+
#                      coeff_bl2_hour_17+coeff_bl2_hour_18+
#                      coeff_bl2_hour_19+coeff_bl2_hour_20+
#                      coeff_bl2_hour_21+coeff_bl2_hour_22+
#                      coeff_bl2_hour_23))*hour_24*bl2 + # end hour_ bl2 interaction --------------
#             hour_1*bl3*coeff_bl3_hour_1+
#               hour_2*bl3*coeff_bl3_hour_2+
#               hour_3*bl3*coeff_bl3_hour_3+
#               hour_4*bl3*coeff_bl3_hour_4+
#               hour_5*bl3*coeff_bl3_hour_5+
#               hour_6*bl3*coeff_bl3_hour_6+
#               hour_7*bl3*coeff_bl3_hour_7+
#               hour_8*bl3*coeff_bl3_hour_8+
#               hour_9*bl3*coeff_bl3_hour_9+
#               hour_10*bl3*coeff_bl3_hour_10+
#               hour_11*bl3*coeff_bl3_hour_11+
#               hour_12*bl3*coeff_bl3_hour_12+
#               hour_13*bl3*coeff_bl3_hour_13+
#               hour_14*bl3*coeff_bl3_hour_14+
#               hour_15*bl3*coeff_bl3_hour_15+
#               hour_16*bl3*coeff_bl3_hour_16+
#               hour_17*bl3*coeff_bl3_hour_17+
#               hour_18*bl3*coeff_bl3_hour_18+
#               hour_19*bl3*coeff_bl3_hour_19+
#               hour_20*bl3*coeff_bl3_hour_20+
#               hour_21*bl3*coeff_bl3_hour_21+
#               hour_22*bl3*coeff_bl3_hour_22+
#               hour_23*bl3*coeff_bl3_hour_23+
#               (-1*(coeff_bl3_hour_1+coeff_bl3_hour_2+
#                      coeff_bl3_hour_3+coeff_bl3_hour_4+
#                      coeff_bl3_hour_5+coeff_bl3_hour_6+
#                      coeff_bl3_hour_7+coeff_bl3_hour_8+
#                      coeff_bl3_hour_9+coeff_bl3_hour_10+
#                      coeff_bl3_hour_11+coeff_bl3_hour_12+
#                      coeff_bl3_hour_13+coeff_bl3_hour_14+
#                      coeff_bl3_hour_15+coeff_bl3_hour_16+
#                      coeff_bl3_hour_17+coeff_bl3_hour_18+
#                      coeff_bl3_hour_19+coeff_bl3_hour_20+
#                      coeff_bl3_hour_21+coeff_bl3_hour_22+
#                      coeff_bl3_hour_23))*hour_24*bl3 + # end hour_ bl3 interaction --------------
#             hour_1*yday*coeff_yday_hour_1+
#               hour_2*yday*coeff_yday_hour_2+
#               hour_3*yday*coeff_yday_hour_3+
#               hour_4*yday*coeff_yday_hour_4+
#               hour_5*yday*coeff_yday_hour_5+
#               hour_6*yday*coeff_yday_hour_6+
#               hour_7*yday*coeff_yday_hour_7+
#               hour_8*yday*coeff_yday_hour_8+
#               hour_9*yday*coeff_yday_hour_9+
#               hour_10*yday*coeff_yday_hour_10+
#               hour_11*yday*coeff_yday_hour_11+
#               hour_12*yday*coeff_yday_hour_12+
#               hour_13*yday*coeff_yday_hour_13+
#               hour_14*yday*coeff_yday_hour_14+
#               hour_15*yday*coeff_yday_hour_15+
#               hour_16*yday*coeff_yday_hour_16+
#               hour_17*yday*coeff_yday_hour_17+
#               hour_18*yday*coeff_yday_hour_18+
#               hour_19*yday*coeff_yday_hour_19+
#               hour_20*yday*coeff_yday_hour_20+
#               hour_21*yday*coeff_yday_hour_21+
#               hour_22*yday*coeff_yday_hour_22+
#               hour_23*yday*coeff_yday_hour_23+
#               (-1*(coeff_yday_hour_1+coeff_yday_hour_2+
#                      coeff_yday_hour_3+coeff_yday_hour_4+
#                      coeff_yday_hour_5+coeff_yday_hour_6+
#                      coeff_yday_hour_7+coeff_yday_hour_8+
#                      coeff_yday_hour_9+coeff_yday_hour_10+
#                      coeff_yday_hour_11+coeff_yday_hour_12+
#                      coeff_yday_hour_13+coeff_yday_hour_14+
#                      coeff_yday_hour_15+coeff_yday_hour_16+
#                      coeff_yday_hour_17+coeff_yday_hour_18+
#                      coeff_yday_hour_19+coeff_yday_hour_20+
#                      coeff_yday_hour_21+coeff_yday_hour_22+
#                      coeff_yday_hour_23))*hour_24*yday + # end hour_ yday interaction --------------
#             hour_1*yday2*coeff_yday2_hour_1+
#               hour_2*yday2*coeff_yday2_hour_2+
#               hour_3*yday2*coeff_yday2_hour_3+
#               hour_4*yday2*coeff_yday2_hour_4+
#               hour_5*yday2*coeff_yday2_hour_5+
#               hour_6*yday2*coeff_yday2_hour_6+
#               hour_7*yday2*coeff_yday2_hour_7+
#               hour_8*yday2*coeff_yday2_hour_8+
#               hour_9*yday2*coeff_yday2_hour_9+
#               hour_10*yday2*coeff_yday2_hour_10+
#               hour_11*yday2*coeff_yday2_hour_11+
#               hour_12*yday2*coeff_yday2_hour_12+
#               hour_13*yday2*coeff_yday2_hour_13+
#               hour_14*yday2*coeff_yday2_hour_14+
#               hour_15*yday2*coeff_yday2_hour_15+
#               hour_16*yday2*coeff_yday2_hour_16+
#               hour_17*yday2*coeff_yday2_hour_17+
#               hour_18*yday2*coeff_yday2_hour_18+
#               hour_19*yday2*coeff_yday2_hour_19+
#               hour_20*yday2*coeff_yday2_hour_20+
#               hour_21*yday2*coeff_yday2_hour_21+
#               hour_22*yday2*coeff_yday2_hour_22+
#               hour_23*yday2*coeff_yday2_hour_23+
#               (-1*(coeff_yday2_hour_1+coeff_yday2_hour_2+
#                      coeff_yday2_hour_3+coeff_yday2_hour_4+
#                      coeff_yday2_hour_5+coeff_yday2_hour_6+
#                      coeff_yday2_hour_7+coeff_yday2_hour_8+
#                      coeff_yday2_hour_9+coeff_yday2_hour_10+
#                      coeff_yday2_hour_11+coeff_yday2_hour_12+
#                      coeff_yday2_hour_13+coeff_yday2_hour_14+
#                      coeff_yday2_hour_15+coeff_yday2_hour_16+
#                      coeff_yday2_hour_17+coeff_yday2_hour_18+
#                      coeff_yday2_hour_19+coeff_yday2_hour_20+
#                      coeff_yday2_hour_21+coeff_yday2_hour_22+
#                      coeff_yday2_hour_23))*hour_24*yday2 + # end hour_ yday2 interaction --------------
#             hour_1*yday3*coeff_yday3_hour_1+
#               hour_2*yday3*coeff_yday3_hour_2+
#               hour_3*yday3*coeff_yday3_hour_3+
#               hour_4*yday3*coeff_yday3_hour_4+
#               hour_5*yday3*coeff_yday3_hour_5+
#               hour_6*yday3*coeff_yday3_hour_6+
#               hour_7*yday3*coeff_yday3_hour_7+
#               hour_8*yday3*coeff_yday3_hour_8+
#               hour_9*yday3*coeff_yday3_hour_9+
#               hour_10*yday3*coeff_yday3_hour_10+
#               hour_11*yday3*coeff_yday3_hour_11+
#               hour_12*yday3*coeff_yday3_hour_12+
#               hour_13*yday3*coeff_yday3_hour_13+
#               hour_14*yday3*coeff_yday3_hour_14+
#               hour_15*yday3*coeff_yday3_hour_15+
#               hour_16*yday3*coeff_yday3_hour_16+
#               hour_17*yday3*coeff_yday3_hour_17+
#               hour_18*yday3*coeff_yday3_hour_18+
#               hour_19*yday3*coeff_yday3_hour_19+
#               hour_20*yday3*coeff_yday3_hour_20+
#               hour_21*yday3*coeff_yday3_hour_21+
#               hour_22*yday3*coeff_yday3_hour_22+
#               hour_23*yday3*coeff_yday3_hour_23+
#               (-1*(coeff_yday3_hour_1+coeff_yday3_hour_2+
#                      coeff_yday3_hour_3+coeff_yday3_hour_4+
#                      coeff_yday3_hour_5+coeff_yday3_hour_6+
#                      coeff_yday3_hour_7+coeff_yday3_hour_8+
#                      coeff_yday3_hour_9+coeff_yday3_hour_10+
#                      coeff_yday3_hour_11+coeff_yday3_hour_12+
#                      coeff_yday3_hour_13+coeff_yday3_hour_14+
#                      coeff_yday3_hour_15+coeff_yday3_hour_16+
#                      coeff_yday3_hour_17+coeff_yday3_hour_18+
#                      coeff_yday3_hour_19+coeff_yday3_hour_20+
#                      coeff_yday3_hour_21+coeff_yday3_hour_22+
#                      coeff_yday3_hour_23))*hour_24*yday3 + # end hour_ yday3 interaction --------------
#             hour_1*trade_close*coeff_trade_close_hour_1+
#               hour_2*trade_close*coeff_trade_close_hour_2+
#               hour_3*trade_close*coeff_trade_close_hour_3+
#               hour_4*trade_close*coeff_trade_close_hour_4+
#               hour_5*trade_close*coeff_trade_close_hour_5+
#               hour_6*trade_close*coeff_trade_close_hour_6+
#               hour_7*trade_close*coeff_trade_close_hour_7+
#               hour_8*trade_close*coeff_trade_close_hour_8+
#               hour_9*trade_close*coeff_trade_close_hour_9+
#               hour_10*trade_close*coeff_trade_close_hour_10+
#               hour_11*trade_close*coeff_trade_close_hour_11+
#               hour_12*trade_close*coeff_trade_close_hour_12+
#               hour_13*trade_close*coeff_trade_close_hour_13+
#               hour_14*trade_close*coeff_trade_close_hour_14+
#               hour_15*trade_close*coeff_trade_close_hour_15+
#               hour_16*trade_close*coeff_trade_close_hour_16+
#               hour_17*trade_close*coeff_trade_close_hour_17+
#               hour_18*trade_close*coeff_trade_close_hour_18+
#               hour_19*trade_close*coeff_trade_close_hour_19+
#               hour_20*trade_close*coeff_trade_close_hour_20+
#               hour_21*trade_close*coeff_trade_close_hour_21+
#               hour_22*trade_close*coeff_trade_close_hour_22+
#               hour_23*trade_close*coeff_trade_close_hour_23+
#               (-1*(coeff_trade_close_hour_1+coeff_trade_close_hour_2+
#                      coeff_trade_close_hour_3+coeff_trade_close_hour_4+
#                      coeff_trade_close_hour_5+coeff_trade_close_hour_6+
#                      coeff_trade_close_hour_7+coeff_trade_close_hour_8+
#                      coeff_trade_close_hour_9+coeff_trade_close_hour_10+
#                      coeff_trade_close_hour_11+coeff_trade_close_hour_12+
#                      coeff_trade_close_hour_13+coeff_trade_close_hour_14+
#                      coeff_trade_close_hour_15+coeff_trade_close_hour_16+
#                      coeff_trade_close_hour_17+coeff_trade_close_hour_18+
#                      coeff_trade_close_hour_19+coeff_trade_close_hour_20+
#                      coeff_trade_close_hour_21+coeff_trade_close_hour_22+
#                      coeff_trade_close_hour_23))*hour_24*trade_close + # end hour_ trade_close interaction --------------
#             hour_1*trade_close2*coeff_trade_close2_hour_1+
#               hour_2*trade_close2*coeff_trade_close2_hour_2+
#               hour_3*trade_close2*coeff_trade_close2_hour_3+
#               hour_4*trade_close2*coeff_trade_close2_hour_4+
#               hour_5*trade_close2*coeff_trade_close2_hour_5+
#               hour_6*trade_close2*coeff_trade_close2_hour_6+
#               hour_7*trade_close2*coeff_trade_close2_hour_7+
#               hour_8*trade_close2*coeff_trade_close2_hour_8+
#               hour_9*trade_close2*coeff_trade_close2_hour_9+
#               hour_10*trade_close2*coeff_trade_close2_hour_10+
#               hour_11*trade_close2*coeff_trade_close2_hour_11+
#               hour_12*trade_close2*coeff_trade_close2_hour_12+
#               hour_13*trade_close2*coeff_trade_close2_hour_13+
#               hour_14*trade_close2*coeff_trade_close2_hour_14+
#               hour_15*trade_close2*coeff_trade_close2_hour_15+
#               hour_16*trade_close2*coeff_trade_close2_hour_16+
#               hour_17*trade_close2*coeff_trade_close2_hour_17+
#               hour_18*trade_close2*coeff_trade_close2_hour_18+
#               hour_19*trade_close2*coeff_trade_close2_hour_19+
#               hour_20*trade_close2*coeff_trade_close2_hour_20+
#               hour_21*trade_close2*coeff_trade_close2_hour_21+
#               hour_22*trade_close2*coeff_trade_close2_hour_22+
#               hour_23*trade_close2*coeff_trade_close2_hour_23+
#               (-1*(coeff_trade_close2_hour_1+coeff_trade_close2_hour_2+
#                      coeff_trade_close2_hour_3+coeff_trade_close2_hour_4+
#                      coeff_trade_close2_hour_5+coeff_trade_close2_hour_6+
#                      coeff_trade_close2_hour_7+coeff_trade_close2_hour_8+
#                      coeff_trade_close2_hour_9+coeff_trade_close2_hour_10+
#                      coeff_trade_close2_hour_11+coeff_trade_close2_hour_12+
#                      coeff_trade_close2_hour_13+coeff_trade_close2_hour_14+
#                      coeff_trade_close2_hour_15+coeff_trade_close2_hour_16+
#                      coeff_trade_close2_hour_17+coeff_trade_close2_hour_18+
#                      coeff_trade_close2_hour_19+coeff_trade_close2_hour_20+
#                      coeff_trade_close2_hour_21+coeff_trade_close2_hour_22+
#                      coeff_trade_close2_hour_23))*hour_24*trade_close2 + # end hour_ trade_close2 interaction --------------
#             hour_1*break_h*coeff_break_h_hour_1+
#               hour_2*break_h*coeff_break_h_hour_2+
#               hour_3*break_h*coeff_break_h_hour_3+
#               hour_4*break_h*coeff_break_h_hour_4+
#               hour_5*break_h*coeff_break_h_hour_5+
#               hour_6*break_h*coeff_break_h_hour_6+
#               hour_7*break_h*coeff_break_h_hour_7+
#               hour_8*break_h*coeff_break_h_hour_8+
#               hour_9*break_h*coeff_break_h_hour_9+
#               hour_10*break_h*coeff_break_h_hour_10+
#               hour_11*break_h*coeff_break_h_hour_11+
#               hour_12*break_h*coeff_break_h_hour_12+
#               hour_13*break_h*coeff_break_h_hour_13+
#               hour_14*break_h*coeff_break_h_hour_14+
#               hour_15*break_h*coeff_break_h_hour_15+
#               hour_16*break_h*coeff_break_h_hour_16+
#               hour_17*break_h*coeff_break_h_hour_17+
#               hour_18*break_h*coeff_break_h_hour_18+
#               hour_19*break_h*coeff_break_h_hour_19+
#               hour_20*break_h*coeff_break_h_hour_20+
#               hour_21*break_h*coeff_break_h_hour_21+
#               hour_22*break_h*coeff_break_h_hour_22+
#               hour_23*break_h*coeff_break_h_hour_23+
#               (-1*(coeff_break_h_hour_1+coeff_break_h_hour_2+
#                      coeff_break_h_hour_3+coeff_break_h_hour_4+
#                      coeff_break_h_hour_5+coeff_break_h_hour_6+
#                      coeff_break_h_hour_7+coeff_break_h_hour_8+
#                      coeff_break_h_hour_9+coeff_break_h_hour_10+
#                      coeff_break_h_hour_11+coeff_break_h_hour_12+
#                      coeff_break_h_hour_13+coeff_break_h_hour_14+
#                      coeff_break_h_hour_15+coeff_break_h_hour_16+
#                      coeff_break_h_hour_17+coeff_break_h_hour_18+
#                      coeff_break_h_hour_19+coeff_break_h_hour_20+
#                      coeff_break_h_hour_21+coeff_break_h_hour_22+
#                      coeff_break_h_hour_23))*hour_24*break_h, # end hour_ break_group_h interaction --------------
#             start=list(coeff_hour_1=0,coeff_hour_2=0,coeff_hour_3=0,coeff_hour_4=0,coeff_hour_5=0,coeff_hour_6=0,coeff_hour_7=0,coeff_hour_8=0,coeff_hour_9=0,coeff_hour_10=0,coeff_hour_11=0,coeff_hour_12=0,coeff_hour_13=0,
#                        coeff_hour_14=0,coeff_hour_15=0,coeff_hour_16=0,coeff_hour_17=0,coeff_hour_18=0,coeff_hour_19=0,coeff_hour_20=0,coeff_hour_21=0,coeff_hour_22=0,coeff_hour_23=0,
#                        coeff_bl_hour_1=0,coeff_bl_hour_2=0,coeff_bl_hour_3=0,coeff_bl_hour_4=0,coeff_bl_hour_5=0,coeff_bl_hour_6=0,coeff_bl_hour_7=0,coeff_bl_hour_8=0,coeff_bl_hour_9=0,coeff_bl_hour_10=0,coeff_bl_hour_11=0,coeff_bl_hour_12=0,coeff_bl_hour_13=0,
#                        coeff_bl_hour_14=0,coeff_bl_hour_15=0,coeff_bl_hour_16=0,coeff_bl_hour_17=0,coeff_bl_hour_18=0,coeff_bl_hour_19=0,coeff_bl_hour_20=0,coeff_bl_hour_21=0,coeff_bl_hour_22=0,coeff_bl_hour_23=0,
#                        coeff_bl2_hour_1=0,coeff_bl2_hour_2=0,coeff_bl2_hour_3=0,coeff_bl2_hour_4=0,coeff_bl2_hour_5=0,coeff_bl2_hour_6=0,coeff_bl2_hour_7=0,coeff_bl2_hour_8=0,coeff_bl2_hour_9=0,coeff_bl2_hour_10=0,coeff_bl2_hour_11=0,coeff_bl2_hour_12=0,coeff_bl2_hour_13=0,
#                        coeff_bl2_hour_14=0,coeff_bl2_hour_15=0,coeff_bl2_hour_16=0,coeff_bl2_hour_17=0,coeff_bl2_hour_18=0,coeff_bl2_hour_19=0,coeff_bl2_hour_20=0,coeff_bl2_hour_21=0,coeff_bl2_hour_22=0,coeff_bl2_hour_23=0,
#                        coeff_bl3_hour_1=0,coeff_bl3_hour_2=0,coeff_bl3_hour_3=0,coeff_bl3_hour_4=0,coeff_bl3_hour_5=0,coeff_bl3_hour_6=0,coeff_bl3_hour_7=0,coeff_bl3_hour_8=0,coeff_bl3_hour_9=0,coeff_bl3_hour_10=0,coeff_bl3_hour_11=0,coeff_bl3_hour_12=0,coeff_bl3_hour_13=0,
#                        coeff_bl3_hour_14=0,coeff_bl3_hour_15=0,coeff_bl3_hour_16=0,coeff_bl3_hour_17=0,coeff_bl3_hour_18=0,coeff_bl3_hour_19=0,coeff_bl3_hour_20=0,coeff_bl3_hour_21=0,coeff_bl3_hour_22=0,coeff_bl3_hour_23=0,
#                        coeff_yday_hour_1=0,coeff_yday_hour_2=0,coeff_yday_hour_3=0,coeff_yday_hour_4=0,coeff_yday_hour_5=0,coeff_yday_hour_6=0,coeff_yday_hour_7=0,coeff_yday_hour_8=0,coeff_yday_hour_9=0,coeff_yday_hour_10=0,coeff_yday_hour_11=0,coeff_yday_hour_12=0,coeff_yday_hour_13=0,
#                        coeff_yday_hour_14=0,coeff_yday_hour_15=0,coeff_yday_hour_16=0,coeff_yday_hour_17=0,coeff_yday_hour_18=0,coeff_yday_hour_19=0,coeff_yday_hour_20=0,coeff_yday_hour_21=0,coeff_yday_hour_22=0,coeff_yday_hour_23=0,
#                        coeff_yday2_hour_1=0,coeff_yday2_hour_2=0,coeff_yday2_hour_3=0,coeff_yday2_hour_4=0,coeff_yday2_hour_5=0,coeff_yday2_hour_6=0,coeff_yday2_hour_7=0,coeff_yday2_hour_8=0,coeff_yday2_hour_9=0,coeff_yday2_hour_10=0,coeff_yday2_hour_11=0,coeff_yday2_hour_12=0,coeff_yday2_hour_13=0,
#                        coeff_yday2_hour_14=0,coeff_yday2_hour_15=0,coeff_yday2_hour_16=0,coeff_yday2_hour_17=0,coeff_yday2_hour_18=0,coeff_yday2_hour_19=0,coeff_yday2_hour_20=0,coeff_yday2_hour_21=0,coeff_yday2_hour_22=0,coeff_yday2_hour_23=0,
#                        coeff_yday3_hour_1=0,coeff_yday3_hour_2=0,coeff_yday3_hour_3=0,coeff_yday3_hour_4=0,coeff_yday3_hour_5=0,coeff_yday3_hour_6=0,coeff_yday3_hour_7=0,coeff_yday3_hour_8=0,coeff_yday3_hour_9=0,coeff_yday3_hour_10=0,coeff_yday3_hour_11=0,coeff_yday3_hour_12=0,coeff_yday3_hour_13=0,
#                        coeff_yday3_hour_14=0,coeff_yday3_hour_15=0,coeff_yday3_hour_16=0,coeff_yday3_hour_17=0,coeff_yday3_hour_18=0,coeff_yday3_hour_19=0,coeff_yday3_hour_20=0,coeff_yday3_hour_21=0,coeff_yday3_hour_22=0,coeff_yday3_hour_23=0,
#                        coeff_trade_close_hour_1=0,coeff_trade_close_hour_2=0,coeff_trade_close_hour_3=0,coeff_trade_close_hour_4=0,coeff_trade_close_hour_5=0,coeff_trade_close_hour_6=0,coeff_trade_close_hour_7=0,coeff_trade_close_hour_8=0,coeff_trade_close_hour_9=0,coeff_trade_close_hour_10=0,coeff_trade_close_hour_11=0,coeff_trade_close_hour_12=0,coeff_trade_close_hour_13=0,
#                        coeff_trade_close_hour_14=0,coeff_trade_close_hour_15=0,coeff_trade_close_hour_16=0,coeff_trade_close_hour_17=0,coeff_trade_close_hour_18=0,coeff_trade_close_hour_19=0,coeff_trade_close_hour_20=0,coeff_trade_close_hour_21=0,coeff_trade_close_hour_22=0,coeff_trade_close_hour_23=0,
#                        coeff_trade_close2_hour_1=0,coeff_trade_close2_hour_2=0,coeff_trade_close2_hour_3=0,coeff_trade_close2_hour_4=0,coeff_trade_close2_hour_5=0,coeff_trade_close2_hour_6=0,coeff_trade_close2_hour_7=0,coeff_trade_close2_hour_8=0,coeff_trade_close2_hour_9=0,coeff_trade_close2_hour_10=0,coeff_trade_close2_hour_11=0,coeff_trade_close2_hour_12=0,coeff_trade_close2_hour_13=0,
#                        coeff_trade_close2_hour_14=0,coeff_trade_close2_hour_15=0,coeff_trade_close2_hour_16=0,coeff_trade_close2_hour_17=0,coeff_trade_close2_hour_18=0,coeff_trade_close2_hour_19=0,coeff_trade_close2_hour_20=0,coeff_trade_close2_hour_21=0,coeff_trade_close2_hour_22=0,coeff_trade_close2_hour_23=0,
#                        coeff_break_h_hour_1=0,coeff_break_h_hour_2=0,coeff_break_h_hour_3=0,coeff_break_h_hour_4=0,coeff_break_h_hour_5=0,coeff_break_h_hour_6=0,coeff_break_h_hour_7=0,coeff_break_h_hour_8=0,coeff_break_h_hour_9=0,coeff_break_h_hour_10=0,coeff_break_h_hour_11=0,coeff_break_h_hour_12=0,coeff_break_h_hour_13=0,
#                        coeff_break_h_hour_14=0,coeff_break_h_hour_15=0,coeff_break_h_hour_16=0,coeff_break_h_hour_17=0,coeff_break_h_hour_18=0,coeff_break_h_hour_19=0,coeff_break_h_hour_20=0,coeff_break_h_hour_21=0,coeff_break_h_hour_22=0,coeff_break_h_hour_23=0),
#             data=filtered_dam_ddhh)
#
#
#
#
# ### 3
#
#
#
# # 3.1 LOAD FORWARD ---------------------------------------------------------------------------------------------------------------------
#
# # quotes_power_BL=import_power_BL('PRICES DATABASE_update.xlsm', 'GR', 'Console.xlsx')
# # quotes_power_PL=import_power_PL('PRICES DATABASE_update.xlsm', 'GR', 'Console.xlsx')
# # quotes_gas_BL=import_gas_BL('PRICES DATABASE_update.xlsm',calendar_holidays, 2021, 2024)
#
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
#
# # LOAD TTF FORWARD
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
# setcolorder(forward_quotes1, c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL', 'forward_cal_PL', 'forward_quarter_PL', 'forward_month_PL'))
# names_vec=colnames(forward_quotes1)
#
# smp_prev_year=copy(filtered_dam_dd)
#
# smp_prev_year[,':='(month=as.numeric(data.table::month(date)), quarter=as.numeric(data.table::quarter(date)), year=as.numeric(data.table::year(date)))]
# smp_prev_year=smp_prev_year[, .(smp_day,month,quarter)]
# smp_prev_year[, sum_bym:=sum(smp_day), by=month]
# smp_prev_year[, sum_byq:=sum(smp_day), by=quarter]
# smp_prev_year[, m_over_q:=sum_bym/sum_byq]
# share_prev_year=unique(smp_prev_year[,.(month,m_over_q)])
#
#
# forward_quotes1 = forward_quotes1[, lapply(.SD, as.numeric)]
#
# forward_quotes1 = share_prev_year[forward_quotes1, on = 'month']
# forward_quotes1[, BL_prev_m := m_over_q*sum(forward_month_BL, na.rm=TRUE)/sum(m_over_q*!is.na(forward_month_BL), na.rm=TRUE), by=c('year', 'quarter')]
#
# #### create arbitrage free calendar
# forward_quotes1[, BL_empty_m := (sum(forward_quarter_BL, na.rm = TRUE) - sum(forward_month_BL, na.rm = TRUE)) / sum(is.na(forward_month_BL)), by = list(quarter, year)]
# forward_quotes1[, BL_empty_q := (sum(forward_cal_BL, na.rm = TRUE) - sum(forward_quarter_BL, na.rm = TRUE)) / sum(is.na(forward_quarter_BL)), by = year]
#
# forward_quotes1[, BL_quotes := fcase(
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
#   is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
#   !is.na(forward_month_BL), forward_month_BL)
# ]
#
#
# forward_quotes1[, PL_empty_m := (sum(forward_quarter_PL, na.rm = TRUE) - sum(forward_month_PL, na.rm = TRUE)) / sum(is.na(forward_month_PL)), by = list(quarter, year)]
# forward_quotes1[, PL_empty_q := (sum(forward_cal_PL, na.rm = TRUE) - sum(forward_quarter_PL, na.rm = TRUE)) / sum(is.na(forward_quarter_PL)), by = year]
# forward_quotes1[, PL_empty_m := fifelse(PL_empty_m == 0, PL_empty_q, PL_empty_m)]
#
# forward_quotes1[, PL_quotes := fifelse(!is.na(forward_month_PL), forward_month_PL, PL_empty_m)]
#
# ##
#
# setcolorder(forward_quotes_TTF, c('year','quarter','month', 'forward_cal_BL', 'forward_quarter_BL', 'forward_month_BL'))
# names_vec2=colnames(forward_quotes_TTF)
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
# forward_quotes_TTF[, trade_close := fcase(
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & !is.nan(BL_prev_m), BL_prev_m,
#   is.na(forward_month_BL) & is.na(forward_quarter_BL) & is.nan(BL_prev_m), BL_empty_q,
#   is.na(forward_month_BL) & !is.na(forward_quarter_BL), BL_empty_m,
#   !is.na(forward_month_BL), forward_month_BL)
# ]
#
# kc_cols(forward_quotes_TTF, c('month', 'year', 'trade_close' ))
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
# free_fwd_power=copy(forward_quotes1)
# free_fwd_gas=copy(forward_quotes_TTF)
# forecast_calendar_daily=copy(calendar)
#
# history_day = history[,smp_day:=mean(smp, na.rm=T)]
# history_day = unique(history_day[,.(date,smp_day)])
#
# #### merge calendar with daily spot
# forecast_calendar_daily = history_day[forecast_calendar_daily, on = 'date']                 # merge
#
# #### merge calendar with forward
# forecast_calendar_daily = free_fwd_power[forecast_calendar_daily, on = c('month', 'year')]        # merge
#
# #### merge calendar with forward
# forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]        # merge
#
# last_date=as.Date(max(history$date))
# #### spot before current date and fwd after for BL
# forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= last_date, smp_day, BL_quotes)]
# #### NA before current date and fwd after for PL
# forecast_calendar_daily[, spot_forward_month_PL := fifelse(date <= last_date | PL_quotes <= 0, as.numeric(NA), PL_quotes)]
#
# Lt_day=copy(forecast_calendar_daily)
# Lt_day[,trade_close2:=trade_close^2]
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
#
#                   trade_close * profile_matrix$trade_close[1] +
#                   trade_close2 * profile_matrix$trade_close2[1] +
#
#                   # generate day-type deviations :
#
#                   holiday * profile_matrix$holiday[1] +
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
# Lt_day_adjusted=refeHPFC::period_adjusting(Lt_day,last_date)
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
# # 3.6 CREATE CALENDAR HOURLY --------------------------------------------------------------------------------------------------------------
#
# ### generate hourly calendar
# calendar_hourly = do.call("rbind", replicate(24, Lt_day, simplify = FALSE))[order(date)]
# calendar_hourly[, hour := rep(rep(1:24), nrow(calendar))]
#
# #### create weekend/holiday
# calendar_hourly[, weekend_h := fifelse(holiday == 1, 1, weekend)]
# calendar_hourly[, quarter := paste('Q', quarter, sep = '')]
#
# #### create ddhh for plot
# calendar_hourly[, ddhh := paste(date, hour, sep = "H")]
#
# calendar_hourly=calendar_hourly[order(date,hour)]
#
# kc_cols(calendar_hourly, c('date', 'hour', 'ddhh', 'period', 'weekend', 'quarter', 'holiday', 'month', 'year', 'yday', 'L_t', 'spot_forward_month_BL', 'spot_forward_month_PL', 'trade_close', 'trade_close2', 'forward_month_BL', 'forward_quarter_BL', 'BL_prev_m', 'history_forecast'))
#
#
# # 3.8 HOURLY CALIBRATION ------------------------------------------------------------------------------------------------------------------
#
# Lt_lu_hh=refeHPFC::H_calibration(forecast_calendar_hourly,hourly_param)
#
# Lt_Lu_hour=copy(calendar_hourly)
#
# #### explicit epsilon u as in Caldana
# Lt_Lu_hour[, epsilon_u := spot_forward_month_BL]
#
# Lt_Lu_hour[, break_h := 1]
# Lt_Lu_hour[, (paste("hour", 1:24, sep = "_")) := lapply(1:24, function(i) {fifelse(hour == i, 1, 0)})]
# Lt_Lu_hour[, yday2 := yday^2]
# Lt_Lu_hour[, yday3 := yday^3]
# Lt_Lu_hour[, trade_close2 := trade_close^2]
#
# Lt_Lu_hour[,bl:=epsilon_u+L_t]
# Lt_Lu_hour[, bl2 := bl^2]
# Lt_Lu_hour[, bl3 := bl^3]
#
# pred_hourly=predict(model_h, Lt_Lu_hour)
# Lt_Lu_hour[, prediction_h := pred_hourly]
# Lt_Lu_hour[, L_u := pred_hourly + L_t]
# Lt_Lu_hour[, L_e_u := pred_hourly + L_t + epsilon_u]
#
# # 3.9 APPLY SPLINE ------------------------------------------------------------------------------------------------------------------------
#
# Lt_lu_hh_spline=refeHPFC::apply_spline(Lt_lu_hh,15)
# smoothig_parameter=15
#
# #### create L_e_u deviation from week mean
# Lt_Lu_hour[, week_n := lubridate::week(date)]
# Lt_Lu_hour[, deviation_from_w_wmean := L_e_u - mean(L_e_u, na.rm = TRUE), by = .(week_n, year)]
#
# #### create daily DB with L_e_u mean to find daily spline
# DB_spline = copy(Lt_Lu_hour)
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
# Lt_Lu_hour = DB_spline[, .(smooth_line, date)][Lt_Lu_hour, on = 'date']
#
# #### combine spline and weekly deviation
# Lt_Lu_hour[, W_mean_adj := mean(smooth_line, na.rm = TRUE), by = .(week_n, year)]
# Lt_Lu_hour[, L_e_u_adj := W_mean_adj + deviation_from_w_wmean]
#
# #### substitute spline with spot before fwd date
# Lt_Lu_hour[, L_e_u_adj := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj)]
#
# # 3.10 APPLY PL CORRECTION ----------------------------------------------------------------------------------------------------------------
#
# #### explicit epsilon as in Caldana
# Lt_Lu_hour[, epsilon_u := spot_forward_month_BL]
#
# #### correct period before last date availbale (history)
# Lt_Lu_hour[, period := fifelse(history_forecast == 0, as.character(date), period)]
#
# #### peak hour during day, offpeak night and we
# Lt_Lu_hour[, peak := as.numeric(hour >= 9 & hour <= 20 & weekend == 0)]
#
# #### intermediate varibale
# Lt_Lu_hour[, tot_hours := .N, by = period]
#
# Lt_Lu_hour[,`:=`(sum_L_e_u_peak = sum(L_e_u_adj * peak),
#                  sum_L_e_u_off = sum(L_e_u_adj * (1 - peak)),
#                  number_peak_hrs = sum(peak)), by = period]
#
# Lt_Lu_hour[, number_off_peak := tot_hours - number_peak_hrs, by = period]
#
# #### PL correction
# Lt_Lu_hour[, final_forward_month_PL := fifelse(is.na(spot_forward_month_PL), sum_L_e_u_peak / number_peak_hrs, spot_forward_month_PL)]
# Lt_Lu_hour[, ci_term_on_peak := (number_peak_hrs * final_forward_month_PL - sum_L_e_u_peak) / (number_peak_hrs)]
# Lt_Lu_hour[, ci_term_off_peak := ((tot_hours * epsilon_u - number_peak_hrs * final_forward_month_PL) - sum_L_e_u_off) / number_off_peak]
#
#
# Lt_Lu_hour[, phi_u := fifelse(peak == 1, ci_term_on_peak, ci_term_off_peak)]
#
# #### forecast vs history
# Lt_Lu_hour[, f_u := fifelse(history_forecast == 0, spot_forward_month_BL, L_e_u_adj + phi_u)]
# Lt_Lu_hour[, L_u := fifelse(history_forecast == 0, 0, L_u)]
# Lt_Lu_hour[, epsilon_u := fifelse(history_forecast == 0, 0, epsilon_u)]
# Lt_Lu_hour[, phi_u := fifelse(history_forecast == 0, 0, phi_u)]
#
#
# #### daily terms
# Lt_Lu_hour[,`:=`(daily_L_e_u = mean(L_e_u),
#                  daily_L_e_u_adj = mean(L_e_u_adj),
#                  daily_f_u = mean(f_u)), by = date]
#
# Lt_Lu_hour[, PL_corr := fifelse(phi_u != 0, phi_u + spot_forward_month_BL, as.numeric(NA))]
#
# Lt_Lu_hour[,final_forecast:=f_u]
#
#
#
#
