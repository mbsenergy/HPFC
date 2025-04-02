
# SETUP -----------------------------------------------------------------------------
box::use(
    data.table[...],
    magrittr[...]
)

devtools::load_all()

smooth = 20

LST_PARAMS <- jsonlite::fromJSON("inst/backtest/params.json")


LST_DIRS = list(
    dir_data_input_t  = file.path('run', 'LAST', 'data', '01_input'),
    dir_data_input_f  = file.path('run', 'LAST', 'data', '01_input'),
    dir_data_inter    = file.path('run', 'LAST', 'data', '01_input'),
    dir_data_output   = file.path('run', 'LAST', 'data', '02_output'),
    dir_data_output_models   = file.path('run', 'LAST', 'data', '02_output'),
    dir_data_other    = file.path('run', 'LAST', 'data', 'xx_other')
)

### Directories path
LST_DIRS_archive = list(
    dir_data_input_t  = file.path('run', LST_PARAMS$sim_name, '01_input'),
    dir_data_input_f  = file.path('run', LST_PARAMS$sim_name, '01_input'),
    dir_data_inter    = file.path('run', LST_PARAMS$sim_name, '01_input'),
    dir_data_output   = file.path('run', LST_PARAMS$sim_name, '02_output'),
    dir_data_output_models   = file.path('run', LST_PARAMS$sim_name, '02_output'),
    dir_data_other    = file.path('run', LST_PARAMS$sim_name, 'xx_other')
)

lapply(LST_DIRS_archive, dir.create, recursive = TRUE, showWarnings = FALSE)


### CODES Parameters ---------------------------

ENV_CODES = list()
# sys.source(file.path("99_accessory.R"), envir = ENV_CODES)
# ENV_CODES$calendar_holidays = setnames(HPFC::calendar_holidays, paste0("holiday_", unique(HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$countries_2d )), 'holiday', skip_absent = TRUE)
ENV_CODES$calendar_holidays = setnames(HPFC::calendar_holidays, paste0("holiday_GR"), 'holiday', skip_absent = TRUE)
ENV_CODES$calendar_holidays = ENV_CODES$calendar_holidays[, .(date, holiday)]



# A. SPOT RETRIEVAL ==============================================================================================
ENV_SPOT = list()

ENV_SPOT$history_gas_full = HPFC::dt_spot_gas[RIC == HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$spot_GAS_code]
ENV_SPOT$history_gas = ENV_SPOT$history_gas_full[date <= LST_PARAMS$history_end]

ENV_SPOT$history_pwr_full = HPFC::dt_spot_pwr[RIC == HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$spot_PWR_code]
ENV_SPOT$history_pwr = ENV_SPOT$history_pwr_full[date <= LST_PARAMS$history_end]




# B. FWD DRETRIEVAL ==============================================================================================

ENV_FWD = list()

ENV_FWD$time_range = as.numeric(data.table::year(as.Date(LST_PARAMS$forecast_start))):as.numeric(data.table::year(as.Date(LST_PARAMS$forecast_end)))
ENV_FWD$calendar = HPFC::calendar_holidays
ENV_FWD$calendar[,`:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]

ENV_FWD$dt_fwds = 
    rbind(HPFC::dt_fwds_gas[substr(RIC, 1, 4) == HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$products_GAS_code],
          HPFC::dt_fwds_pwr_fwddam[spot_PWR_code == HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$spot_PWR_code, .(date, RIC, value = DAM)]
)

ENV_FWD$dt_fwds = ENV_FWD$dt_fwds[ENV_FWD$dt_fwds[, .I[date == max(date)], by = RIC]$V1]

ENV_FWD$dt_fwd_gas = HPFC::prep_fwd_curve(DT = ENV_FWD$dt_fwds,
                                          list_rics = HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$products_GAS_code,
                                          type = 'GAS',
                                          start_date = LST_PARAMS$forecast_start,
                                          end_date = LST_PARAMS$forecast_end, 
                                          calendar_sim = ENV_FWD$calendar)


ENV_FWD$dt_fwd_pwr = HPFC::prep_fwd_curve(DT = ENV_FWD$dt_fwds,
                                          list_rics = unique(HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$products_PWR_code),
                                          type = 'PWR',
                                          start_date = LST_PARAMS$forecast_start,
                                          end_date = LST_PARAMS$forecast_end, 
                                          calendar_sim = ENV_FWD$calendar)



# C. TRAINING ============================================================================================================== 

ENV_MODELS = list()

## Prepare GAS ------------------------- 

ENV_MODELS$dt_gas = ENV_SPOT$history_gas[date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, .(date, value, RIC)]
ric_gas = unique(ENV_MODELS$dt_gas$RIC)

### GAS Daily Filter and Clean
print(paste('GAS DAY Filter'))

ENV_MODELS$dt_gas = HPFC::break_detection_dd(ENV_MODELS$dt_gas)
ENV_MODELS$dt_gas_dd_filt = HPFC::filter_outlier_dd(ENV_MODELS$dt_gas)

ENV_MODELS$dt_lt_param_gasdep = copy(ENV_MODELS$dt_gas_dd_filt)
ENV_MODELS$dt_lt_param_gasdep[, RIC := NULL]

ENV_MODELS$dt_lt_param_gasdep = HPFC::detrend_dd(ENV_MODELS$dt_lt_param_gasdep, value_name = 'value')

### merge with calendar holidays for model
ENV_MODELS$dt_lt_param_gasdep = ENV_CODES$calendar_holidays[ENV_MODELS$dt_lt_param_gasdep, on = 'date']


## Prepare PWR ------------------------- 

ENV_MODELS$dt_pwr = ENV_SPOT$history_pwr[date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, .(date, hour, value, RIC)]
ric_pwr = unique(ENV_MODELS$dt_pwr$RIC)

### PWR Daily Filter and Clean
print(paste('PWR DAY Filter'))

ENV_MODELS$dt_pwr_filt_dd = ENV_MODELS$dt_pwr[, .(date, hour, value)]
ENV_MODELS$dt_pwr_filt_dd = HPFC::break_detection_ddhh(ENV_MODELS$dt_pwr_filt_dd)
ENV_MODELS$dt_pwr_filt_dd = HPFC::filter_outlier_dd_pwr(ENV_MODELS$dt_pwr_filt_dd)
ENV_MODELS$dt_pwr_filt_dd[, RIC := as.character(ric_pwr)]
ENV_MODELS$dt_lt_param_pwr = copy(ENV_MODELS$dt_pwr_filt_dd)
ENV_MODELS$dt_lt_param_pwr[, RIC := NULL]

ENV_MODELS$dt_lt_param_pwr = HPFC::detrend_dd(ENV_MODELS$dt_lt_param_pwr, value_name = 'value_day')


print(paste('PWR HOUR Filter'))

dt_pwr_filt_wbreaks  = HPFC::break_detection_ddhh(ENV_MODELS$dt_pwr)
dt_pwr_filt = HPFC::filter_outlier_dd_pwr(dt_pwr_filt_wbreaks)
ENV_MODELS$dt_pwr_filt_ddhh = dt_pwr_filt_wbreaks[dt_pwr_filt, on = "date"]
ENV_MODELS$dt_pwr_filt_ddhh = HPFC::filter_outlier_ddhh(ENV_MODELS$dt_pwr_filt_ddhh)
ENV_MODELS$dt_pwr_filt_ddhh[, RIC := as.character(ric_pwr)]
rm(dt_pwr_filt_wbreaks, dt_pwr_filt)

ENV_MODELS$dt_hr_param_pwr = copy(ENV_MODELS$dt_pwr_filt_ddhh)
ENV_MODELS$dt_hr_param_pwr[, RIC := NULL]



## Train GAS ------------------------- 
print(paste('GAS LT Regressors'))

ENV_MODELS$dt_lt_param_gasdep = HPFC::regressors_lt_model_gas(ENV_MODELS$dt_lt_param_gasdep, alpha = 0.4)

ENV_MODELS$dt_lt_param_gasdep = HPFC::train_lt_model_gas(ENV_MODELS$dt_lt_param_gasdep)
ENV_MODELS$dt_lt_param_gasdep[, RIC := as.character(ric_gas)]


## Train PWR ------------------------- 
print(paste('PWR LT Regressors'))

ENV_MODELS$dt_lt_param_pwr = regressors_lt_model_pwr(ENV_MODELS$dt_lt_param_pwr, alpha = 0.4)

### Gas Dependant 
ENV_MODELS$dt_gasdep = ENV_SPOT$history_gas[date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, .(date, value, RIC)]
ENV_MODELS$dt_gasdep = ENV_MODELS$dt_gasdep[RIC == HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$spot_GAS_code, .(date, value)] 
ENV_MODELS$dt_lt_param_pwr = ENV_MODELS$dt_gasdep[, .(date, value_gas = value)][ENV_MODELS$dt_lt_param_pwr, on = 'date'] 

calendar_holidays_pwr = copy(ENV_CODES$calendar_holidays)
ENV_MODELS$dt_lt_param_pwr = calendar_holidays_pwr[ENV_MODELS$dt_lt_param_pwr, on = 'date']         

ENV_MODELS$dt_lt_param_pwr = train_lt_model_pwr(ENV_MODELS$dt_lt_param_pwr)
ENV_MODELS$dt_lt_param_pwr[, RIC := as.character(ric_pwr)]


print(paste('PWR ST Regressors'))

dt_pwr_filt_ddhh_wreg = regressors_st_model_pwr(ENV_MODELS$dt_hr_param_pwr)
dt_pwr_filt_ddhh_wreg = ENV_MODELS$dt_gasdep[, .(date, value_gas = value)][dt_pwr_filt_ddhh_wreg, on = 'date']
ENV_MODELS$dt_hr_param_pwr = train_st_model_pwr(dt_pwr_filt_ddhh_wreg)

# lapply(dt_pwr_filt_ddhh_wreg[, .SD, .SDcols = patterns("^hour_", "^bl")], function(x) mean(x, na.rm=TRUE))
ENV_MODELS$lst_hr_param_pwr = ENV_MODELS$dt_hr_param_pwr ; rm(dt_pwr_filt_ddhh_wreg)



# D. FORECAST ===============================================================================================================

ENV_FOR = list()

LST_FOR = list(
    model_lt_gas = copy(ENV_MODELS$dt_lt_param_gasdep),
    model_lt_pwr = copy(ENV_MODELS$dt_lt_param_pwr),
    model_st_pwr = copy(ENV_MODELS$lst_hr_param_pwr),
    dt_gas_fwds = copy(ENV_FWD$dt_fwd_gas),
    dt_pwr_fwds = copy(ENV_FWD$dt_fwd_pwr),
    saved_history_gas_bis = copy(ENV_MODELS$dt_gas_dd_filt),
    saved_history_pwr = copy(ENV_MODELS$dt_pwr_filt_dd)
) 

LST_FOR$last_date = as.Date(LST_PARAMS$forecast_start) - 1


## Forecast GAS ---------------------------------------
print(paste('FORECAST GAS'))

spot_RIC = unique(HPFC::spot_GAS_products_full[products_GAS %in% c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code)]$spot_GAS_code)
fwd_RIC = unique(HPFC::spot_GAS_products_full[products_GAS %in% c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code)]$products_GAS_code)

dt_gas_fwds = LST_FOR$dt_gas_fwds[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]

saved_history_gas = copy(LST_FOR$saved_history_gas)
saved_history_gas = saved_history_gas[, RIC := NULL]

free_fwd_gas = HPFC::arbitrage_free_gas(dt_gas_fwds, DT_history = saved_history_gas, colnames(dt_gas_fwds))
dt_arbfree_fwd_gas = free_fwd_gas[, .(year, month, BL_quotes_gas, BL_gas_prev_m, RIC_s = spot_RIC, RIC_f = fwd_RIC)]

model_lt_gas = LST_FOR$model_lt_gas[RIC == spot_RIC]
model_lt_gas = model_lt_gas[, RIC := NULL]

## 3.2 CREATE CALENDAR FOR FORECAST
calendar_future = copy(ENV_CODES$calendar_holidays)
calendar_future[,`:=` (year = as.character(data.table::year(date)), 
                quarter = as.character(data.table::quarter(date)),
                month = as.character(data.table::month(date)))
]

calendar_future = calendar_future[date >= LST_PARAMS$forecast_start & date <= LST_PARAMS$forecast_end]

forecast_calendar_daily = HPFC::create_calendar_dd(calendar_future)
forecast_calendar_daily = saved_history_gas[forecast_calendar_daily, on = 'date']                 

#### merge calendar with forward
forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]        

#### spot before current date and fwd after for BL
forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= LST_FOR$last_date, value, BL_quotes_gas)]
saveRDS(forecast_calendar_daily, 'data_check.rds')
saveRDS(model_lt_gas, 'model_check.rds')
ENV_FOR$dt_gas_for_dd = HPFC::predict_lt_gas(forecast_calendar_daily, profile_matrix = model_lt_gas)

ENV_FOR$dt_gas_for_dd = HPFC::period_calibration(ENV_FOR$dt_gas_for_dd, last_date = LST_FOR$last_date)

ENV_FOR$dt_gas_for_dd[, epsilon_u := spot_forward_month_BL]
ENV_FOR$dt_gas_for_dd[, L_e_u := L_t + epsilon_u]

ENV_FOR$dt_gas_for_dd = HPFC::spline_gas(ENV_FOR$dt_gas_for_dd, smoothig_parameter = 20)
ENV_FOR$dt_gas_for_dd[, RIC := spot_RIC]

rm(forecast_calendar_daily, model_lt_gas, free_fwd_gas, dt_arbfree_fwd_gas, saved_history_gas, dt_gas_fwds, fwd_RIC, spot_RIC, calendar_future)


## Forecast PWR ---------------------------------------

print(paste('FORECAST PWR'))
#devtools::load_all()
spot_RIC = unique(HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$spot_PWR_code)
fwd_RIC =  unique(HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$products_PWR_code)

#could differ from TTF if other gas is selected as independent variable for pwr
dt_gas_fwds = LST_FOR$dt_gas_fwds[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]

dt_pwr_fwds = LST_FOR$dt_pwr_fwds
dt_pwr_fwds = dt_pwr_fwds[,.(year, quarter, month, forward_cal_BL_pwr, forward_quarter_BL_pwr, forward_month_BL_pwr, forward_cal_PL_pwr, forward_quarter_PL_pwr, forward_month_PL_pwr)]
dt_pwr_fwds = dt_pwr_fwds[, lapply(.SD, as.numeric)]

saved_history_pwr = copy(LST_FOR$saved_history_pwr)
saved_history_pwr = saved_history_pwr[,RIC := NULL]

saved_history_gas_bis = copy(LST_FOR$saved_history_gas_bis)
saved_history_gas_bis = saved_history_gas_bis[, RIC := NULL]

free_fwd_pwr = HPFC::arbitrage_free_power(dt_pwr_fwds, DT_history = saved_history_pwr, colnames(dt_pwr_fwds))
free_fwd_gas = HPFC::arbitrage_free_gas(dt_gas_fwds, DT_history = saved_history_gas_bis, colnames(dt_gas_fwds))

dt_arbfree_fwd_pwr = free_fwd_pwr[, .(year, month, BL_quotes, PL_quotes, RIC_s = spot_RIC, RIC_f = fwd_RIC)]

model_lt_pwr_long = copy(LST_FOR$model_lt_pwr) 
model_lt_pwr_long = model_lt_pwr_long[, RIC := NULL]

model_st_pwr = copy(LST_FOR$model_st_pwr)


## 3.2 CREATE CALIBRATION GAS
calibration_gas = copy(ENV_FOR$dt_gas_for_dd)
calibration_gas = calibration_gas[, .(date, value_gas = smooth_corrected)]

## 3.2 CREATE CALENDAR FOR FORECAST
calendar_future = copy(ENV_CODES$calendar_holidays)
calendar_future[,`:=` (year = as.character(data.table::year(date)),
                quarter = as.character(data.table::quarter(date)),
                month = as.character(data.table::month(date)))
]

calendar_future = calendar_future[LST_PARAMS$forecast_start <= date & date <= LST_PARAMS$forecast_end]

forecast_calendar_daily_raw = HPFC::create_calendar_dd(calendar_future)

## 3.3 LONG TERM CALIBRATION

#### merge calendar with daily spot
forecast_calendar_daily = saved_history_pwr[forecast_calendar_daily_raw, on = 'date']                 
forecast_calendar_daily = free_fwd_pwr[forecast_calendar_daily, on = c('month', 'year')]        
forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]        
forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= LST_FOR$last_date, value_day, BL_quotes)]
forecast_calendar_daily[, spot_forward_month_PL := fifelse(date <= LST_FOR$last_date | PL_quotes <= 0, as.numeric(NA), PL_quotes)]
forecast_calendar_daily = calibration_gas[forecast_calendar_daily, on = c('date')]

# devtools::load_all()
ENV_FOR$dt_pwr_for_ddhh = HPFC::predict_lt_pwr(forecast_calendar_daily, profile_matrix = model_lt_pwr_long)
ENV_FOR$dt_pwr_for_ddhh = HPFC::period_calibration(ENV_FOR$dt_pwr_for_ddhh, last_date = LST_FOR$last_date)

forecast_calendar_hourly = HPFC::create_calendar_ddhh(ENV_FOR$dt_pwr_for_ddhh)

ENV_FOR$dt_pwr_for_ddhh = HPFC::predict_st_pwr(forecast_calendar_hourly, model_h = model_st_pwr)

ENV_FOR$dt_pwr_for_ddhh = HPFC::spline_pwr(ENV_FOR$dt_pwr_for_ddhh, smoothig_parameter = smooth)
ENV_FOR$dt_pwr_for_ddhh = HPFC::PL_correction(ENV_FOR$dt_pwr_for_ddhh)
ENV_FOR$dt_pwr_for_ddhh[, RIC := spot_RIC]

rm(forecast_calendar_hourly, model_lt_pwr_long, model_st_pwr, free_fwd_gas, free_fwd_pwr, dt_arbfree_fwd_gas, saved_history_gas_bis, saved_history_pwr, dt_gas_fwds, dt_pwr_fwds, fwd_RIC, spot_RIC, calendar_future, calibration_gas)

paths = paste0(LST_PARAMS$sim_name, '.rds')
saveRDS(ENV_FOR$dt_pwr_for_ddhh, file.path('inst', 'backtest', paths))
