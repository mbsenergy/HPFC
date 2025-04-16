    
commodity_main = 'Greece'
coef_glm = readRDS('inst/app_demo/coeff_table.rds')
list_data = readRDS('inst/app_demo/list_data.rds')
start_train = Sys.Date() - 365
end_train = Sys.Date() 
start_horizon = Sys.Date() 
end_horizon = Sys.Date() + 365

commodity_basket = unique(coef_glm$COMMODITY)
    # saveRDS(dt_co2_expanded, 'dt_co2_expanded.rds')
    
## Get History ------------------------------------------------------

list_history = list_data$list_history
list_fwds = list_data$list_fwds

## PREPARE ARB FREE CURVES
history_main = list_history$history_main
history_ttf = list_history$history_ttf
history_basket_pwr = list_history$history_basket_pwr
history_basket_gas = list_history$history_basket_gas

main_inputs_fwd = list_fwds$main_inputs_fwd
main_inputs_fwd_gas = list_fwds$main_inputs_fwd_gas
basketpwr_inputs_fwd = list_fwds$basketpwr_inputs_fwd
basketgas_inputs_fwd = list_fwds$basketgas_inputs_fwd
dt_co2_expanded = list_fwds$dt_co2_expanded
    
    
    ## PREPARE ARB FREE CURVES
    fwd_calendar = eikondata::calendar_holidays
    fwd_calendar[, `:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]
    
    # PREPARE ARB FREE FWD CURVE - MAIN
    # gas for apply_shape
    dt_fwd_gas = eikondata::prep_fwd_curve(DT = main_inputs_fwd_gas,
                                           list_rics = eikondata::gas_products_full[products_GAS %in% commodity_basket]$products_GAS_code,
                                           type = 'GAS',
                                           start_date = start_horizon,
                                           end_date = end_horizon, 
                                           calendar_sim = fwd_calendar)
    dt_gas_fwds = dt_fwd_gas[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
    dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]
    
    
    free_fwd_gas = HPFC::arbitrage_free_gas(dt_gas_fwds, DT_history = history_ttf, colnames(dt_gas_fwds))
    dt_gas_freearb_fwds_main = free_fwd_gas[, .(sim = 'FWD', yymm = as.Date(paste(year, month, '01', sep = '-')), value = BL_quotes_gas)]
    
    # MAIN
    dt_fwd_pwr = eikondata::prep_fwd_curve(DT = main_inputs_fwd,
                                           list_rics = eikondata::pwr_products_full[countries %in% commodity_main]$products_PWR_code,
                                           type = 'PWR',
                                           start_date = start_horizon,
                                           end_date = end_horizon, 
                                           calendar_sim = fwd_calendar)
    
    
    dt_pwr_fwds = dt_fwd_pwr[, .(year, quarter, month, forward_cal_BL_pwr, forward_quarter_BL_pwr, 
                                 forward_month_BL_pwr, forward_cal_PL_pwr, forward_quarter_PL_pwr, forward_month_PL_pwr)]
    dt_pwr_fwds = dt_pwr_fwds[, lapply(.SD, as.numeric)]
    
    history_main_fa = history_main[, .(date, hour, value, RIC)]
    history_main_fa = history_main_fa[, .(date, hour, value)]
    history_main_fa = HPFC::break_detection_ddhh(history_main_fa)
    history_main_fa = HPFC::filter_outlier_dd_pwr(history_main_fa)
    free_fwd_pwr = HPFC::arbitrage_free_power(dt_pwr_fwds, DT_history = history_main_fa, colnames(dt_pwr_fwds))
    dt_pwr_freearb_fwds_main = free_fwd_pwr[, .(sim = 'FWD', yymm = as.Date(paste(year, month, '01', sep = '-')), value = BL_quotes)]
    
    
    ## PREPARE ARB FREE GAS 
    
    # PREPARE ARB FREE FWD CURVE
    # gas for apply_shape
    vec_gas = eikondata::gas_products_full[products_GAS %in% commodity_basket]$products_GAS_code
    list_dt_gas_freearb_fwds = lapply(vec_gas, function(gas_code, history, start_date, end_date, calendar_sim) {
        dt_gas_basket = basketgas_inputs_fwd[substr(RIC, 1, 3) %in% substr(gas_code, 1, 3)]
        
        dt_fwd_gas = eikondata::prep_fwd_curve(
            DT = dt_gas_basket,
            list_rics = gas_code,
            type = 'GAS',
            start_date = start_date,
            end_date = end_date, 
            calendar_sim = calendar_sim
        )
        
        dt_gas_fwds = dt_fwd_gas[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
        dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]
        
        history = history[RIC %in% eikondata::gas_products_full[products_GAS %in% gas_code]$spot_GAS_code]
        
        free_fwd_gas = HPFC::arbitrage_free_gas(
            dt_gas_fwds,
            DT_history = history,
            colnames(dt_gas_fwds)
        )
        
        dt_gas_freearb_fwds = free_fwd_gas[, .(products_GAS_code = gas_code, yymm = as.Date(paste(year, month, '01', sep = '-')), value = BL_quotes_gas)]
        
        return(dt_gas_freearb_fwds)
    },  history = history_basket_gas,
    start_date = start_horizon,
    end_date = end_horizon, 
    calendar_sim = fwd_calendar)
    
    dt_gas_freearb_fwds = rbindlist(list_dt_gas_freearb_fwds)
    
    
    
    ## PREPARE ARB FREE PWR 
    
    # PREPARE ARB FREE FWD CURVE
    # gas for apply_shape
    vec_pwr = eikondata::pwr_products_full[countries %in% commodity_basket]$products_PWR_code
    list_dt_pwr_freearb_fwds = lapply(vec_pwr, function(pwr_code, history, start_date, end_date, calendar_sim) {
        dt_pwr_basket = basketpwr_inputs_fwd[substr(RIC, 1, 2) %in% pwr_code]
        dt_pwr_basket = dt_pwr_basket[date >= start_horizon]
        dt_fwd_pwr = eikondata::prep_fwd_curve(
            DT = dt_pwr_basket,
            list_rics = pwr_code,
            type = 'PWR',
            start_date = start_date,
            end_date = end_date, 
            calendar_sim = calendar_sim
        )
        
        dt_pwr_fwds = dt_fwd_pwr[, .(year, quarter, month, forward_cal_BL_pwr, forward_quarter_BL_pwr, 
                                     forward_month_BL_pwr, forward_cal_PL_pwr, forward_quarter_PL_pwr, forward_month_PL_pwr)]
        dt_pwr_fwds = dt_pwr_fwds[, lapply(.SD, as.numeric)]
        
        history = history[RIC %in% eikondata::pwr_products_full[products_PWR_code %in% pwr_code]$spot_PWR_code]
        
        history_main_fa = history[, .(date, hour, value, RIC)]
        history_main_fa = history_main_fa[, .(date, hour, value)]
        history_main_fa = HPFC::break_detection_ddhh(history_main_fa)
        history_main_fa = HPFC::filter_outlier_dd_pwr(history_main_fa)
        free_fwd_pwr = HPFC::arbitrage_free_power(dt_pwr_fwds, DT_history = history_main_fa, colnames(dt_pwr_fwds))
        dt_pwr_freearb_fwds_main = free_fwd_pwr[, .(products_PWR_code = pwr_code, yymm = as.Date(paste(year, month, '01', sep = '-')), value = BL_quotes)]
        
        return(dt_pwr_freearb_fwds_main)
    },  history = history_basket_pwr,
    start_date = start_horizon,
    end_date = end_horizon, 
    calendar_sim = fwd_calendar
    )
    
    dt_pwr_freearb_fwds = rbindlist(list_dt_pwr_freearb_fwds)
    
    
    ## PREPARE FWD BASKET SYNTH
    dt_pwr_freearb_fwds = merge(eikondata::pwr_products_full[countries %in% commodity_basket], dt_pwr_freearb_fwds, by = 'products_PWR_code')
    dt_gas_freearb_fwds = merge(eikondata::gas_products_full[products_GAS %in% commodity_basket], dt_gas_freearb_fwds, by = 'products_GAS_code')
    
    dt_pwr_freearb_fwds = dt_pwr_freearb_fwds[, .(RIC = countries, yymm, value)]
    dt_gas_freearb_fwds = dt_gas_freearb_fwds[, .(RIC = products_GAS, yymm, value)]
    
    if(any(commodity_basket %in% 'C02')) {
        dt_basket = rbindlist(list(dt_pwr_freearb_fwds, dt_gas_freearb_fwds, dt_co2_expanded), use.names = TRUE)
    } else {
        dt_basket = rbindlist(list(dt_pwr_freearb_fwds, dt_gas_freearb_fwds))
        
    }
    
    dt_basket[, COMMODITY := RIC]
    dt_basket[, RIC := NULL]
    coef_glm[, RIC := NULL]
    dt_basket = merge(coef_glm, dt_basket, by = 'COMMODITY')
    dt_basket[, value_new := coeff * value]
    
    dt_basket = dt_basket[, .(sim = 'FWD', value = sum(value_new, na.rm = TRUE)), by = 'yymm']
    dt_basket = dt_basket[, .(sim, yymm, value)]
    
    
    ## APPLY SHAPE
    ## LOAD MODELS 
    ENV_MODELS_GAS = list()
    ENV_MODELS_PWR = list()
    
    last_path = file.path('inst', 'app_demo','HPFC', 'last', 'models', commodity_main)
    # last_path = file.path('HPFC', 'last', 'models', commodity_main)
    ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path, paste0('model_gas_lt.rds')))
    ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path, paste0('model_pwr_lt.rds')))
    ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path, paste0('model_pwr_st.rds')))
    
    
    # outputs - DT_FWD_main, DT_FWD_basket
    # APPLY SHAPE:
    DT_FWD_main = apply_shape(
        country = commodity_main,
        name = 'FWD',
        start_date = start_horizon,
        end_date = end_horizon,
        dt_fwd_pwr = dt_pwr_freearb_fwds_main,
        dt_spot_pwr = history_main,
        dt_fwd_gas = dt_gas_freearb_fwds_main,
        dt_spot_gas = history_ttf,
        model_gas = ENV_MODELS_GAS,
        model_pwr = ENV_MODELS_PWR
    )
    
    DT_FWD_basket = apply_shape(
        country = commodity_main,
        name = 'FWD',
        start_date = start_horizon,
        end_date = end_horizon,
        dt_fwd_pwr = dt_basket,
        dt_spot_pwr = history_main,
        dt_fwd_gas = dt_gas_freearb_fwds_main,
        dt_spot_gas = history_ttf,
        model_gas = ENV_MODELS_GAS,
        model_pwr = ENV_MODELS_PWR
    )
    
    output = list(DT_FWD_main, DT_FWD_basket)
    names(output) =  c('MAIN', 'PROXY')
    saveRDS(output, 'output.rds')
    