#' Run Power Forecast Backtest
#'
#' Executes a backtest of power price forecasts using pre-trained models and forward curves.
#'
#' @param x Character. Power market code (e.g., "Greece").
#' @param in_select_backtest_source Character. Source of the backtest. One of `'Last'` or `'Sim'`.
#' @param in_select_backtest_period_1 Character. Start date of the backtest in `'YYYY-MM-DD'` format.
#' @param in_select_backtest_period_2 Character. End date of the backtest in `'YYYY-MM-DD'` format.
#' @param shiny_sim Character or NULL. Simulation identifier used if `in_select_backtest_source = 'Sim'`.
#' @param reuters_key Character. API key required to pull forward data using `eikondata`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{`dt_pwr`}{`data.table` with forecast and observed spot values for each hour.}
#'   \item{`dt_pwr_lg`}{`data.table` in long format with forecast, spot, and other attributes.}
#' }
#'
#' @details
#' - Loads pre-trained models and historical data from either the "last" or "archive" directory structure.
#' - Uses the `prepare_fwd()`, `forecast_gas()`, and `forecast_pwr()` functions to generate the backtest.
#' - Writes forecast results to disk under the corresponding folder structure.
#'
#' @import data.table
#' @import eikondata 
#'
#' @examples
#' run_forecast_backtest_pwr(
#'   x = "Greece",
#'   in_select_backtest_source = "Last",
#'   in_select_backtest_period_1 = "2024-01-01",
#'   in_select_backtest_period_2 = "2024-12-31",
#'   shiny_sim = NULL,
#'   reuters_key = "YOUR_KEY"
#' )
#'
#' @export


run_forecast_backtest_pwr = function(
        x,
        in_select_backtest_source = 'Last',
        in_select_backtest_period_1,
        in_select_backtest_period_2,
        shiny_sim = NULL,
        reuters_key
) {
    tryCatch({
        
        list_inputs_fwd = prepare_fwd(
            fwd_pwr_code = x,
            fwd_gas_code = 'TTF',
            start_date = in_select_backtest_period_1,
            end_date = in_select_backtest_period_2,
            model_type = 'PWR',
            forecast_source = 'FWD',
            archive = 'NO',
            manual_pwr = NULL,
            manual_gas = NULL,
            reuters_key = reuters_key
        ) 
        
        pwr_codes = eikondata::pwr_products_full[countries %in% x]$spot_PWR_code
        
        FWD = list_inputs_fwd$ENV_FWD
        
        if (in_select_backtest_source == 'Last') {
            last_path_models = file.path('HPFC', 'last', 'models', x)
            last_path_history = file.path('HPFC', 'last', 'history', x)
        } else if (in_select_backtest_source == 'Sim') {
            last_path_models = file.path('HPFC', 'archive', 'models', x, shiny_sim)
            last_path_history = file.path('HPFC', 'archive', 'history', x, shiny_sim)
        }
        
        ENV_MODELS_GAS = list(
            dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
        )
        ENV_MODELS_PWR = list(
            dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds')),
            lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
        )
        list_inputs = list(
            history_gas = fread(file.path(last_path_history, 'history_gas.csv')),
            history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
        )
        
        LST_FOR = list(
            model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
            model_lt_pwr = copy(ENV_MODELS_PWR$dt_lt_param_pwr),
            model_st_pwr = copy(ENV_MODELS_PWR$lst_hr_param_pwr),
            dt_fwds = copy(FWD$dt_fwds),
            saved_history_gas = copy(list_inputs$history_gas),
            saved_history_pwr = copy(list_inputs$history_pwr),
            ric_spot_gas = eikondata::gas_products_full[products_GAS %in% 'TTF']$spot_GAS_code,
            ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% 'TTF']$products_GAS_code,
            ric_spot_pwr = eikondata::pwr_products_full[countries %in% x]$spot_PWR_code,
            ric_fwd_pwr = eikondata::pwr_products_full[countries %in% x]$products_PWR_code,
            calendar_forecast = FWD$calendar_future,
            start_date = in_select_backtest_period_1,
            end_date = in_select_backtest_period_2,
            last_date = FWD$last_date
        ) 
        
        ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
        ENV_FOR_PWR = forecast_pwr(input_forecast = LST_FOR, gas_forecast = ENV_FOR_GAS)
        
        dt_pwr_for = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast, RIC, season, peak, value_gas, value_bl = spot_forward_month_BL)]
        dt_pwr_obs = LST_FOR$saved_history_pwr[year(date) %in% unique(year(dt_pwr_for$date)) & RIC == unique(dt_pwr_for$RIC)][, .(date, hour, spot = value, RIC)]
        dt_pwr = merge(dt_pwr_for, dt_pwr_obs, by = c('date', 'hour', 'RIC'), all = TRUE)
        
        is_eikon_pwr = eikondata::pwr_mapped_codes[countries == eikondata::pwr_products_full[spot_PWR_code == pwr_codes]$countries]$eikon
        if (is_eikon_pwr == 'NO') {
            file_path = file.path(file.path('HPFC', 'last', 'history'), 'backup_spot_pwr.xlsx')
            print(file_path)
            sheet_names = openxlsx::getSheetNames(file_path)
            print(sheet_names)
            df = openxlsx::read.xlsx(file_path, sheet = pwr_codes, detectDates = TRUE)
            history_pwr_full = data.table::as.data.table(df)
        } else {
            history_pwr_full = eikondata::dt_spot_pwr[RIC == pwr_codes]
        }
        
        if(as.character(in_select_backtest_period_2) >= '2025-01-01') {
            DT_NEW = eikondata::retrieve_spot(
                ric = pwr_codes,
                from_date = '2025-01-01',
                to_date = in_select_backtest_period_2,
                type = 'PWR')
            
            history_pwr_full = 
                rbind(
                    history_pwr_full,
                    DT_NEW,
                    use.names=TRUE
                )
        }
        
        dt_pwr = merge(dt_pwr, history_pwr_full, by = c('date', 'hour', 'RIC'), all.x = TRUE)
        dt_pwr[, spot := value]
        dt_pwr[, value := NULL]
        
        setcolorder(dt_pwr, c('date', 'hour', 'season', 'peak', 'RIC', 'spot', 'forecast', 'value_bl', 'value_gas'))
        setorder(dt_pwr, date, hour)
        
        output_path = if (in_select_backtest_source == 'Last') {
            file.path('HPFC', 'last', 'output', x)
        } else {
            file.path('HPFC', 'archive', 'output', x, shiny_sim)
        }
        
        if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
        fwrite(dt_pwr, file.path(output_path, 'forecast_pwr_backtest.csv'))
        
        dt_pwr_lg = melt(copy(dt_pwr), id.vars = c('date', 'hour', 'season', 'peak', 'RIC'),
                         variable.name = 'type', value.name = 'value')
        dt_pwr_lg[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        setorder(dt_pwr_lg, datetime, RIC)
        
        return(list(dt_pwr = dt_pwr, dt_pwr_lg = dt_pwr_lg))
        
    }, error = function(e) {
        msg = paste0("Error while training ", x, ": ", e$message)
        # shiny::showNotification(msg, type = "error", duration = NULL)
        message(msg)
        return(NULL)
    })
}
