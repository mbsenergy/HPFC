#' Load Input Data and Parameters for HPFC Model
#'
#' This function loads simulation parameters from a JSON file and retrieves relevant historical 
#' and forward market data for power and gas prices. It also initializes directories for storing 
#' data and returns an environment with relevant variables.
#'
#' @param params_path Character. Path to the JSON file containing simulation parameters. 
#'        Default is `'params.json'`.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{LST_PARAMS} - List of parameters loaded from the JSON file.
#'   \item \code{LST_DIRS} - List of directories for storing data.
#'   \item \code{LST_DIRS_archive} - List of directories specific to the simulation name.
#'   \item \code{ENV_CODES} - List containing calendar holiday information.
#'   \item \code{ENV_SPOT} - List with historical gas and power spot prices.
#'   \item \code{ENV_FWD} - List containing forward market data and calendar information.
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.table year quarter month setnames
#' 
#' @export
load_inputs = function(params_path = 'params.json') {
    
    LST_PARAMS <- jsonlite::fromJSON(file.path(params_path))
    
    LST_DIRS = list(
        dir_data_input_t  = file.path('run', 'LAST', 'data', '01_input'),
        dir_data_input_f  = file.path('run', 'LAST', 'data', '01_input'),
        dir_data_inter    = file.path('run', 'LAST', 'data', '01_input'),
        dir_data_output   = file.path('run', 'LAST', 'data', '02_output'),
        dir_data_output_models   = file.path('run', 'LAST', 'data', '02_output'),
        dir_data_other    = file.path('run', 'LAST', 'data', 'xx_other')
    )
    
    LST_DIRS_archive = list(
        dir_data_input_t  = file.path('run', LST_PARAMS$sim_name, '01_input'),
        dir_data_input_f  = file.path('run', LST_PARAMS$sim_name, '01_input'),
        dir_data_inter    = file.path('run', LST_PARAMS$sim_name, '01_input'),
        dir_data_output   = file.path('run', LST_PARAMS$sim_name, '02_output'),
        dir_data_output_models   = file.path('run', LST_PARAMS$sim_name, '02_output'),
        dir_data_other    = file.path('run', LST_PARAMS$sim_name, 'xx_other')
    )
    
    lapply(LST_DIRS_archive, dir.create, recursive = TRUE, showWarnings = FALSE)
    
    ### CODES Parameters
    ENV_CODES = list()
    ENV_CODES$calendar_holidays = setnames(HPFC::calendar_holidays, paste0("holiday_GR"), 'holiday', skip_absent = TRUE)
    ENV_CODES$calendar_holidays = ENV_CODES$calendar_holidays[, .(date, holiday)]
    
    # A. Spot Market Data
    ENV_SPOT = list()
    ENV_SPOT$history_gas_full = HPFC::dt_spot_gas[RIC == HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$spot_GAS_code]
    ENV_SPOT$history_gas = ENV_SPOT$history_gas_full[date <= LST_PARAMS$history_end]
    
    ENV_SPOT$history_pwr_full = HPFC::dt_spot_pwr[RIC == HPFC::spot_PWR_products_full[countries %in% LST_PARAMS$selected_pwr_code]$spot_PWR_code]
    ENV_SPOT$history_pwr = ENV_SPOT$history_pwr_full[date <= LST_PARAMS$history_end]
    
    # B. Forward Market Data
    ENV_FWD = list()
    ENV_FWD$time_range = as.numeric(data.table::year(as.Date(LST_PARAMS$forecast_start))):as.numeric(data.table::year(as.Date(LST_PARAMS$forecast_end)))
    ENV_FWD$calendar = HPFC::calendar_holidays
    ENV_FWD$calendar[, `:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]
    
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
    
    return_list = list(LST_PARAMS, LST_DIRS, LST_DIRS_archive, ENV_CODES, ENV_SPOT, ENV_FWD)
    names(return_list) = c('LST_PARAMS', 'LST_DIRS', 'LST_DIRS_archive', 'ENV_CODES', 'ENV_SPOT', 'ENV_FWD')
    return(return_list)
}


#' Prepare Gas Data for Modeling
#'
#' This function processes gas price data for use in a regime-switching VAR-GARCH model. 
#' It filters historical gas prices, detects breaks, removes outliers, and applies detrending.
#' The processed data is merged with a holiday calendar for modeling.
#'
#' @param list_inputs A list containing required datasets and parameters:
#'   \itemize{
#'     \item `LST_PARAMS`: A list of parameters, including `history_start` and `history_end` defining the time range.
#'     \item `ENV_SPOT`: An environment containing `history_gas`, a data.table with gas price history.
#'     \item `ENV_CODES`: An environment containing `calendar_holidays`, a data.table of holidays.
#'   }
#'
#' @return A list (`ENV_MODELS_GAS`) with the following elements:
#'   \item{dt_gas}{Filtered gas price data (date, value, RIC).}
#'   \item{dt_gas_dd_filt}{Gas data after break detection and outlier removal.}
#'   \item{dt_lt_param_gasdep}{Detrended gas data, merged with holiday calendar.}
#'
#' @import data.table
#' @import HPFC
#' @export
prepare_gas = function(list_inputs = list_inputs) {
    
    ## Prepare GAS ------------------------- 
    ENV_MODELS_GAS = list()
    
    ## Load inputs 
    LST_PARAMS = list_inputs$LST_PARAMS
    ENV_SPOT = list_inputs$ENV_SPOT
    ENV_CODES = list_inputs$ENV_CODES
    
    ENV_MODELS_GAS$dt_gas = ENV_SPOT$history_gas[
        date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, 
        .(date, value, RIC)
    ]
    
    ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
    
    ### GAS Daily Filter and Clean
    print(paste('GAS DAY Filter'))
    
    ENV_MODELS_GAS$dt_gas = HPFC::break_detection_dd(ENV_MODELS_GAS$dt_gas)
    ENV_MODELS_GAS$dt_gas_dd_filt = HPFC::filter_outlier_dd(ENV_MODELS_GAS$dt_gas)
    
    ENV_MODELS_GAS$dt_lt_param_gasdep = copy(ENV_MODELS_GAS$dt_gas_dd_filt)
    ENV_MODELS_GAS$dt_lt_param_gasdep[, RIC := NULL]
    
    ENV_MODELS_GAS$dt_lt_param_gasdep = HPFC::detrend_dd(
        ENV_MODELS_GAS$dt_lt_param_gasdep, 
        value_name = 'value'
    )
    
    ### Merge with calendar holidays for model
    ENV_MODELS_GAS$dt_lt_param_gasdep = ENV_CODES$calendar_holidays[
        ENV_MODELS_GAS$dt_lt_param_gasdep, on = 'date'
    ]
    
    return(ENV_MODELS_GAS)
}



#' Prepare Power Data for Modeling
#'
#' This function processes power price data for use in a regime-switching VAR-GARCH model. 
#' It filters historical power prices, detects breaks, removes outliers, and applies detrending.
#'
#' @param list_inputs A list containing required datasets and parameters:
#'   \itemize{
#'     \item `LST_PARAMS`: A list of parameters, including `history_start` and `history_end` defining the time range.
#'     \item `ENV_SPOT`: An environment containing `history_pwr`, a data.table with power price history.
#'     \item `ENV_CODES`: An environment (not explicitly used in this function but kept for consistency).
#'   }
#'
#' @return A list (`ENV_MODELS_PWR`) with the following elements:
#'   \item{dt_pwr}{Filtered power price data (date, hour, value, RIC).}
#'   \item{dt_pwr_filt_dd}{Power data after break detection and outlier removal (daily level).}
#'   \item{dt_lt_param_pwr}{Detrended daily power data.}
#'   \item{dt_pwr_filt_ddhh}{Power data after break detection and outlier removal (hourly level).}
#'   \item{dt_hr_param_pwr}{Processed hourly power data without RIC.}
#'
#' @import data.table
#' @import HPFC
#' @export
prepare_pwr = function(list_inputs = list_inputs) {
    
    ## Prepare PWR ------------------------- 
    ENV_MODELS_PWR = list()
    
    ## Load inputs
    LST_PARAMS = list_inputs$LST_PARAMS
    ENV_SPOT = list_inputs$ENV_SPOT
    ENV_CODES = list_inputs$ENV_CODES
    
    ENV_MODELS_PWR$dt_pwr = ENV_SPOT$history_pwr[
        date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, 
        .(date, hour, value, RIC)
    ]
    
    ric_pwr = unique(ENV_MODELS_PWR$dt_pwr$RIC)
    
    ### PWR Daily Filter and Clean
    print(paste('PWR DAY Filter'))
    
    ENV_MODELS_PWR$dt_pwr_filt_dd = ENV_MODELS_PWR$dt_pwr[, .(date, hour, value)]
    ENV_MODELS_PWR$dt_pwr_filt_dd = HPFC::break_detection_ddhh(ENV_MODELS_PWR$dt_pwr_filt_dd)
    ENV_MODELS_PWR$dt_pwr_filt_dd = HPFC::filter_outlier_dd_pwr(ENV_MODELS_PWR$dt_pwr_filt_dd)
    ENV_MODELS_PWR$dt_pwr_filt_dd[, RIC := as.character(ric_pwr)]
    
    ENV_MODELS_PWR$dt_lt_param_pwr = copy(ENV_MODELS_PWR$dt_pwr_filt_dd)
    ENV_MODELS_PWR$dt_lt_param_pwr[, RIC := NULL]
    
    ENV_MODELS_PWR$dt_lt_param_pwr = HPFC::detrend_dd(
        ENV_MODELS_PWR$dt_lt_param_pwr, 
        value_name = 'value_day'
    )
    
    print(paste('PWR HOUR Filter'))
    
    dt_pwr_filt_wbreaks = HPFC::break_detection_ddhh(ENV_MODELS_PWR$dt_pwr)
    dt_pwr_filt = HPFC::filter_outlier_dd_pwr(dt_pwr_filt_wbreaks)
    
    ENV_MODELS_PWR$dt_pwr_filt_ddhh = dt_pwr_filt_wbreaks[dt_pwr_filt, on = "date"]
    ENV_MODELS_PWR$dt_pwr_filt_ddhh = HPFC::filter_outlier_ddhh(ENV_MODELS_PWR$dt_pwr_filt_ddhh)
    ENV_MODELS_PWR$dt_pwr_filt_ddhh[, RIC := as.character(ric_pwr)]
    
    rm(dt_pwr_filt_wbreaks, dt_pwr_filt)
    
    ENV_MODELS_PWR$dt_hr_param_pwr = copy(ENV_MODELS_PWR$dt_pwr_filt_ddhh)
    ENV_MODELS_PWR$dt_hr_param_pwr[, RIC := NULL]

    ENV_MODELS_PWR$gas_history = ENV_SPOT$history_gas[date >= LST_PARAMS$history_start & date <= LST_PARAMS$history_end, .(date, value, RIC)]
    ENV_MODELS_PWR$gas_history = ENV_MODELS_PWR$gas_history[RIC == HPFC::spot_GAS_products_full[products_GAS %in% unique(c(LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$spot_GAS_code, .(date, value)] 
    
    ENV_MODELS_PWR$calendar_holidays_pwr = copy(ENV_CODES$calendar_holidays)

    return(ENV_MODELS_PWR)
}


#' Train Gas Model
#'
#' This function trains a long-term (LT) gas model by computing regressors and fitting the model.
#'
#' @param gas_data A data.table containing processed gas data.
#'
#' @return A data.table with trained gas model results, including computed regressors.
#'
#' @import data.table
#' @import HPFC
#' @export
train_lt_gas = function(gas_data, ric_gas) {
    ## Train GAS ------------------------- 
    print(paste('GAS LT TRAIN'))
    
    gas_data = HPFC::regressors_lt_model_gas(gas_data, alpha = 0.4)
    gas_data = HPFC::train_lt_model_gas(gas_data)
    gas_data[, RIC := as.character(ric_gas)]
    
    return(gas_data)
}


#' Train Long-Term Power Model
#'
#' This function trains a long-term (LT) power model by computing regressors, incorporating gas price data, 
#' merging holiday effects, and fitting the model.
#'
#' @param pwr_data A data.table containing processed power data.
#' @param ric_pwr A unique identifier (RIC) for the power data.
#' @param gas_history A data.table containing historical gas prices (must include `date` and `value` columns).
#' @param pwr_holidays A data.table containing holiday effects for power modeling (must include `date`).
#'
#' @return A data.table with the trained power model results, including computed regressors and gas dependencies.
#'
#' @import data.table
#' @import HPFC
#' @export
train_lt_pwr = function(pwr_data, ric_pwr, gas_history, pwr_holidays) {
    
    ## Train PWR ------------------------- 
    print(paste('PWR LT TRAIN'))
    
    pwr_data = regressors_lt_model_pwr(pwr_data, alpha = 0.4)
    
    ### Gas Dependent
    pwr_data = gas_history[, .(date, value_gas = value)][pwr_data, on = 'date'] 
    pwr_data = pwr_holidays[pwr_data, on = 'date']         

    pwr_data = train_lt_model_pwr(pwr_data)
    pwr_data[, RIC := as.character(ric_pwr)]
    
    return(pwr_data)
}



#' Train Short-Term Power Model
#'
#' This function trains a short-term (ST) power model by computing regressors, incorporating gas price data, 
#' and fitting the model.
#'
#' @param pwr_data A data.table containing processed short-term power data.
#' @param gas_history A data.table containing historical gas prices (must include `date` and `value` columns).
#'
#' @return A trained short-term power model (data.table).
#'
#' @import data.table
#' @import HPFC
#' @export
train_st_pwr = function(pwr_data, gas_history) {

    print(paste('PWR ST TRAIN'))
    
    dt_pwr_filt_ddhh_wreg = regressors_st_model_pwr(pwr_data)
    dt_pwr_filt_ddhh_wreg = gas_history[, .(date, value_gas = value)][dt_pwr_filt_ddhh_wreg, on = 'date']
    
    pwr_data = train_st_model_pwr(dt_pwr_filt_ddhh_wreg)
    
    return(pwr_data)
}



