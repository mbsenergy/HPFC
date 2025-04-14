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
#' @importFrom eikondata set_proxy_port set_app_id
#' 
#' @export
load_inputs = function(params, manual_data = NULL, reuters_key = NULL, last_run_path = NULL) {
    
    LST_PARAMS = params
    
    if(LST_PARAMS$sim_name != 'NO') {
        
        LST_DIRS = list(
            dir_data_raw  = file.path('run', LST_PARAMS$sim_name, '01_raw'),
            dir_data_output   = file.path('run', LST_PARAMS$sim_name, '02_output'),
            dir_data_other    = file.path('run', LST_PARAMS$sim_name, '03_misc')
        )
        
    }
    
    if(LST_PARAMS$archive != 'NO') {
        
        LST_DIRS_archive = list(
            dir_data_raw  = file.path('run', LST_PARAMS$archive, '01_raw'),
            dir_data_output   = file.path('run', LST_PARAMS$archive, '02_output'),
            dir_data_other    = file.path('run', LST_PARAMS$archive, '03_misc')
        )
        
        lapply(LST_DIRS_archive, dir.create, recursive = TRUE, showWarnings = FALSE)
        
    }
    
    ### CODES Parameters
    ENV_CODES = list()
    calendar_holidays = as.data.table(eikondata::new_calendar_holidays)
    setnames(calendar_holidays, paste0("holiday_", LST_PARAMS$selected_pwr_code), 'holiday', skip_absent = TRUE)
    ENV_CODES$calendar_holidays = calendar_holidays[, .(date, holiday)]
    
    gas_codes = eikondata::gas_products_full[products_GAS %in% LST_PARAMS$selected_gas_code]$spot_GAS_code
    pwr_codes = eikondata::pwr_products_full[countries %in% LST_PARAMS$selected_pwr_code]$spot_PWR_code
    # A. Spot Market Data
    ENV_SPOT = list()
    
    if(LST_PARAMS$data_source == 'Excel') {
        if(!is.null(manual_data)) {
            
            ENV_SPOT$history_gas_full = manual_data[RIC == gas_codes]
            ENV_SPOT$history_gas_full[, hour := NULL]
            ENV_SPOT$history_gas = ENV_SPOT$history_gas_full[date <= LST_PARAMS$history_end]
            ENV_SPOT$spot_gas_RIC = gas_codes
            
            cat(crayon::green$bold("\n✔ Manual Data Gas retrieved. \n"))
            
            ENV_SPOT$history_pwr_full = manual_data[RIC == pwr_codes]
            ENV_SPOT$history_pwr = ENV_SPOT$history_pwr_full[date <= LST_PARAMS$history_end]
            ENV_SPOT$spot_pwr_RIC = pwr_codes
            
            cat(crayon::green$bold("\n✔ Manual Data Power retrieved. \n"))
        } else {
            stop('Missing manual data!')
        }
        
    } 
    
    if(LST_PARAMS$data_source != 'Excel') {
        
        ### Connection
        eikondata::set_proxy_port(9000L)
        PLEASE_INSERT_REUTERS_KEY = reuters_key[[1]]
        eikondata::set_app_id(as.character(PLEASE_INSERT_REUTERS_KEY))
        
        is_eikon_gas = eikondata::gas_mapped_codes[products %in% LST_PARAMS$selected_gas_code]$eikon
        is_eikon_pwr = eikondata::pwr_mapped_codes[countries == eikondata::pwr_products_full[spot_PWR_code == pwr_codes]$countries]$eikon
        if (is_eikon_gas == 'NO') {
            file_path = file.path(last_run_path, 'backup_spot_gas.xlsx')
            print(file_path)
            sheet_names = openxlsx::getSheetNames(file_path)
            print(sheet_names)
            df = openxlsx::read.xlsx(file_path, sheet = LST_PARAMS$selected_gas_code, detectDates = TRUE)
            ENV_SPOT$history_gas_full = data.table::as.data.table(df)
            ENV_SPOT$spot_gas_RIC = gas_codes
            print(ENV_SPOT$history_gas_full)
        } else {
            ENV_SPOT$history_gas_full = eikondata::dt_spot_gas[RIC == gas_codes]
            ENV_SPOT$spot_gas_RIC = gas_codes
        }
        
        if (is_eikon_pwr == 'NO') {
            file_path = file.path(last_run_path, 'backup_spot_pwr.xlsx')
            print(file_path)
            sheet_names = openxlsx::getSheetNames(file_path)
            print(sheet_names)
            df = openxlsx::read.xlsx(file_path, sheet = LST_PARAMS$selected_pwr_code, detectDates = TRUE)
            ENV_SPOT$history_pwr_full = data.table::as.data.table(df)
            ENV_SPOT$spot_pwr_RIC = LST_PARAMS$selected_pwr_code
            print(ENV_SPOT$history_pwr_full)
        } else {
            ENV_SPOT$history_pwr_full = eikondata::dt_spot_pwr[RIC == pwr_codes]
            ENV_SPOT$spot_pwr_RIC = pwr_codes
        }
        
        ENV_SPOT$history_gas = ENV_SPOT$history_gas_full[date <= LST_PARAMS$history_end]
        ENV_SPOT$history_pwr = ENV_SPOT$history_pwr_full[date <= LST_PARAMS$history_end]
        
            
        if(LST_PARAMS$data_source != 'Excel') {
            
            ## GAS
            if(is_eikon_gas == 'YES' & as.character(LST_PARAMS$history_end) >= '2025-01-01') {
                DT_NEW = eikondata::retrieve_spot(
                    ric = gas_codes,
                    from_date = as.character('2025-01-01'),
                    to_date = as.character(LST_PARAMS$history_end),
                    type = 'GAS')
                
                ENV_SPOT$history_gas_full = 
                    rbind(
                        ENV_SPOT$history_gas_full,
                        DT_NEW,
                        use.names=TRUE
                    )
            }
            
            ENV_SPOT$history_gas = ENV_SPOT$history_gas_full[date <= LST_PARAMS$history_end]
            ## PWR
            if(is_eikon_pwr == 'YES' & as.character(LST_PARAMS$history_end) >= '2025-01-01') {
                
                DT_NEW = eikondata::retrieve_spot(
                    ric = pwr_codes,
                    from_date = '2025-01-01',
                    to_date = LST_PARAMS$history_end,
                    type = 'PWR')
                
                ENV_SPOT$history_pwr_full = 
                    rbind(
                        ENV_SPOT$history_pwr_full,
                        DT_NEW,
                        use.names=TRUE
                    )
                
            }
            
            ENV_SPOT$history_pwr = ENV_SPOT$history_pwr_full[date <= LST_PARAMS$history_end]
            
        }
        
    }
    
    ## PREPARE AND RETURN
    
    if(LST_PARAMS$archive != 'NO') {
        
        ## Backup
        saveRDS(ENV_SPOT, file.path(LST_DIRS_archive$dir_data_raw, 'ENV_SPOT.rds'))
        saveRDS(ENV_CODES, file.path(LST_DIRS_archive$dir_data_other, 'ENV_CODES.rds'))
        
        ## Single files
        saveRDS(ENV_SPOT$history_gas, file.path(LST_DIRS_archive$dir_data_raw, 'history_gas.rds'))
        saveRDS(ENV_SPOT$history_pwr, file.path(LST_DIRS_archive$dir_data_raw, 'history_pwr.rds'))
        
        
        ## CSVs
        fwrite(ENV_SPOT$history_gas, file.path(LST_DIRS_archive$dir_data_raw, 'history_gas.csv'))
        fwrite(ENV_SPOT$history_pwr, file.path(LST_DIRS_archive$dir_data_raw, 'history_pwr.csv'))
        
        return_list = list(LST_PARAMS, LST_DIRS_archive, ENV_CODES, ENV_SPOT)
        names(return_list) = c('LST_PARAMS', 'LST_DIRS_archive', 'ENV_CODES', 'ENV_SPOT')
        
        cat(crayon::green$bold("\n✔ Archived inputs in:"), LST_DIRS_archive$dir_data_raw, "\n")
        
    }
    
    if(LST_PARAMS$sim_name != 'NO') {
        
        if(LST_PARAMS$data_source == 'Excel') {
            
            ENV_SPOT = readRDS(file.path(LST_DIRS$dir_data_raw, 'ENV_SPOT.rds'))
            ENV_CODES = readRDS(file.path(LST_DIRS$dir_data_other, 'ENV_CODES.rds'))
            
            cat(crayon::green$bold("\n✔ Sim Data retrieved from:"), LST_DIRS$dir_data_raw, "\n")
            
        }
        
        return_list = list(LST_PARAMS, LST_DIRS, ENV_CODES, ENV_SPOT)
        names(return_list) = c('LST_PARAMS', 'LST_DIRS', 'ENV_CODES', 'ENV_SPOT')        
        
    }
    
    if(LST_PARAMS$sim_name == 'NO' & LST_PARAMS$archive == 'NO') {
        return_list = list(LST_PARAMS, ENV_CODES, ENV_SPOT)
        names(return_list) = c('LST_PARAMS', 'ENV_CODES', 'ENV_SPOT')   
        
        cat(crayon::green$bold("\n✔ Prepared inputs in for training."), "\n")
    }
    
    return(return_list)
}


#' Prepare Forward Curves for Forecasting
#'
#' Prepares forward market data for power and gas products to be used in forecasting models. Supports both
#' automated data retrieval and manual input. Outputs are stored and returned for downstream forecasting workflows.
#'
#' @param fwd_pwr_code Character vector. Power forward product code(s).
#' @param fwd_gas_code Character vector. Gas forward product code(s).
#' @param start_date Date. Forecast start date.
#' @param end_date Date. Forecast end date.
#' @param model_type Character. Either `'PWR'` (default) or `'GAS'`, determining which forward products to retrieve.
#' @param forecast_source Character. Forward price source, `'FWD'` (default) or `'DAM'`.
#' @param archive Character. Path where output files are saved.
#' @param manual_pwr Optional `data.table`. Manually supplied forward power data.
#' @param manual_gas Optional `data.table`. Manually supplied forward gas data.
#'
#' @details
#' If `manual_pwr` and `manual_gas` are both provided, the function uses them instead of retrieving raw data.
#' Otherwise, it constructs a list of required RICs and fetches the forward curves from internal datasets and APIs.
#' It filters the latest available value for each RIC and ensures coverage for the forecast horizon.
#'
#' The function saves three files to the specified `archive` path (unless `LST_PARAMS$archive == 'NO'`):
#' - `ENV_FWD.rds`: list containing metadata and raw data
#' - `dt_fwds.rds`: processed forward curve data
#' - `dt_fwds.csv`: forward data in CSV format
#'
#' @return A named list containing:
#' \describe{
#'   \item{ENV_FWD}{A list with calendar, RICs, forward data, and metadata for downstream modeling.}
#' }
#'
#' @import data.table
#' @importFrom crayon green bold
#' @export
prepare_fwd = function(fwd_pwr_code = NULL, fwd_gas_code = NULL, start_date, end_date, model_type = 'PWR', forecast_source = 'FWD', archive, manual_pwr = NULL, manual_gas = NULL, reuters_key = NULL) {
    
    ENV_FWD = list()
    
    forecast_start = start_date
    forecast_end = end_date
    
    if(model_type == 'GAS') {
        selected_gas_code = fwd_gas_code
        ENV_FWD$fwd_gas_RIC = unique(eikondata::gas_products_full[products_GAS %in% c(selected_gas_code)]$products_GAS_code) ; selected_pwr_code = 'Greece'
        
    } else {
        selected_gas_code = fwd_gas_code
        selected_pwr_code = fwd_pwr_code
        ENV_FWD$fwd_gas_RIC = unique(eikondata::gas_products_full[products_GAS %in% c(selected_gas_code)]$products_GAS_code)
        ENV_FWD$fwd_pwr_RIC =  unique(eikondata::pwr_products_full[countries %in% selected_pwr_code]$products_PWR_code)
    }
    
    ENV_FWD$last_date = as.Date(forecast_start) - 1
    
    ### CODES Parameters
    calendar_holidays = as.data.table(eikondata::new_calendar_holidays)
    setnames(calendar_holidays, paste0("holiday_", selected_pwr_code), 'holiday', skip_absent = TRUE)
    calendar_future = calendar_holidays[, .(date, holiday)]
    calendar_future[,`:=` (year = as.character(data.table::year(date)), 
                           quarter = as.character(data.table::quarter(date)),
                           month = as.character(data.table::month(date)))
    ]
    
    ENV_FWD$calendar_future = calendar_future[date >= forecast_start & date <= forecast_end]
    
    
    # B. Forward Market Data
    
    ENV_FWD$time_range = as.numeric(data.table::year(as.Date(forecast_start))):as.numeric(data.table::year(as.Date(forecast_end)))
    
    is_manual = !is.null(manual_pwr) | !is.null(manual_gas)
    
    if(isFALSE(is_manual)) {
        
        DT_GAS = eikondata::dt_fwds_gas[substr(RIC, 1, 4) == ENV_FWD$fwd_gas_RIC]
        if(model_type == 'PWR') {
            if(forecast_source == 'FWD') {
                DT_PWR = eikondata::dt_fwds_pwr_fwddam[spot_PWR_code == eikondata::pwr_products_full[countries %in% selected_pwr_code]$spot_PWR_code, .(date, RIC, value = FWD)]
            } else {
                DT_PWR = eikondata::dt_fwds_pwr_fwddam[spot_PWR_code == eikondata::pwr_products_full[countries %in% selected_pwr_code]$spot_PWR_code, .(date, RIC, value = DAM)]
            }
        }
        
        ## Generate RICS
        lst_rics_gas = eikondata::generate_rics_gas(unique(ENV_FWD$fwd_gas_RIC), time_range = 2025:as.numeric(data.table::year(as.Date(forecast_end))))
        
        if(model_type == 'PWR') {
            ### POWER 
            lst_rics_pwr = eikondata::generate_rics_pwr(selected_pwr_code, time_range = 2025:as.numeric(data.table::year(as.Date(forecast_end))))
            ENV_FWD$lst_rics = c(lst_rics_pwr, lst_rics_gas) ; rm(lst_rics_pwr, lst_rics_gas)
            
        } else {
            ENV_FWD$lst_rics = c(lst_rics_gas) ; rm(lst_rics_gas)
        }
        
        ## RETRIEVE
        if(model_type == 'PWR') {
            ENV_FWD$dt_fwds = 
                rbind(DT_GAS,
                      DT_PWR
                )  
        } else {
            ENV_FWD$dt_fwds = DT_GAS
        }
        
        ENV_FWD$dt_fwds = ENV_FWD$dt_fwds[ENV_FWD$dt_fwds[, .I[date == max(date)], by = RIC]$V1]
        
        if(max(ENV_FWD$time_range) > 2024) {
            
            ### Connection
            eikondata::set_proxy_port(9000L)
            PLEASE_INSERT_REUTERS_KEY = reuters_key[[1]]
            eikondata::set_app_id(as.character(PLEASE_INSERT_REUTERS_KEY))
            
            DT_NEW = eikondata::retrieve_fwd(ric = ENV_FWD$lst_rics, from_date = '2025-01-01', to_date = forecast_end)
            
            ENV_FWD$dt_fwds = rbind(
                ENV_FWD$dt_fwds,
                DT_NEW,
                use.names=TRUE
            )
            ENV_FWD$dt_fwds = ENV_FWD$dt_fwds[ENV_FWD$dt_fwds[, .I[date == max(date)], by = RIC]$V1]
        }
        
        if (is.null(ENV_FWD$dt_fwds) && nrow(ENV_FWD$dt_fwds) == 0) {
            
            stop('NO RAW DATA RETRIEVED')
            
        }
        
    }
    
    if(isTRUE(is_manual)) {
        dt_fwd_gas = manual_gas
        dt_fwd_prep_gas = merge(dt_fwd_gas, generate_monthrics_gas(unique(eikondata::gas_products_full[products_GAS %in% c(selected_gas_code)]$products_GAS_code), time_range = 2024), by.x = 'yymm', by.y ='date', all.x = TRUE) 
        
        if(model_type == 'PWR') {
            dt_fwd_pwr = manual_pwr
            dt_fwd_prep_pwr = merge(dt_fwd_pwr, generate_monthrics_pwr(unique(eikondata::pwr_products_full[countries %in% selected_pwr_code]$countries), time_range = 2024), by.x = 'yymm', by.y ='date', all.x = TRUE) 
            dt_fwds = rbind(dt_fwd_prep_pwr, dt_fwd_prep_gas)
            
        } else {
            dt_fwds = dt_fwd_prep_gas
        }
        
        dt_fwds[, sim := NULL]
        colnames(dt_fwds) = c('date', 'value', 'RIC')
        
        ENV_FWD$dt_fwds = dt_fwds
        
    }
    
    if(archive != 'NO') {
        saveRDS(ENV_FWD, file.path(archive, 'ENV_FWD.rds'))
        saveRDS(ENV_FWD$dt_fwds, file.path(archive, 'dt_fwds.rds'))
        fwrite(ENV_FWD$dt_fwds, file.path(archive, 'dt_fwds.csv'))
    }
    
    cat(crayon::green$bold("\n✔ Prepared fwds in for forecasting."), "\n")
    
    return_list = list(ENV_FWD)
    names(return_list) = c('ENV_FWD') 
    
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
    # ENV_MODELS_GAS$dt_lt_param_gasdep[, RIC := NULL]
    
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
    ENV_MODELS_PWR$gas_history = ENV_MODELS_PWR$gas_history[RIC == eikondata::gas_products_full[products_GAS %in% unique(c(LST_PARAMS$LST_PARAMS$selected_gas_code, LST_PARAMS$dependent_gas_code))]$spot_GAS_code, .(date, value)] 
    
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


#' Forecast Gas Prices
#'
#' This function generates gas price forecasts using historical data, forward market quotes, and a long-term model.
#'
#' @param input_forecast A list containing the required data for forecasting:
#'   \itemize{
#'     \item `ric_spot_gas`: RIC identifier for spot gas prices.
#'     \item `ric_fwd_gas`: RIC identifier for forward gas prices.
#'     \item `dt_gas_fwds`: Data.table with forward market quotes (year, quarter, month, and price columns).
#'     \item `saved_history_gas`: Data.table with historical gas prices.
#'     \item `model_lt_gas`: Data.table with the trained long-term gas model.
#'     \item `calendar_forecast`: Data.table defining the forecast calendar.
#'     \item `last_date`: The last available historical date for calibration.
#'   }
#'
#' @return A data.table with gas price forecasts, including adjusted spot-forward blends.
#'
#' @import data.table
#' @import HPFC
#' @export
forecast_gas = function(input_forecast = LST_FOR) {
    ## Forecast GAS ---------------------------------------
    print(paste('FORECAST GAS'))
    
    LST_FOR = input_forecast
    spot_RIC = LST_FOR$ric_spot_gas
    fwd_RIC = LST_FOR$ric_fwd_gas
    
    dt_gas = LST_FOR$saved_history_gas[, .(date, value, RIC)]
    ric_gas = unique(dt_gas$RIC)
    dt_gas = HPFC::break_detection_dd(dt_gas)
    # dt_gas_dd_filt = HPFC::filter_outlier_dd(dt_gas)
    dt_gas_dd_filt = dt_gas
    
    fwd_calendar = eikondata::calendar_holidays
    fwd_calendar[, `:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]
    
    dt_fwd_gas = eikondata::prep_fwd_curve(DT = LST_FOR$dt_fwds,
                                      list_rics = fwd_RIC,
                                      type = 'GAS',
                                      start_date = LST_FOR$start_date,
                                      end_date = LST_FOR$end_date, 
                                      calendar_sim = fwd_calendar)
    
    
    dt_gas_fwds = dt_fwd_gas[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
    dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]
    
    saved_history_gas = copy(dt_gas_dd_filt)
    # saved_history_gas = saved_history_gas[, RIC := NULL]
    
    free_fwd_gas = HPFC::arbitrage_free_gas(dt_gas_fwds, DT_history = saved_history_gas, colnames(dt_gas_fwds))
    dt_arbfree_fwd_gas = free_fwd_gas[, .(year, month, BL_quotes_gas, BL_gas_prev_m, RIC_s = spot_RIC, RIC_f = fwd_RIC)]
    
    model_lt_gas = LST_FOR$model_lt_gas
    model_lt_gas = model_lt_gas[, RIC := NULL]
    
    ## 3.2 CREATE CALENDAR FOR FORECAST
    forecast_calendar_daily = HPFC::create_calendar_dd(LST_FOR$calendar_forecast)
    forecast_calendar_daily = saved_history_gas[forecast_calendar_daily, on = 'date']                 
    
    #### Merge calendar with forward market data
    forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]        
    
    #### Spot before current date and forward price after
    forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= LST_FOR$last_date, value, BL_quotes_gas)]
    dt_gas_for_dd = HPFC::predict_lt_gas(forecast_calendar_daily, profile_matrix = model_lt_gas)
    
    dt_gas_for_dd = HPFC::period_calibration(dt_gas_for_dd, last_date = LST_FOR$last_date)
    
    dt_gas_for_dd[, epsilon_u := spot_forward_month_BL]
    dt_gas_for_dd[, L_e_u := L_t + epsilon_u]
    
    dt_gas_for_dd = HPFC::spline_gas(dt_gas_for_dd, smoothig_parameter = 20)
    dt_gas_for_dd[, RIC := spot_RIC]
    
    rm(forecast_calendar_daily, model_lt_gas, free_fwd_gas, dt_arbfree_fwd_gas, saved_history_gas, dt_gas_fwds, fwd_RIC, spot_RIC)
    
    return(dt_gas_for_dd)
}



#' Forecast Power Prices
#'
#' This function forecasts power prices using long-term and short-term models, forward market quotes, and gas price dependencies.
#'
#' @param input_forecast A list containing required data for power forecasting:
#'   \itemize{
#'     \item `ric_spot_gas`: RIC identifier for spot gas prices.
#'     \item `ric_fwd_gas`: RIC identifier for forward gas prices.
#'     \item `dt_gas_fwds`: Data.table with forward market quotes for gas.
#'     \item `dt_pwr_fwds`: Data.table with forward market quotes for power.
#'     \item `saved_history_pwr`: Data.table with historical power prices.
#'     \item `saved_history_gas_bis`: Data.table with historical gas prices.
#'     \item `model_lt_pwr`: Data.table with the long-term power model.
#'     \item `model_st_pwr`: Data.table with the short-term power model.
#'     \item `calendar_forecast`: Data.table defining the forecast calendar.
#'     \item `last_date`: The last available historical date for calibration.
#'   }
#' @param gas_forecast A data.table containing the gas price forecast with columns `date` and `smooth_corrected`.
#'
#' @return A data.table with power price forecasts, including short-term and long-term components.
#'
#' @import data.table
#' @import HPFC
#' @export
forecast_pwr = function(input_forecast = LST_FOR, gas_forecast = ENV_FOR_GAS, smooth = 13) {
    
    print(paste('FORECAST PWR'))
    
    LST_FOR = input_forecast
    spot_RIC = LST_FOR$ric_spot_pwr
    fwd_RIC_gas = LST_FOR$ric_fwd_gas
    fwd_RIC = LST_FOR$ric_fwd_pwr
    
    fwd_calendar = eikondata::calendar_holidays
    fwd_calendar[, `:=` (year = as.character(data.table::year(date)), quarter = as.character(data.table::quarter(date)), month = as.character(data.table::month(date)))]
    
    dt_fwd_gas = eikondata::prep_fwd_curve(DT = LST_FOR$dt_fwds,
                                      list_rics = fwd_RIC_gas,
                                      type = 'GAS',
                                      start_date = LST_FOR$start_date,
                                      end_date = LST_FOR$end_date, 
                                      calendar_sim = fwd_calendar)
    
    # Extract and convert forward market data
    dt_gas_fwds = dt_fwd_gas[, .(year, quarter, month, forward_cal_BL_gas, forward_quarter_BL_gas, forward_month_BL_gas)]
    dt_gas_fwds = dt_gas_fwds[, lapply(.SD, as.numeric)]
    
    dt_fwd_pwr = eikondata::prep_fwd_curve(DT = LST_FOR$dt_fwds,
                                      list_rics = fwd_RIC,
                                      type = 'PWR',
                                      start_date = LST_FOR$start_date,
                                      end_date = LST_FOR$end_date, 
                                      calendar_sim = fwd_calendar)
    
    
    dt_pwr_fwds = dt_fwd_pwr[, .(year, quarter, month, forward_cal_BL_pwr, forward_quarter_BL_pwr, 
                                 forward_month_BL_pwr, forward_cal_PL_pwr, forward_quarter_PL_pwr, forward_month_PL_pwr)]
    dt_pwr_fwds = dt_pwr_fwds[, lapply(.SD, as.numeric)]
    
    dt_pwr = LST_FOR$saved_history_pwr[, .(date, hour, value, RIC)]
    dt_pwr_filt_dd = dt_pwr[, .(date, hour, value)]
    dt_pwr_filt_dd = HPFC::break_detection_ddhh(dt_pwr_filt_dd)
    dt_pwr_filt_dd = HPFC::filter_outlier_dd_pwr(dt_pwr_filt_dd)
    saved_history_pwr = copy(dt_pwr_filt_dd)
    
    dt_gas = LST_FOR$saved_history_gas[, .(date, value, RIC)]
    ric_gas = unique(dt_gas$RIC)
    dt_gas = HPFC::break_detection_dd(dt_gas)
    # dt_gas_dd_filt = HPFC::filter_outlier_dd(dt_gas)
    dt_gas_dd_filt = dt_gas
    saved_history_gas = copy(dt_gas)
    
    # Apply arbitrage-free transformations
    free_fwd_pwr = HPFC::arbitrage_free_power(dt_pwr_fwds, DT_history = saved_history_pwr, colnames(dt_pwr_fwds))
    free_fwd_gas = HPFC::arbitrage_free_gas(dt_gas_fwds, DT_history = saved_history_gas, colnames(dt_gas_fwds))
    
    dt_arbfree_fwd_pwr = free_fwd_pwr[, .(year, month, BL_quotes, PL_quotes, RIC_s = spot_RIC, RIC_f = fwd_RIC)]
    
    # Copy models
    model_lt_pwr_long = copy(LST_FOR$model_lt_pwr)[, RIC := NULL]
    model_st_pwr = copy(LST_FOR$model_st_pwr)
    
    ## Gas Calibration
    calibration_gas = copy(gas_forecast)[, .(date, value_gas = smooth_corrected)]
    
    forecast_calendar_daily_raw = HPFC::create_calendar_dd(LST_FOR$calendar_forecast)
    
    ## Long-Term Calibration
    forecast_calendar_daily = saved_history_pwr[forecast_calendar_daily_raw, on = 'date']
    forecast_calendar_daily = free_fwd_pwr[forecast_calendar_daily, on = c('month', 'year')]
    forecast_calendar_daily = free_fwd_gas[forecast_calendar_daily, on = c('month', 'year')]
    
    forecast_calendar_daily[, spot_forward_month_BL := fifelse(date <= LST_FOR$last_date, value_day, BL_quotes)]
    forecast_calendar_daily[, spot_forward_month_PL := fifelse(date <= LST_FOR$last_date | PL_quotes <= 0, as.numeric(NA), PL_quotes)]
    
    forecast_calendar_daily = calibration_gas[forecast_calendar_daily, on = 'date']
    
    dt_pwr_for_ddhh = HPFC::predict_lt_pwr(forecast_calendar_daily, profile_matrix = model_lt_pwr_long)
    dt_pwr_for_ddhh = HPFC::period_calibration(dt_pwr_for_ddhh, last_date = LST_FOR$last_date)
    
    ## Short-Term Forecasting
    forecast_calendar_hourly = HPFC::create_calendar_ddhh(dt_pwr_for_ddhh)
    dt_pwr_for_ddhh = HPFC::predict_st_pwr(forecast_calendar_hourly, model_h = model_st_pwr)
    
    dt_pwr_for_ddhh = HPFC::spline_pwr(dt_pwr_for_ddhh, smoothig_parameter = smooth)
    dt_pwr_for_ddhh = HPFC::PL_correction(dt_pwr_for_ddhh)
    dt_pwr_for_ddhh[, RIC := spot_RIC]
    
    return(dt_pwr_for_ddhh)
}
