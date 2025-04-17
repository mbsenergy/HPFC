

server_app = function(input, output, session) {
    
    # 0. PREPARE ------------------------------------------------------
    
    ## Check Connection
    
    check_reuters = reactiveVal(FALSE)
    
    observe({
        print('---------------------------------------------')
        print('CONNECTION TEST')
        check_reuters = FALSE
        
        ### Connection
        eikondata::set_proxy_port(9000L)
        print(PLEASE_INSERT_REUTERS_KEY[[1]])
        eikondata::set_app_id(as.character(PLEASE_INSERT_REUTERS_KEY[[1]]))
        
        tryCatch({
            get_rics_d('MSFT.O', from_date = Sys.Date() - (365 * 10), to_date = Sys.Date())
            check_reuters = TRUE
        }, error = function(e) {
            check_reuters = FALSE
        })
        
        if(isTRUE(check_reuters)) {
            print('[CONNECTION] OK')
            check_reuters(TRUE)
        } else {
            print('[CONNECTION] ERROR')
            check_reuters(FALSE)
        }
    })
    
    output$reuters_status = renderText({
        if (isTRUE(check_reuters())) {
            "✅ EIKON: OK"
        } else {
            "❌ EIKON: ERROR"
        }
    })
    
    ## FORECAST SOURCE -----------------------
    
    observe({
        if (input$in_source_run == 'Sim') {
            output$select_source_run = renderUI({
                textInput(
                    inputId = "sim_name",
                    label = "Simualtion Name",
                    placeholder = c("Insert saved simulation name here..."),
                )
            })
            
        } else {
            output$select_source_run = renderUI(NULL)
        }
    })
    
    ## MANUAL DATA ------------------------------------------------------
    ### SPOTS
    observe({
        if (input$in_source_train == 'Excel') {
            output$select_source_file_train_pwr = renderUI({
                fileInput(
                    inputId = "in_train_excel_pwr",
                    label = "Excel file with Power spot data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })
            
            output$select_source_file_train_gas = renderUI({
                fileInput(
                    inputId = "in_train_excel_gas",
                    label = "Excel file with Gas spot data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })            
            
        } else {
            output$select_source_file_train_pwr = renderUI(NULL)
            output$select_source_file_train_gas = renderUI(NULL)
        }
    })
    
    dt_spot_manual_pwr = reactiveVal(NULL)
    dt_spot_manual_gas = reactiveVal(NULL)
    dt_spot_manual = reactiveVal(NULL)
    
    observe({
        req(input$in_train_excel_pwr)
        file_path = input$in_train_excel_pwr$datapath
        
        # Get all sheet names
        sheet_names = openxlsx::getSheetNames(file_path)
        
        # Read and bind all sheets
        dt_all = rbindlist(
            lapply(sheet_names, function(sheet) {
                df = openxlsx::read.xlsx(file_path, sheet = sheet, detectDates = TRUE)
                dt = data.table::as.data.table(df)
                dt[, sheet_name := sheet]  # Optional: track source sheet
                return(dt)
            }),
            use.names = TRUE,
            fill = TRUE
        )
        
        # Store in reactive value
        dt_spot_manual_pwr(dt_all)
    })
    
    
    observe({
        req(input$in_train_excel_gas)
        file_path = input$in_train_excel_gas$datapath
        
        # Get all sheet names
        sheet_names = openxlsx::getSheetNames(file_path)
        
        # Read and bind all sheets
        dt_all = rbindlist(
            lapply(sheet_names, function(sheet) {
                df = openxlsx::read.xlsx(file_path, sheet = sheet, detectDates = TRUE)
                dt = data.table::as.data.table(df)
                dt[, sheet_name := sheet]  # Optional: track source sheet
                return(dt)
            }),
            use.names = TRUE,
            fill = TRUE
        )
        
        # Store in reactive value
        dt_spot_manual_gas(dt_all)
    })
    
    
    observe({
        req(react$dt_spot_manual_gas, react$dt_spot_manual_pwr)
        DTS = rbind(react$dt_spot_manual_pwr, react$dt_spot_manual_gas, fill = TRUE)
        dt_spot_manual(DTS)
        
        showNotification("Manual data prepared", type = "message")
    })    
    
    
    ### FWD
    observe({
        if (input$in_source_forecast == 'Excel') {
            output$select_source_file_forecast_pwr = renderUI({
                fileInput(
                    inputId = "in_forecast_excel_pwr",
                    label = "Excel with Power forecast data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })
            
            output$select_source_file_forecast_gas = renderUI({
                fileInput(
                    inputId = "in_forecast_excel_gas",
                    label = "Excel with Gas forecast data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })
            
        } else {
            output$select_source_file_forecast_pwr = renderUI(NULL)
            output$select_source_file_forecast_gas = renderUI(NULL)
        }
    })
    
    dt_forecast_manual_pwr = reactiveVal(NULL)
    dt_forecast_manual_gas = reactiveVal(NULL)
    
    observe({
        req(input$in_forecast_excel_pwr)
        file_path = input$in_forecast_excel_pwr$datapath
        sheet_names = openxlsx::getSheetNames(file_path)
        if (input$in_select_PWR_indicator %in% sheet_names) {
            df = openxlsx::read.xlsx(file_path, sheet = input$in_select_PWR_indicator_for, detectDates = TRUE)
            dt = data.table::as.data.table(df)
            dt_forecast_manual_pwr(dt)
        } else {
            dt_forecast_manual_pwr(NULL)
            warning("Selected sheet not found in Excel file.")
        }
    })
    
    
    observe({
        req(input$in_forecast_excel_gas)
        file_path = input$in_forecast_excel_gas$datapath
        sheet_names = openxlsx::getSheetNames(file_path)
        if (input$in_select_GAS_indicator %in% sheet_names) {
            df = openxlsx::read.xlsx(file_path, sheet = input$in_select_GAS_indicator_for, detectDates = TRUE)
            dt = data.table::as.data.table(df)
            dt_forecast_manual_gas(dt)
        } else {
            dt_forecast_manual_gas(NULL)
            warning("Selected sheet not found in Excel file.")
        }
    })
    
    
    
    # A. MULTIPLE - TRAIN - PWR ------------------------------------------
    
    list_pwr_multi = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_train_pwr_mult, {
        
        if (is.null(react$dt_spot_manual_gas)) {
            showNotification("Missing manual gas spot data. Training cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if(!is.null(react$dt_spot_manual) & !is.null(react$dt_spot_manual_gas) & input$in_source_train == 'Excel') {
            
            LST_PARAMS = list(
                model_type = 'PWR',
                selected_pwr_code = NULL,
                selected_gas_code = 'TTF',
                dependent_gas_code = 'TTF',
                history_start = input$in_select_history[1],
                history_end = input$in_select_history[2],
                forecast_start = input$in_select_horizon[1],
                forecast_end = input$in_select_horizon[2],
                model_source = 'TRAIN',
                data_source = input$in_source_train,
                forecast_source = 'FWD',
                sim_name = 'NO',
                archive = 'NO',
                shiny_sim = input$in_new_sim_name,
                shiny_manual = react$dt_spot_manual
            )            
            
            list_pwr = lapply(input$in_select_PWR_indicator_mult, function(x, LST_PARAMS) {
                
                tryCatch({
                    print('')
                    print('==================== ++++++++++++++++++ ====================')
                    print('==================== MANUAL MULTI TRAINING PWR ====================')
                    print(x)
                    print('==================== ++++++++++++++++++ ====================')
                    
                    LST_PARAMS$selected_pwr_code = x
                    
                    print('')
                    print('==================== START TRAINING PWR ====================')
                    print('')
                    print('-------------------- LOAD INPUTS START  --------------------')
                    
                    list_inputs = HPFC::load_inputs(params = LST_PARAMS,
                                                    manual_data = LST_PARAMS$shiny_manual,
                                                    reuters_key = NULL)
                    
                    ## ARCHIVE
                    last_path = file.path('HPFC', 'last', 'history', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'history', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                        fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
                    }
                    
                    print('-------------------- LOAD INPUTS END   --------------------')     
                    
                    print('============= +++++++++++++ ====================')
                    print('------------- PREPARE START --------------------')
                    
                    ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
                    ENV_MODELS_PWR = prepare_pwr(list_inputs = list_inputs)
                    
                    print('------------- PREPARE END ----------------------')
                    
                    print('============= +++++++++++ ====================')
                    print('------------- TRAIN START --------------------')
                    
                    ENV_MODELS_GAS$dt_lt_param_gasdep = 
                        train_lt_gas(
                            gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                            ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
                        )
                    
                    ENV_MODELS_PWR$dt_lt_param_pwr = 
                        train_lt_pwr(
                            pwr_data = ENV_MODELS_PWR$dt_lt_param_pwr,
                            ric_pwr = unique(ENV_MODELS_PWR$dt_pwr$RIC),
                            pwr_holidays = ENV_MODELS_PWR$calendar_holidays_pwr,
                            gas_history = ENV_MODELS_PWR$gas_history
                        )
                    
                    ENV_MODELS_PWR$lst_hr_param_pwr = 
                        train_st_pwr(
                            pwr_data = ENV_MODELS_PWR$dt_hr_param_pwr,
                            gas_history = ENV_MODELS_PWR$gas_history
                        )
                    
                    ## ARCHIVE
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'models', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
                    saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'models', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                        saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
                        saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
                    }
                    
                    # models_gas_field_pwr(ENV_MODELS_GAS)
                    # models_pwr_field(ENV_MODELS_PWR)
                    
                    print('------------- TRAIN END  --------------------') 
                    
                    print('============= +++++++++++ ====================')
                    print('------------- PLOT START --------------------')
                    
                    DT = copy(list_inputs$ENV_SPOT$history_pwr)
                    DT[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
                    rics = unique(DT$RIC) 
                    setorder(DT, datetime, RIC)
                    
                    PLOT_X = 
                        DT %>%
                        e_charts(datetime) %>%
                        e_line(value, name = rics, symbol = 'none') %>%
                        e_title(text = paste("Hourly Spot Prices for", rics)) %>%
                        e_x_axis(name = "Datetime") %>%
                        e_y_axis(name = "Price") %>%
                        e_tooltip(trigger = "axis") %>%
                        e_datazoom(type = "slider") %>%
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_theme("westeros") 
                    
                    print('------------- PLOT END  --------------------')
                    showNotification(paste(x, 'completed!'), type = "default", duration = 20)
                    
                    DT_X = DT
                    
                    LIST_X = list(PLOT_X, DT_X)
                    names(LIST_X) = c('PLOT_X', 'DT_X')
                    
                    return(LIST_X)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            LST_PARAMS = LST_PARAMS
            ) 
            
        } else {
            
            LST_PARAMS = list(
                model_type = 'PWR',
                selected_pwr_code = NULL,
                selected_gas_code = 'TTF',
                dependent_gas_code = 'TTF',
                history_start = input$in_select_history[1],
                history_end = input$in_select_history[2],
                forecast_start = input$in_select_horizon[1],
                forecast_end = input$in_select_horizon[2],
                model_source = 'TRAIN',
                data_source = input$in_source_train,
                forecast_source = 'FWD',
                sim_name = 'NO',
                archive = 'NO',
                shiny_sim = input$in_new_sim_name
            )
            
            list_pwr = lapply(input$in_select_PWR_indicator_mult, function(x, LST_PARAMS) {
                
                tryCatch({
                    print('')
                    print('==================== ++++++++++++++++++ ====================')
                    print('==================== REUTERS MULTI TRAINING PWR ====================')
                    print(x)
                    print('==================== ++++++++++++++++++ ====================')
                    
                    LST_PARAMS$selected_pwr_code = x
                    
                    print('')
                    print('==================== START TRAINING PWR ====================')
                    print('')
                    print('-------------------- LOAD INPUTS START  --------------------')
                    
                    list_inputs = HPFC::load_inputs(params = LST_PARAMS, manual_data = NULL,
                                                    reuters_key = PLEASE_INSERT_REUTERS_KEY,
                                                    last_run_path = file.path('HPFC', 'last', 'history')
                    )
                    
                    ## ARCHIVE
                    last_path = file.path('HPFC', 'last', 'history', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'history', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                        fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
                    }
                    
                    print('-------------------- LOAD INPUTS END   --------------------')     
                    
                    print('============= +++++++++++++ ====================')
                    print('------------- PREPARE START --------------------')
                    
                    ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
                    ENV_MODELS_PWR = prepare_pwr(list_inputs = list_inputs)
                    
                    print('------------- PREPARE END ----------------------')
                    
                    print('============= +++++++++++ ====================')
                    print('------------- TRAIN START --------------------')
                    
                    ENV_MODELS_GAS$dt_lt_param_gasdep = 
                        train_lt_gas(
                            gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                            ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
                        )
                    
                    ENV_MODELS_PWR$dt_lt_param_pwr = 
                        train_lt_pwr(
                            pwr_data = ENV_MODELS_PWR$dt_lt_param_pwr,
                            ric_pwr = unique(ENV_MODELS_PWR$dt_pwr$RIC),
                            pwr_holidays = ENV_MODELS_PWR$calendar_holidays_pwr,
                            gas_history = ENV_MODELS_PWR$gas_history
                        )
                    
                    ENV_MODELS_PWR$lst_hr_param_pwr = 
                        train_st_pwr(
                            pwr_data = ENV_MODELS_PWR$dt_hr_param_pwr,
                            gas_history = ENV_MODELS_PWR$gas_history
                        )
                    
                    ## ARCHIVE
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'models', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
                    saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'models', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                        saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
                        saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
                    }
                    
                    # models_gas_field_pwr(ENV_MODELS_GAS)
                    # models_pwr_field(ENV_MODELS_PWR)
                    
                    print('------------- TRAIN END  --------------------')     
                    
                    print('============= +++++++++++ ====================')
                    print('------------- PLOT START --------------------')
                    
                    DT = copy(list_inputs$ENV_SPOT$history_pwr)
                    DT[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
                    rics = unique(DT$RIC) 
                    setorder(DT, datetime, RIC)
                    
                    PLOT_X = 
                        DT %>%
                        e_charts(datetime) %>%
                        e_line(value, name = rics, symbol = 'none') %>%
                        e_title(text = paste("Hourly Spot Prices for", rics)) %>%
                        e_x_axis(name = "Datetime") %>%
                        e_y_axis(name = "Price") %>%
                        e_tooltip(trigger = "axis") %>%
                        e_datazoom(type = "slider") %>%
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_theme("westeros") 
                    
                    print('------------- PLOT END  --------------------')
                    showNotification(paste(x, 'completed!'), type = "default", duration = 20)
                    
                    DT_X = DT
                    
                    LIST_X = list(PLOT_X, DT_X)
                    names(LIST_X) = c('PLOT_X', 'DT_X')
                    
                    return(LIST_X)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            LST_PARAMS = LST_PARAMS
            )
            
        }
        names(list_pwr) = input$in_select_PWR_indicator_mult
        valid_names = names(list_pwr)[!sapply(list_pwr, is.null)]
        list_pwr_multi(list_pwr[valid_names])
        
        updateSelectInput(
            session = session,
            inputId = "in_select_pwrplot_mult",
            choices = valid_names,
            selected = if (length(valid_names) > 0) valid_names[1] else NULL
        )
        
        list_pwr_multi(list_pwr)
        
    })
    
    
    
    # B. MULTIPLE - TRAIN - GAS ------------------------------------------
    
    list_gas_multi = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_train_gas_mult, {
        
        if(!is.null(react$dt_spot_manual_gas) & input$in_source_train == 'Excel') {
            
            LST_PARAMS = list(
                model_type = 'GAS',
                selected_pwr_code = input$in_select_PWR_indicator,
                selected_gas_code = NULL,
                dependent_gas_code = 'TTF',
                history_start = input$in_select_history[1],
                history_end = input$in_select_history[2],
                forecast_start = input$in_select_horizon[1],
                forecast_end = input$in_select_horizon[2],
                model_source = 'TRAIN',
                data_source = input$in_source_train,
                forecast_source = 'FWD',
                sim_name = 'NO',
                archive = 'NO',
                shiny_sim = input$in_new_sim_name,
                shiny_manual = react$dt_spot_manual_gas
            )            
            
            list_gas = lapply(input$in_select_GAS_indicator_mult, function(x, LST_PARAMS) {
                
                tryCatch({
                    print('')
                    print('==================== ++++++++++++++++++ ====================')
                    print('==================== MANUAL MULTI TRAINING GAS ====================')
                    print(x)
                    print('==================== ++++++++++++++++++ ====================')
                    
                    LST_PARAMS$selected_gas_code = x
                    
                    print('')
                    print('==================== START TRAINING GAS ====================')
                    print('')
                    print('-------------------- LOAD INPUTS START  --------------------')
                    
                    list_inputs = HPFC::load_inputs(params = LST_PARAMS,
                                                    manual_data = LST_PARAMS$shiny_manual,
                                                    reuters_key = NULL)
                    
                    ## ARCHIVE
                    last_path = file.path('HPFC', 'last', 'history', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'history', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    }
                    
                    print('-------------------- LOAD INPUTS END   --------------------')     
                    
                    print('============= +++++++++++++ ====================')
                    print('------------- PREPARE START --------------------')
                    
                    ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
                    
                    print('------------- PREPARE END ----------------------')
                    
                    print('============= +++++++++++ ====================')
                    print('------------- TRAIN START --------------------')
                    
                    ENV_MODELS_GAS$dt_lt_param_gasdep = 
                        train_lt_gas(
                            gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                            ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
                        )
                    
                    ## ARCHIVE
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'models', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'models', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    }
                    
                    print('------------- TRAIN END  --------------------') 
                    
                    print('============= +++++++++++ ====================')
                    print('------------- PLOT START --------------------')
                    
                    DT = copy(list_inputs$ENV_SPOT$history_gas)
                    rics = unique(DT$RIC) 
                    setorder(DT, date, RIC)
                    
                    PLOT_X = 
                        DT %>%
                        e_charts(date) %>%
                        e_line(value, name = rics, symbol = 'none') %>%
                        e_title(text = paste("Daily Spot Prices for", rics)) %>%
                        e_x_axis(name = "Date") %>%
                        e_y_axis(name = "Price") %>%
                        e_tooltip(trigger = "axis") %>%
                        e_datazoom(type = "slider") %>%
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_theme("westeros") 
                    
                    print('------------- PLOT END  --------------------')
                    showNotification(paste(x, 'completed!'), type = "default", duration = 20)
                    
                    DT_X = DT
                    
                    LIST_X = list(PLOT_X, DT_X)
                    names(LIST_X) = c('PLOT_X', 'DT_X')
                    
                    return(LIST_X)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            LST_PARAMS = LST_PARAMS
            ) 
            
        } else {
            
            LST_PARAMS = list(
                model_type = 'GAS',
                selected_pwr_code = input$in_select_PWR_indicator,
                selected_gas_code = NULL,
                dependent_gas_code = 'TTF',
                history_start = input$in_select_history[1],
                history_end = input$in_select_history[2],
                forecast_start = input$in_select_horizon[1],
                forecast_end = input$in_select_horizon[2],
                model_source = 'TRAIN',
                data_source = input$in_source_train,
                forecast_source = 'FWD',
                sim_name = 'NO',
                archive = 'NO',
                shiny_sim = input$in_new_sim_name
            )
            
            list_gas = lapply(input$in_select_GAS_indicator_mult, function(x, LST_PARAMS) {
                
                tryCatch({
                    print('')
                    print('==================== ++++++++++++++++++ ====================')
                    print('==================== REUTERS MULTI TRAINING GAS ====================')
                    print(x)
                    print('==================== ++++++++++++++++++ ====================')
                    
                    LST_PARAMS$selected_gas_code = x
                    
                    print('')
                    print('==================== START TRAINING GAS ====================')
                    print('')
                    print('-------------------- LOAD INPUTS START  --------------------')
                    
                    list_inputs = HPFC::load_inputs(params = LST_PARAMS, manual_data = NULL,
                                                    reuters_key = PLEASE_INSERT_REUTERS_KEY,
                                                    last_run_path = file.path('HPFC', 'last', 'history')
                    )
                    
                    ## ARCHIVE
                    last_path = file.path('HPFC', 'last', 'history', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'history', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
                    }
                    
                    print('-------------------- LOAD INPUTS END   --------------------')     
                    
                    print('============= +++++++++++++ ====================')
                    print('------------- PREPARE START --------------------')
                    
                    ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
                    
                    print('------------- PREPARE END ----------------------')
                    
                    print('============= +++++++++++ ====================')
                    print('------------- TRAIN START --------------------')
                    
                    ENV_MODELS_GAS$dt_lt_param_gasdep = 
                        train_lt_gas(
                            gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                            ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
                        )
                    
                    ## ARCHIVE
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'models', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    
                    if(nchar(LST_PARAMS$shiny_sim) > 0) {
                        
                        last_path = file.path('HPFC', 'archive', 'models', x, LST_PARAMS$shiny_sim)
                        if (!dir.exists(last_path)) {
                            dir.create(last_path, recursive = TRUE)
                        }
                        
                        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
                    }
                    
                    print('------------- TRAIN END  --------------------')     
                    
                    print('============= +++++++++++ ====================')
                    print('------------- PLOT START --------------------')
                    
                    DT = copy(list_inputs$ENV_SPOT$history_gas)
                    rics = unique(DT$RIC) 
                    setorder(DT, date, RIC)
                    
                    PLOT_X = 
                        DT %>%
                        e_charts(date) %>%
                        e_line(value, name = rics, symbol = 'none') %>%
                        e_title(text = paste("Daily Spot Prices for", rics)) %>%
                        e_x_axis(name = "Date") %>%
                        e_y_axis(name = "Price") %>%
                        e_tooltip(trigger = "axis") %>%
                        e_datazoom(type = "slider") %>%
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_theme("westeros") 
                    
                    print('------------- PLOT END  --------------------')
                    showNotification(paste(x, 'completed!'), type = "default", duration = 20)
                    
                    DT_X = DT
                    
                    LIST_X = list(PLOT_X, DT_X)
                    names(LIST_X) = c('PLOT_X', 'DT_X')
                    
                    return(LIST_X)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            LST_PARAMS = LST_PARAMS
            )
            
        }
        names(list_gas) = input$in_select_GAS_indicator_mult
        valid_names = names(list_gas)[!sapply(list_gas, is.null)]
        list_gas_multi(list_gas[valid_names])
        
        updateSelectInput(
            session = session,
            inputId = "in_select_gasplot_mult",
            choices = valid_names,
            selected = if (length(valid_names) > 0) valid_names[1] else NULL
        )
        
        list_gas_multi(list_gas)
        
    })
    
    
    
    # C. SINGLE - TRAIN - PWR ------------------------------------------
    
    ## Inputs -----------------------
    params_input_pwr = reactiveVal(NULL)
    list_inputs_field_pwr = reactiveVal(NULL)
    
    ### Prepare inputs params
    observe({
        params_list = list(
            model_type = 'PWR',
            selected_pwr_code = input$in_select_PWR_indicator,
            selected_gas_code = 'TTF',
            dependent_gas_code = 'TTF',
            history_start = input$in_select_history[1],
            history_end = input$in_select_history[2],
            forecast_start = input$in_select_horizon[1],
            forecast_end = input$in_select_horizon[2],
            model_source = 'TRAIN',
            data_source = input$in_source_train,
            forecast_source = 'FWD',
            sim_name = 'NO',
            archive = 'NO'
        )
        
        params_input_pwr(params_list)
        
    })
    
    ### Exceute load_inputs
    observeEvent(input$act_indicator_train_pwr, {
        
        print('')
        print('==================== ++++++++++++++++++ ====================')
        print('==================== START TRAINING PWR ====================')
        print('==================== ++++++++++++++++++ ====================')
        print('')
        print('-------------------- LOAD INPUTS START  --------------------')
        
        LST_PARAMS = react$params_input_pwr
        
        if (is.null(react$dt_spot_manual_gas)) {
            showNotification("Missing manual gas spot data. Training cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if (is.null(react$dt_spot_manual_pwr)) {
            showNotification("Missing manual pwr spot data. Training cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if(!is.null(react$dt_spot_manual) & !is.null(react$dt_spot_manual_gas) & input$in_source_train == 'Excel') {
        
            if(!is.null(react$dt_spot_manual)) {
                list_inputs = HPFC::load_inputs(params = LST_PARAMS,
                                                manual_data = react$dt_spot_manual,
                                                reuters_key = NULL)
            }
            
        } else {
            
            list_inputs = HPFC::load_inputs(params = LST_PARAMS, manual_data = NULL,
                                            reuters_key = PLEASE_INSERT_REUTERS_KEY,
                                            last_run_path = file.path('HPFC', 'last', 'history')
            )
        }
        
        
        ## ARCHIVE
        last_path = file.path('HPFC', 'last', 'history', input$in_select_PWR_indicator)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
        fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
        
        if(nchar(input$in_new_sim_name) > 0) {
            
            last_path = file.path('HPFC', 'archive', 'history', input$in_select_PWR_indicator, input$in_new_sim_name)
            if (!dir.exists(last_path)) {
                dir.create(last_path, recursive = TRUE)
            }
            
            fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
            fwrite(list_inputs$ENV_SPOT$history_pwr, file.path(last_path, paste0('history_pwr.csv')))
        }
        
        list_inputs_field_pwr(list_inputs)
        
        print('-------------------- LOAD INPUTS END   --------------------')
        
    })
    
    
    
    
    ## Prepare Curves -----------------------
    prepare_gas_field_pwr = reactiveVal(NULL)
    prepare_pwr_field = reactiveVal(NULL)
    
    observe({
        
        req(react$list_inputs_field_pwr)
        
        print('============= +++++++++++++ ====================')
        print('------------- PREPARE START --------------------')
        
        list_inputs = react$list_inputs_field_pwr
        ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
        ENV_MODELS_PWR = prepare_pwr(list_inputs = list_inputs)
        
        prepare_gas_field_pwr(ENV_MODELS_GAS)
        prepare_pwr_field(ENV_MODELS_PWR)
        
        print('------------- PREPARE END ----------------------')
        
    })
    
    
    ## Train Models -----------------------
    models_gas_field_pwr = reactiveVal(NULL)
    models_pwr_field = reactiveVal(NULL)
    
    observe({
        
        req(react$prepare_gas_field_pwr)
        req(react$prepare_pwr_field)
        
        print('============= +++++++++++ ====================')
        print('------------- TRAIN START --------------------')
        
        ENV_MODELS_GAS = react$prepare_gas_field_pwr
        ENV_MODELS_PWR = react$prepare_pwr_field
        
        ENV_MODELS_GAS$dt_lt_param_gasdep = 
            train_lt_gas(
                gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
            )
        
        ENV_MODELS_PWR$dt_lt_param_pwr = 
            train_lt_pwr(
                pwr_data = ENV_MODELS_PWR$dt_lt_param_pwr,
                ric_pwr = unique(ENV_MODELS_PWR$dt_pwr$RIC),
                pwr_holidays = ENV_MODELS_PWR$calendar_holidays_pwr,
                gas_history = ENV_MODELS_PWR$gas_history
            )
        
        ENV_MODELS_PWR$lst_hr_param_pwr = 
            train_st_pwr(
                pwr_data = ENV_MODELS_PWR$dt_hr_param_pwr,
                gas_history = ENV_MODELS_PWR$gas_history
            )
        
        ## ARCHIVE
        
        ### LAST
        last_path = file.path('HPFC', 'last', 'models', input$in_select_PWR_indicator)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
        saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
        saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
        
        if(nchar(input$in_new_sim_name) > 0) {
            
            last_path = file.path('HPFC', 'archive', 'models', input$in_select_PWR_indicator, input$in_new_sim_name)
            if (!dir.exists(last_path)) {
                dir.create(last_path, recursive = TRUE)
            }
            
            saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
            saveRDS(ENV_MODELS_PWR$dt_lt_param_pwr, file.path(last_path, paste0('model_pwr_lt.rds')))
            saveRDS(ENV_MODELS_PWR$lst_hr_param_pwr, file.path(last_path, paste0('model_pwr_st.rds')))
        }
        
        models_gas_field_pwr(ENV_MODELS_GAS)
        models_pwr_field(ENV_MODELS_PWR)
        
        print('------------- TRAIN END  --------------------')
        
    })
    
    
    # D. SINGLE - TRAIN - GAS ------------------------------------------
    
    ## Inputs -----------------------
    params_input_gas = reactiveVal(NULL)
    list_inputs_field_gas = reactiveVal(NULL)
    
    ### Prepare inputs params
    observe({
        params_list = list(
            model_type = 'GAS',
            selected_pwr_code = input$in_select_PWR_indicator,
            selected_gas_code = input$in_select_GAS_indicator,
            dependent_gas_code = input$in_select_PWR_indicator,
            history_start = input$in_select_history[1],
            history_end = input$in_select_history[2],
            forecast_start = input$in_select_horizon[1],
            forecast_end = input$in_select_horizon[2],
            model_source = 'TRAIN',
            data_source = input$in_source_train,
            forecast_source = 'FWD',
            sim_name = 'NO',
            archive = 'NO'
        )
        
        params_input_gas(params_list)
        
    })
    
    ### Exceute load_inputs
    observeEvent(input$act_indicator_train_gas, {
        
        print('')
        print('==================== ++++++++++++++++++ ====================')
        print('==================== START TRAINING GAS ====================')
        print('==================== ++++++++++++++++++ ====================')
        print('')
        print('-------------------- LOAD INPUTS START  --------------------')
        
        LST_PARAMS = react$params_input_gas
        
        if(input$in_source_train == 'Excel') {
            req(!is.null(react$dt_spot_manual_gas))
            if(!is.null(react$dt_spot_manual_gas)) {
                list_inputs = HPFC::load_inputs(params = LST_PARAMS, 
                                                manual_data = react$dt_spot_manual_gas, 
                                                reuters_key = NULL
                )
            } else {
                showNotification("Missing manual gas spot data. Training cannot proceed.", type = "error", duration = NULL)
                return()
            }
        } else {
            
            list_inputs = HPFC::load_inputs(params = LST_PARAMS, 
                                            manual_data = NULL, 
                                            reuters_key = PLEASE_INSERT_REUTERS_KEY,
                                            last_run_path = file.path('HPFC', 'last', 'history'))        
            
        }
        
        ## ARCHIVE
        last_path = file.path('HPFC', 'last', 'history', input$in_select_GAS_indicator)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
        
        if(nchar(input$in_new_sim_name) > 0) {
            last_path = file.path('HPFC', 'archive', 'history', input$in_select_GAS_indicator, input$in_new_sim_name)
            if (!dir.exists(last_path)) {
                dir.create(last_path, recursive = TRUE)
            }
            
            fwrite(list_inputs$ENV_SPOT$history_gas, file.path(last_path, paste0('history_gas.csv')))
        }
        
        list_inputs_field_gas(list_inputs)
        
        print('-------------------- LOAD INPUTS END   --------------------')
        
    })
    
    
    
    
    ## Prepare Curves -----------------------
    prepare_gas_field_gas = reactiveVal(NULL)
    
    observe({
        
        req(react$list_inputs_field_gas)
        
        print('=================== +++++++++++++ ====================')
        print('------------------- PREPARE START --------------------')
        
        list_inputs = react$list_inputs_field_gas
        ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
        
        prepare_gas_field_gas(ENV_MODELS_GAS)
        
        print('------------------- PREPARE END ----------------------')
        
    })
    
    
    ## Train Models -----------------------
    models_gas_field_gas = reactiveVal(NULL)
    
    observe({
        
        req(react$prepare_gas_field_gas)
        
        print('=================== +++++++++++ ====================')
        print('------------------- TRAIN START --------------------')
        
        ENV_MODELS_GAS = react$prepare_gas_field_gas
        
        ENV_MODELS_GAS$dt_lt_param_gasdep = 
            train_lt_gas(
                gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
            )
        
        
        ## ARCHIVE
        
        ### LAST
        last_path = file.path('HPFC', 'last', 'models', input$in_select_GAS_indicator)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
        
        if(nchar(input$in_new_sim_name) > 0) {
            
            last_path = file.path('HPFC', 'archive', 'models', input$in_select_GAS_indicator, input$in_new_sim_name)
            if (!dir.exists(last_path)) {
                dir.create(last_path, recursive = TRUE)
            }
            
            saveRDS(ENV_MODELS_GAS$dt_lt_param_gasdep, file.path(last_path, paste0('model_gas_lt.rds')))
        }
        
        models_gas_field_gas(ENV_MODELS_GAS)
        
        print('------------------- TRAIN END --------------------')
        
    })
    
    
    
    
    # E. MULTIPLE - FORECAST - PWR ------------------------------------------
    
    list_pwr_for_mult = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_pwr_mult, {
        
            
            if (is.null(react$dt_forecast_manual_pwr) & is.null(react$dt_forecast_manual_gas)) {
                showNotification("Missing manual gas and/pwr fwds data. Forecasting cannot proceed.", type = "error", duration = NULL)
                return()
            }
            
        if(!is.null(react$dt_forecast_manual_pwr) & !is.null(react$dt_forecast_manual_gas) & input$in_source_forecast == 'Excel') {
            
            showNotification("Using manual data", type = "message")
            
            
            list_pwr = lapply(input$in_select_PWR_indicator_for_mult, function(x, list_inputs_fwd, start_date, end_date, shiny_run, shiny_sim) {
                
                tryCatch({
                    
                    list_inputs_fwd = prepare_fwd(
                        fwd_pwr_code = x,
                        fwd_gas_code = 'TTF',
                        start_date = input$in_select_horizon[1],
                        end_date = input$in_select_horizon[2],
                        model_type = 'PWR',
                        forecast_source = 'FWD',
                        archive = 'NO',
                        manual_pwr = react$dt_forecast_manual_pwr,
                        manual_gas = react$dt_forecast_manual_gas
                    )
                    
                    ## Forecast Parameters 
                    
                    print('=================== ++++++++++++++++++++++++++ =============')
                    print('------------------- FORECAST PARAMS PREP START -------------')
                    
                    FWD = list_inputs_fwd$ENV_FWD
                    
                    if (shiny_run == 'Last') {
                        last_path_models = file.path('HPFC', 'last', 'models', x)
                        last_path_history = file.path('HPFC', 'last', 'history', x)
                        
                        ENV_MODELS_GAS = list()
                        ENV_MODELS_PWR = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
                        ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                        list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
                    }
                    
                    if (shiny_run == 'Sim') {
                        last_path_models = file.path('HPFC', 'archive', 'models', x, shiny_sim)
                        last_path_history = file.path('HPFC', 'archive', 'history', x, shiny_sim)
                        
                        ENV_MODELS_GAS = list()
                        ENV_MODELS_PWR = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
                        ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                        list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
                    }        
                    
                    LST_FOR = list(
                        model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
                        model_lt_pwr = copy(ENV_MODELS_PWR$dt_lt_param_pwr),
                        model_st_pwr = copy(ENV_MODELS_PWR$lst_hr_param_pwr),
                        dt_fwds = copy(FWD$dt_fwds),
                        saved_history_gas = copy(list_inputs$history_gas),
                        saved_history_pwr = copy(list_inputs$history_pwr),
                        ric_spot_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$spot_GAS_code,
                        ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$products_GAS_code,
                        ric_spot_pwr = eikondata::pwr_products_full[countries %in% unique(x)]$spot_PWR_code,
                        ric_fwd_pwr = eikondata::pwr_products_full[countries %in% unique(x)]$products_PWR_code,
                        calendar_forecast = FWD$calendar_future,
                        start_date = start_date,
                        end_date = end_date,
                        last_date = FWD$last_date
                    ) 
                    
                    print('------------------- FORECAST PARAMS PREP END -----------------')
                    
                    print('')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('==================== START FORECASTING PWR ====================')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('')
                    print('------------- FORECAST START -------------')
                    
                    ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
                    ENV_FOR_PWR = forecast_pwr(input_forecast = LST_FOR, gas_forecast = ENV_FOR_GAS)
                    
                    dt_pwr_for = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast, RIC, season, peak, value_gas, value_bl = spot_forward_month_BL)]
                    dt_pwr_obs = LST_FOR$saved_history_pwr[year(date) %in% unique(year(dt_pwr_for$date)) & RIC == unique(dt_pwr_for$RIC)][, .(date, hour, spot = value, RIC)]
                    dt_pwr = merge(dt_pwr_for, dt_pwr_obs, by = c('date', 'hour', 'RIC'), all = TRUE)
                    
                    setcolorder(dt_pwr, c('date', 'hour', 'season', 'peak', 'RIC', 'spot', 'forecast', 'value_bl', 'value_gas'))
                    setorder(dt_pwr, date, hour)
                    
                    ## ARCHIVE 
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'output', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.csv')))
                    
                    if(!is.null(shiny_sim)) {
                        if(nchar(shiny_sim) > 0) {
                            
                            last_path = file.path('HPFC', 'archive', 'output', x, shiny_sim)
                            if (!dir.exists(last_path)) {
                                dir.create(last_path, recursive = TRUE)
                            }
                            
                            fwrite(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.csv')))
                        }
                    }
                    
                    print('------------- FORECAST END -------------')        
                    print('')
                    print('==================== +++++++++++++++++++ ====================')
                    print('==================== END FORECASTING PWR ====================')
                    print('==================== +++++++++++++++++++ ====================')
                    print('')
                    
                    # Visualize 
                    
                    DTS = copy(dt_pwr)
                    dt_pwr_lg = melt(DTS, id.vars = c('date', 'hour', 'season', 'peak', 'RIC'), variable.name = 'type', value.name = 'value')
                    dt_pwr_lg[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
                    rics = unique(dt_pwr_lg$RIC) 
                    setorder(dt_pwr_lg, datetime, RIC)
                    
                    PLOT_Y = 
                        dt_pwr_lg %>% 
                        group_by(type) %>% 
                        e_charts(datetime) %>% 
                        e_line(value, smooth = TRUE, symbol='none') %>% 
                        e_title(text = paste("Hourly Forecast Prices for", rics)) %>%
                        e_tooltip(trigger = "axis") %>% 
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_datazoom(start = 0) %>% 
                        e_theme('westeros')
                    
                    DT_Y = dt_pwr
                    
                    LIST_Y = list(PLOT_Y, DT_Y)
                    names(LIST_Y) = c('PLOT_Y', 'DT_Y')
                    
                    return(LIST_Y)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            list_inputs_fwd = list_inputs_fwd,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            shiny_run = input$in_source_run,
            shiny_sim = input$sim_name
            )
            
        } else {
            
            list_pwr = lapply(input$in_select_PWR_indicator_for_mult, function(x, list_inputs_fwd, start_date, end_date, shiny_run, shiny_sim) {
                
                tryCatch({
                    
                    list_inputs_fwd = prepare_fwd(
                        fwd_pwr_code = x,
                        fwd_gas_code = 'TTF',
                        start_date = input$in_select_horizon[1],
                        end_date = input$in_select_horizon[2],
                        model_type = 'PWR',
                        forecast_source = 'FWD',
                        archive = 'NO',
                        manual_pwr = NULL,
                        manual_gas = NULL,
                        reuters_key = PLEASE_INSERT_REUTERS_KEY
                    ) 
                    
                    ## Forecast Parameters 
                    
                    print('=================== ++++++++++++++++++++++++++ =============')
                    print('------------------- FORECAST PARAMS PREP START -------------')
                    
                    FWD = list_inputs_fwd$ENV_FWD
                    
                    if (shiny_run == 'Last') {
                        last_path_models = file.path('HPFC', 'last', 'models', x)
                        last_path_history = file.path('HPFC', 'last', 'history', x)
                        
                        ENV_MODELS_GAS = list()
                        ENV_MODELS_PWR = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
                        ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                        list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
                    }
                    
                    if (shiny_run == 'Sim') {
                        last_path_models = file.path('HPFC', 'archive', 'models', x, shiny_sim)
                        last_path_history = file.path('HPFC', 'archive', 'history', x, shiny_sim)
                        
                        ENV_MODELS_GAS = list()
                        ENV_MODELS_PWR = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
                        ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                        list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
                    }        
                    
                    LST_FOR = list(
                        model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
                        model_lt_pwr = copy(ENV_MODELS_PWR$dt_lt_param_pwr),
                        model_st_pwr = copy(ENV_MODELS_PWR$lst_hr_param_pwr),
                        dt_fwds = copy(FWD$dt_fwds),
                        saved_history_gas = copy(list_inputs$history_gas),
                        saved_history_pwr = copy(list_inputs$history_pwr),
                        ric_spot_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$spot_GAS_code,
                        ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$products_GAS_code,
                        ric_spot_pwr = eikondata::pwr_products_full[countries %in% unique(x)]$spot_PWR_code,
                        ric_fwd_pwr = eikondata::pwr_products_full[countries %in% unique(x)]$products_PWR_code,
                        calendar_forecast = FWD$calendar_future,
                        start_date = start_date,
                        end_date = end_date,
                        last_date = FWD$last_date
                    ) 
                    
                    print('------------------- FORECAST PARAMS PREP END -----------------')
                    
                    print('')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('==================== START FORECASTING PWR ====================')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('')
                    print('------------- FORECAST START -------------')
                    
                    ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
                    ENV_FOR_PWR = forecast_pwr(input_forecast = LST_FOR, gas_forecast = ENV_FOR_GAS)
                    dt_pwr_for = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast, RIC, season, peak, value_gas, value_bl = spot_forward_month_BL)]
                    dt_pwr_obs = LST_FOR$saved_history_pwr[year(date) %in% unique(year(dt_pwr_for$date)) & RIC == unique(dt_pwr_for$RIC)][, .(date, hour, spot = value, RIC)]
                    dt_pwr = merge(dt_pwr_for, dt_pwr_obs, by = c('date', 'hour', 'RIC'), all = TRUE)
                    
                    setcolorder(dt_pwr, c('date', 'hour', 'season', 'peak', 'RIC', 'spot', 'forecast', 'value_bl', 'value_gas'))
                    setorder(dt_pwr, date, hour)
                    
                    ## ARCHIVE 
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'output', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.csv')))
                    if(!is.null(shiny_sim)) {
                        if(nchar(shiny_sim) > 0) {
                            
                            last_path = file.path('HPFC', 'archive', 'output', x, shiny_sim)
                            if (!dir.exists(last_path)) {
                                dir.create(last_path, recursive = TRUE)
                            }
                            
                            fwrite(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.csv')))
                        }
                    }
                    
                    print('------------- FORECAST END -------------')        
                    print('')
                    print('==================== +++++++++++++++++++ ====================')
                    print('==================== END FORECASTING PWR ====================')
                    print('==================== +++++++++++++++++++ ====================')
                    print('')
                    
                    # Visualize 
                    
                    DTS = copy(dt_pwr)
                    dt_pwr_lg = melt(DTS, id.vars = c('date', 'hour', 'season', 'peak', 'RIC'), variable.name = 'type', value.name = 'value')
                    dt_pwr_lg[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
                    rics = unique(dt_pwr_lg$RIC) 
                    setorder(dt_pwr_lg, datetime, RIC)
                    
                    PLOT_Y = 
                    dt_pwr_lg %>% 
                        group_by(type) %>% 
                        e_charts(datetime) %>% 
                        e_line(value, smooth = TRUE, symbol='none') %>% 
                        e_title(text = paste("Hourly Forecast Prices for", rics)) %>%
                        e_tooltip(trigger = "axis") %>% 
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_datazoom(start = 0) %>% 
                        e_theme('westeros')
                    
                    DT_Y = dt_pwr
                    
                    LIST_Y = list(PLOT_Y, DT_Y)
                    names(LIST_Y) = c('PLOT_Y', 'DT_Y')
                    
                    return(LIST_Y)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            list_inputs_fwd = list_inputs_fwd,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            shiny_run = input$in_source_run,
            shiny_sim = input$sim_name
            )
            
        }
        
        names(list_pwr) = input$in_select_PWR_indicator_for_mult
        valid_names = names(list_pwr)[!sapply(list_pwr, is.null)]
        
        updateSelectInput(
            session = session,
            inputId = "in_select_pwrplot_mult_for",
            choices = valid_names,
            selected = if (length(valid_names) > 0) valid_names[1] else NULL
        )
        
        list_pwr_for_mult(list_pwr[valid_names])
        
    })
    
    
    
    # F. MULTIPLE - FORECAST - GAS ------------------------------------------
    
    list_gas_for_mult = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_gas_mult, {
        
        if (is.null(react$dt_forecast_manual_gas)) {
            showNotification("Missing manual gas and/pwr fwds data. Forecasting cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if(!is.null(react$dt_forecast_manual_gas) & input$in_source_forecast == 'Excel') {
            
            showNotification("Using manual data", type = "message")
            
            list_gas = lapply(input$in_select_GAS_indicator_for_mult, function(x, list_inputs_fwd, start_date, end_date, shiny_run, shiny_sim) {
                
                tryCatch({
                    
                    list_inputs_fwd = prepare_fwd(
                        fwd_gas_code = x,
                        start_date = input$in_select_horizon[1],
                        end_date = input$in_select_horizon[2],
                        model_type = 'GAS',
                        forecast_source = 'FWD',
                        archive = 'NO',
                        manual_pwr = NULL,
                        manual_gas = react$dt_forecast_manual_gas
                    )
                    
                    ## Forecast Parameters 
                    
                    print('=================== ++++++++++++++++++++++++++ =============')
                    print('------------------- FORECAST PARAMS PREP START -------------')
                    
                    FWD = list_inputs_fwd$ENV_FWD
                    
                    if (shiny_run == 'Last') {
                        last_path_models = file.path('HPFC', 'last', 'models', x)
                        last_path_history = file.path('HPFC', 'last', 'history', x)
                        
                        ENV_MODELS_GAS = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                    }
                    
                    if (shiny_run == 'Sim') {
                        last_path_models = file.path('HPFC', 'archive', 'models', x, shiny_sim)
                        last_path_history = file.path('HPFC', 'archive', 'history', x, shiny_sim)
                        
                        ENV_MODELS_GAS = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                    }        
                    
                    LST_FOR = list(
                        model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
                        dt_fwds = copy(FWD$dt_fwds),
                        saved_history_gas = copy(list_inputs$history_gas),
                        ric_spot_gas = eikondata::gas_products_full[products_GAS %in% unique(x)]$spot_GAS_code,
                        ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% unique(x)]$products_GAS_code,
                        calendar_forecast = FWD$calendar_future,
                        start_date = start_date,
                        end_date = end_date,
                        last_date = FWD$last_date
                    ) 
                    
                    print('------------------- FORECAST PARAMS PREP END -----------------')
                    
                    print('')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('==================== START FORECASTING GAS ====================')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('')
                    print('------------- FORECAST START -------------')
                    
                    ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
                    
                    dt_gas_for = ENV_FOR_GAS[, .(date, forecast = L_e_u_adj, RIC, value_gas = spot_forward_month_BL)]
                    dt_gas_obs = LST_FOR$saved_history_gas[year(date) %in% unique(year(dt_gas_for$date)) & RIC == unique(dt_gas_for$RIC)][, .(date, spot = value, RIC)]
                    dt_gas = merge(dt_gas_for, dt_gas_obs, by = c('date', 'RIC'), all = TRUE)
                    
                    setcolorder(dt_gas, c('date', 'RIC', 'spot', 'forecast', 'value_gas'))
                    setorder(dt_gas, date)
                    
                    ## ARCHIVE 
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'output', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.csv')))
                    
                    if(!is.null(shiny_sim)) {
                        if(nchar(shiny_sim) > 0) {
                            
                            last_path = file.path('HPFC', 'archive', 'output', x, shiny_sim)
                            if (!dir.exists(last_path)) {
                                dir.create(last_path, recursive = TRUE)
                            }
                            
                            fwrite(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.csv')))
                        }
                    }
                    
                    print('------------- FORECAST END -------------')        
                    print('')
                    print('==================== +++++++++++++++++++ ====================')
                    print('==================== END FORECASTING GAS ====================')
                    print('==================== +++++++++++++++++++ ====================')
                    print('')
                    
                    # Visualize 
                    
                    DTS = copy(dt_gas)
                    dt_gas_lg = melt(DTS, id.vars = c('date', 'RIC'), variable.name = 'type', value.name = 'value')
                    rics = unique(dt_gas_lg$RIC) 
                    setorder(dt_gas_lg, date, RIC)
                    
                    PLOT_Y = 
                        dt_gas_lg %>% 
                        group_by(type) %>% 
                        e_charts(date) %>% 
                        e_line(value, smooth = TRUE, symbol='none') %>% 
                        e_title(text = paste("Daily Forecast Prices for", rics)) %>%
                        e_tooltip(trigger = "axis") %>% 
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_datazoom(start = 0) %>% 
                        e_theme('westeros')
                    
                    DT_Y = dt_gas
                    
                    LIST_Y = list(PLOT_Y, DT_Y)
                    names(LIST_Y) = c('PLOT_Y', 'DT_Y')
                    
                    return(LIST_Y)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            list_inputs_fwd = list_inputs_fwd,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            shiny_run = input$in_source_run,
            shiny_sim = input$sim_name
            )
            
        } else {
            
            list_gas = lapply(input$in_select_GAS_indicator_for_mult, function(x, list_inputs_fwd, start_date, end_date, shiny_run, shiny_sim) {
                
                tryCatch({
                    
                    list_inputs_fwd = prepare_fwd(
                        fwd_gas_code = x,
                        start_date = input$in_select_horizon[1],
                        end_date = input$in_select_horizon[2],
                        model_type = 'GAS',
                        forecast_source = 'FWD',
                        archive = 'NO',
                        manual_pwr = NULL,
                        manual_gas = NULL,
                        reuters_key = PLEASE_INSERT_REUTERS_KEY
                    ) 
                    
                    ## Forecast Parameters 
                    
                    print('=================== ++++++++++++++++++++++++++ =============')
                    print('------------------- FORECAST PARAMS PREP START -------------')
                    
                    FWD = list_inputs_fwd$ENV_FWD
                    
                    if (shiny_run == 'Last') {
                        last_path_models = file.path('HPFC', 'last', 'models', x)
                        last_path_history = file.path('HPFC', 'last', 'history', x)
                        
                        ENV_MODELS_GAS = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                    }
                    
                    if (shiny_run == 'Sim') {
                        last_path_models = file.path('HPFC', 'archive', 'models', x, shiny_sim)
                        last_path_history = file.path('HPFC', 'archive', 'history', x, shiny_sim)
                        
                        ENV_MODELS_GAS = list()
                        list_inputs = list()
                        ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
                        list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
                    }        
                    
                    LST_FOR = list(
                        model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
                        dt_fwds = copy(FWD$dt_fwds),
                        saved_history_gas = copy(list_inputs$history_gas),
                        ric_spot_gas = eikondata::gas_products_full[products_GAS %in% unique(x)]$spot_GAS_code,
                        ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% unique(x)]$products_GAS_code,
                        calendar_forecast = FWD$calendar_future,
                        start_date = start_date,
                        end_date = end_date,
                        last_date = FWD$last_date
                    ) 
                    
                    print('------------------- FORECAST PARAMS PREP END -----------------')
                    
                    print('')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('==================== START FORECASTING GAS ====================')
                    print('==================== +++++++++++++++++++++ ====================')
                    print('')
                    print('------------- FORECAST START -------------')
                    
                    ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
                    
                    dt_gas_for = ENV_FOR_GAS[, .(date, forecast = L_e_u_adj, RIC, value_gas = spot_forward_month_BL)]
                    dt_gas_obs = LST_FOR$saved_history_gas[year(date) %in% unique(year(dt_gas_for$date)) & RIC == unique(dt_gas_for$RIC)][, .(date, spot = value, RIC)]
                    dt_gas = merge(dt_gas_for, dt_gas_obs, by = c('date', 'RIC'), all = TRUE)
                    
                    setcolorder(dt_gas, c('date', 'RIC', 'spot', 'forecast', 'value_gas'))
                    setorder(dt_gas, date)
                    
                    ## ARCHIVE 
                    
                    ### LAST
                    last_path = file.path('HPFC', 'last', 'output', x)
                    if (!dir.exists(last_path)) {
                        dir.create(last_path, recursive = TRUE)
                    }
                    
                    fwrite(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.csv')))
                    if(!is.null(shiny_sim)) {
                        if(nchar(shiny_sim) > 0) {
                            
                            last_path = file.path('HPFC', 'archive', 'output', x, shiny_sim)
                            if (!dir.exists(last_path)) {
                                dir.create(last_path, recursive = TRUE)
                            }
                            
                            fwrite(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.csv')))
                        }
                    }
                    
                    print('------------- FORECAST END -------------')        
                    print('')
                    print('==================== +++++++++++++++++++ ====================')
                    print('==================== END FORECASTING GAS ====================')
                    print('==================== +++++++++++++++++++ ====================')
                    print('')
                    
                    # Visualize 
                    
                    DTS = copy(dt_gas)
                    dt_gas_lg = melt(DTS, id.vars = c('date', 'RIC'), variable.name = 'type', value.name = 'value')
                    rics = unique(dt_gas_lg$RIC) 
                    setorder(dt_gas_lg, date, RIC)
                    
                    PLOT_Y = 
                        dt_gas_lg %>% 
                        group_by(type) %>% 
                        e_charts(date) %>% 
                        e_line(value, smooth = TRUE, symbol='none') %>% 
                        e_title(text = paste("Daily Forecast Prices for", rics)) %>%
                        e_tooltip(trigger = "axis") %>% 
                        e_toolbox_feature(feature = "saveAsImage") %>%
                        e_toolbox_feature(feature = "dataZoom") %>%
                        e_toolbox_feature(feature = "dataView") %>%
                        e_toolbox_feature(feature = "restore") %>%
                        e_datazoom(start = 0) %>% 
                        e_theme('westeros')
                
                DT_Y = dt_gas
                
                LIST_Y = list(PLOT_Y, DT_Y)
                names(LIST_Y) = c('PLOT_Y', 'DT_Y')
                
                return(LIST_Y)
                    
                }, error = function(e) {
                    msg = paste0("Error while training ", x, ": ", e$message)
                    showNotification(msg, type = "error", duration = NULL)
                    message(msg)
                    return(NULL)
                })
            },
            list_inputs_fwd = list_inputs_fwd,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            shiny_run = input$in_source_run,
            shiny_sim = input$sim_name
            )
            
        }
        
        names(list_gas) = input$in_select_GAS_indicator_for_mult
        valid_names = names(list_gas)[!sapply(list_gas, is.null)]
        
        
        updateSelectInput(
            session = session,
            inputId = "in_select_gasplot_mult_for",
            choices = valid_names,
            selected = if (length(valid_names) > 0) valid_names[1] else NULL
        )
        
        list_gas_for_mult(list_gas[valid_names])
        
    })
    
    
    
    # G. SINGLE - FORECAST - PWR ------------------------------------------
    
    object_with_forecast_data_pwr = reactiveVal(NULL) 
    
    # PREPARE FWD
    
    fwd_pwr_field = reactiveVal(NULL)
    fwd_gas_field = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_pwr, {
        
        if (is.null(react$dt_forecast_manual_pwr) & is.null(react$dt_forecast_manual_gas)) {
            showNotification("Missing manual gas and/pwr fwds data. Forecasting cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if(!is.null(react$dt_forecast_manual_pwr) & !is.null(react$dt_forecast_manual_gas) & input$in_source_forecast == 'Excel') {
        
            list_inputs_fwd = prepare_fwd(
                fwd_pwr_code = input$in_select_PWR_indicator_for,
                fwd_gas_code = 'TTF',
                start_date = input$in_select_horizon[1],
                end_date = input$in_select_horizon[2],
                model_type = 'PWR',
                forecast_source = 'FWD',
                archive = 'NO',
                manual_pwr = react$dt_forecast_manual_pwr,
                manual_gas = react$dt_forecast_manual_gas
            )
            
            showNotification("Using manual data", type = "message")
            
        } else {
            list_inputs_fwd = prepare_fwd(
                fwd_pwr_code = input$in_select_PWR_indicator_for,
                fwd_gas_code = 'TTF',
                start_date = input$in_select_horizon[1],
                end_date = input$in_select_horizon[2],
                model_type = 'PWR',
                forecast_source = 'FWD',
                archive = 'NO',
                manual_pwr = NULL,
                manual_gas = NULL,
                reuters_key = PLEASE_INSERT_REUTERS_KEY
            )            
            
        }
        
        fwd_pwr_field(list_inputs_fwd)
        
    })
    
    
    ## Forecast Parameters
    forecast_params_field_pwr = reactiveVal(NULL)
    forecast_params_table_pwr = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_pwr, {
        
        req(react$fwd_pwr_field)
        
        print('=================== ++++++++++++++++++++++++++ =============')
        print('------------------- FORECAST PARAMS PREP START -------------')
        
        FWD = react$fwd_pwr_field$ENV_FWD
        
        if (input$in_source_run == 'Last') {
            last_path_models = file.path('HPFC', 'last', 'models', input$in_select_PWR_indicator_for)
            last_path_history = file.path('HPFC', 'last', 'history', input$in_select_PWR_indicator_for)
            
            ENV_MODELS_GAS = list()
            ENV_MODELS_PWR = list()
            list_inputs = list()
            ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
            ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
            ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
            list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
            list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
        }
        
        if (input$in_source_run == 'Sim') {
            last_path_models = file.path('HPFC', 'archive', 'models', input$in_select_PWR_indicator_for, input$sim_name)
            last_path_history = file.path('HPFC', 'archive', 'history', input$in_select_PWR_indicator_for, input$sim_name)
            
            ENV_MODELS_GAS = list()
            ENV_MODELS_PWR = list()
            list_inputs = list()
            ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
            ENV_MODELS_PWR$dt_lt_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_lt.rds'))
            ENV_MODELS_PWR$lst_hr_param_pwr = readRDS(file.path(last_path_models, 'model_pwr_st.rds'))
            list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
            list_inputs$history_pwr = fread(file.path(last_path_history, 'history_pwr.csv'))
        }        
        
        LST_FOR = list(
            model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
            model_lt_pwr = copy(ENV_MODELS_PWR$dt_lt_param_pwr),
            model_st_pwr = copy(ENV_MODELS_PWR$lst_hr_param_pwr),
            dt_fwds = copy(FWD$dt_fwds),
            saved_history_gas = copy(list_inputs$history_gas),
            saved_history_pwr = copy(list_inputs$history_pwr),
            ric_spot_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$spot_GAS_code,
            ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% unique('TTF')]$products_GAS_code,
            ric_spot_pwr = eikondata::pwr_products_full[countries %in% unique(input$in_select_PWR_indicator_for)]$spot_PWR_code,
            ric_fwd_pwr = eikondata::pwr_products_full[countries %in% unique(input$in_select_PWR_indicator_for)]$products_PWR_code,
            calendar_forecast = FWD$calendar_future,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            last_date = FWD$last_date
        ) 
        
        dt_recap =
            data.table(
                params = names(LST_FOR),
                value = sapply(LST_FOR, function(x) {
                    if (is.data.table(x)) {
                        sprintf("data.table [%d x %d]", nrow(x), ncol(x))
                    } else if (is.list(x)) {
                        sprintf("list [%d elements]", length(x))
                    } else if (is.character(x) || is.numeric(x) || is.logical(x)) {
                        paste0(x, collapse = ", ")
                    } else {
                        paste0(x, collapse = ", ")
                    }
                })
            )        
        
        print('------------------- FORECAST PARAMS PREP END -----------------')
        
        forecast_params_field_pwr(LST_FOR)
        forecast_params_table_pwr(dt_recap)
        
        print('')
        print('==================== ++++++++++++++++ ====================')
        print('==================== END TRAINING PWR ====================')
        print('==================== ++++++++++++++++ ====================')
        print('')
    })
    
    
    observe({
        
        req(react$forecast_params_field_pwr)
        print('')
        print('==================== +++++++++++++++++++++ ====================')
        print('==================== START FORECASTING PWR ====================')
        print('==================== +++++++++++++++++++++ ====================')
        print('')
        print('------------- FORECAST START -------------')
        
        ENV_FOR_GAS = forecast_gas(input_forecast = react$forecast_params_field_pwr)
        ENV_FOR_PWR = forecast_pwr(input_forecast = react$forecast_params_field_pwr, gas_forecast = ENV_FOR_GAS)
        
        dt_pwr_for = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast, RIC, season, peak, value_gas, value_bl = spot_forward_month_BL)]
        dt_pwr_obs = react$forecast_params_field_pwr$saved_history_pwr[year(date) %in% unique(year(dt_pwr_for$date)) & RIC == unique(dt_pwr_for$RIC)][, .(date, hour, spot = value, RIC)]
        dt_pwr = merge(dt_pwr_for, dt_pwr_obs, by = c('date', 'hour', 'RIC'), all = TRUE)
        
        setcolorder(dt_pwr, c('date', 'hour', 'season', 'peak', 'RIC', 'spot', 'forecast', 'value_bl', 'value_gas'))
        setorder(dt_pwr, date, hour)
        
        ## ARCHIVE
        
        ### LAST
        last_path = file.path('HPFC', 'last', 'output', input$in_select_PWR_indicator_for)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        saveRDS(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.rds')))
        
        if(!is.null(input$sim_name)) {
            if(nchar(input$sim_name) > 0) {
                
                last_path = file.path('HPFC', 'archive', 'output', input$in_select_PWR_indicator_for, input$sim_name)
                if (!dir.exists(last_path)) {
                    dir.create(last_path, recursive = TRUE)
                }
                
                saveRDS(dt_pwr, file.path(last_path, paste0('forecast_pwr', Sys.Date() ,'.rds')))
            }
        }
        
        object_with_forecast_data_pwr(dt_pwr)
        
        print('------------- FORECAST END -------------')        
        print('')
        print('==================== +++++++++++++++++++ ====================')
        print('==================== END FORECASTING PWR ====================')
        print('==================== +++++++++++++++++++ ====================')
        print('')
        
    })
    
    
    # H. SINGLE - FORECAST - GAS ------------------------------------------
    
    object_with_forecast_data_gas = reactiveVal(NULL) 
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        
        if (is.null(react$dt_forecast_manual_gas)) {
            showNotification("Missing manual gas and/pwr fwds data. Forecasting cannot proceed.", type = "error", duration = NULL)
            return()
        }
        
        if(!is.null(react$dt_forecast_manual_gas) & input$in_source_forecast == 'Excel') {
            
            req(react$dt_forecast_manual_gas)
            list_inputs_fwd = prepare_fwd(
                fwd_gas_code = input$in_select_GAS_indicator_for,
                start_date = input$in_select_horizon[1],
                end_date = input$in_select_horizon[2],
                model_type = 'GAS',
                forecast_source = 'FWD',
                archive = 'NO',
                manual_pwr = NULL,
                manual_gas = react$dt_forecast_manual_gas
            )
            
            showNotification("Using manual data", type = "message")
            
        } else {
            list_inputs_fwd = prepare_fwd(
                fwd_gas_code = input$in_select_GAS_indicator_for,
                start_date = input$in_select_horizon[1],
                end_date = input$in_select_horizon[2],
                model_type = 'GAS',
                forecast_source = 'FWD',
                archive = 'NO',
                manual_pwr = NULL,
                manual_gas = NULL,
                reuters_key = PLEASE_INSERT_REUTERS_KEY
            )
            
        }
        
        fwd_gas_field(list_inputs_fwd)
        
    })    
    
    ## Forecast Parameters
    forecast_params_field_gas = reactiveVal(NULL)
    forecast_params_table_gas = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        req(react$fwd_gas_field)
        
        print('============= ++++++++++++++++++++++++++ =============')
        print('------------- FORECAST PARAMS PREP START -------------')
        
        FWD = react$fwd_gas_field$ENV_FWD
        
        if (input$in_source_run == 'Last') {
            last_path_models = file.path('HPFC', 'last', 'models', input$in_select_GAS_indicator_for)
            last_path_history = file.path('HPFC', 'last', 'history', input$in_select_GAS_indicator_for)
            
            ENV_MODELS_GAS = list()
            list_inputs = list()
            ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
            list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
        }
        
        if (input$in_source_run == 'Sim') {
            last_path_models = file.path('HPFC', 'archive', 'models', input$in_select_GAS_indicator_for, input$sim_name)
            last_path_history = file.path('HPFC', 'archive', 'history', input$in_select_GAS_indicator_for, input$sim_name)
            
            ENV_MODELS_GAS = list()
            list_inputs = list()
            ENV_MODELS_GAS$dt_lt_param_gasdep = readRDS(file.path(last_path_models, 'model_gas_lt.rds'))
            list_inputs$history_gas = fread(file.path(last_path_history, 'history_gas.csv'))
        }
        
        LST_FOR = list(
            model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
            dt_fwds = copy(FWD$dt_fwds),
            saved_history_gas = copy(list_inputs$history_gas),
            ric_spot_gas = eikondata::gas_products_full[products_GAS %in% input$in_select_GAS_indicator_for]$spot_GAS_code,
            ric_fwd_gas = eikondata::gas_products_full[products_GAS %in% input$in_select_GAS_indicator_for]$products_GAS_code,
            calendar_forecast = FWD$calendar_future,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            last_date = FWD$last_date
        ) 
        
        dt_recap =
            data.table(
                params = names(LST_FOR),
                value = sapply(LST_FOR, function(x) {
                    if (is.data.table(x)) {
                        sprintf("data.table [%d x %d]", nrow(x), ncol(x))
                    } else if (is.list(x)) {
                        sprintf("list [%d elements]", length(x))
                    } else if (is.character(x) || is.numeric(x) || is.logical(x)) {
                        paste0(x, collapse = ", ")
                    } else {
                        paste0(x, collapse = ", ")
                    }
                })
            )        
        
        print('------------- FORECAST PARAMS PREP END -----------------')
        
        forecast_params_field_gas(LST_FOR)
        forecast_params_table_gas(dt_recap)
        
        print('')
        print('==================== ++++++++++++++++ ====================')
        print('==================== END TRAINING GAS ====================')
        print('==================== ++++++++++++++++ ====================')
        print('')
    })    
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        req(react$forecast_params_field_gas)
        print('')
        print('==================== +++++++++++++++++++++ ====================')
        print('==================== START FORECASTING GAS ====================')
        print('==================== +++++++++++++++++++++ ====================')
        print('')
        print('------------- FORECAST START -------------')
        
        ENV_FOR_GAS = forecast_gas(input_forecast = react$forecast_params_field_gas)
        
        dt_gas_for = ENV_FOR_GAS[, .(date, forecast = L_e_u_adj, RIC, value_gas = spot_forward_month_BL)]
        dt_gas_obs = react$forecast_params_field_gas$saved_history_gas[year(date) %in% unique(year(dt_gas_for$date)) & RIC == unique(dt_gas_for$RIC)][, .(date, spot = value, RIC)]
        dt_gas = merge(dt_gas_for, dt_gas_obs, by = c('date', 'RIC'), all = TRUE)
        
        setcolorder(dt_gas, c('date', 'RIC', 'spot', 'forecast', 'value_gas'))
        setorder(dt_gas, date)
        
        ## ARCHIVE
        
        ### LAST
        last_path = file.path('HPFC', 'last', 'output', input$in_select_GAS_indicator_for)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        saveRDS(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.rds')))
        
        if(!is.null(input$sim_name)) {
            if(nchar(input$sim_name) > 0) {
                
                last_path = file.path('HPFC', 'archive', 'output', input$in_select_GAS_indicator_for, input$sim_name)
                if (!dir.exists(last_path)) {
                    dir.create(last_path, recursive = TRUE)
                }
                
                saveRDS(dt_gas, file.path(last_path, paste0('forecast_gas', Sys.Date() ,'.rds')))
            }
        }
        
        object_with_forecast_data_gas(dt_gas)
        
        print('------------- FORECAST END -------------')        
        print('')
        print('==================== +++++++++++++++++++ ====================')
        print('==================== END FORECASTING GAS ====================')
        print('==================== +++++++++++++++++++ ====================')
        print('')
        
    })    
    
    
    # I. DOWNLOADS -----------------------
    object_with_train_data_pwr = reactiveVal(NULL)
    
    observe({
        req(react$models_pwr_field)
        object_with_train_data_pwr(react$models_pwr_field)
    })
    
    output$act_train_pwr_download = downloadHandler(
        filename = function() {
            paste0("pwr_models_", input$in_select_PWR_indicator, '-', Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(react$object_with_train_data_pwr, file)
        }
    )
    
    
    object_with_train_data_gas = reactiveVal(NULL)
    
    observe({
        req(react$models_gas_field_gas)
        object_with_train_data_gas(react$models_gas_field_gas)
    })
    
    output$act_train_gas_download = downloadHandler(
        filename = function() {
            paste0("gas_models_", input$in_select_GAS_indicator, '-', Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(react$object_with_train_data_gas, file)
        }
    )    
    
    output$act_forecast_pwr_download = downloadHandler(
        filename = function() {
            paste0("pwr_forecast_", input$in_select_pwr_indicator, '-', Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(react$object_with_forecast_data_pwr, file)
        }
    )   
    
    output$act_forecast_gas_download = downloadHandler(
        filename = function() {
            paste0("gas_forecast_", input$in_select_GAS_indicator, '-', Sys.Date(), ".rds")
        },
        content = function(file) {
            saveRDS(react$object_with_forecast_data_gas, file)
        }
    )   
    
    
    
    # J. VISUALIZE ------------------------------------------
    
    ## TRAIN ------------------------------------
    
    ### POWER
    output$pwr_history_plot_mult = renderEcharts4r({
        req(react$list_pwr_multi)
        react$list_pwr_multi[[input$in_select_pwrplot_mult]]$PLOT_X
    })
    
    output$pwr_history_table_mult = renderDatagrid({
        req(react$list_pwr_multi)
        DT = copy(react$list_pwr_multi[[input$in_select_pwrplot_mult]]$DT_X)
        setorder(DT, date, RIC)
        datagrid(DT,
                 filters = TRUE)
    })
    
    ## RECAP FORECAST PARAMS
    output$forecast_params_table_recap_pwr = renderDatagrid({
        req(react$forecast_params_table_pwr)
        datagrid(react$forecast_params_table_pwr,
                  pagination = 14)
    })
    
    
    # Outputs for the selected history period (for training)
    output$pwr_history_plot = renderEcharts4r({
        
        req(react$list_inputs_field_pwr)
        
        list_inputs = react$list_inputs_field_pwr
        DT = copy(list_inputs$ENV_SPOT$history_pwr)
        DT[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        rics = unique(DT$RIC) 
        setorder(DT, datetime, RIC)
        
        DT %>%
            e_charts(datetime) %>%
            e_line(value, name = rics, symbol = 'none') %>%
            e_title(text = paste("Hourly Spot Prices for", rics)) %>%
            e_x_axis(name = "Datetime") %>%
            e_y_axis(name = "Price") %>%
            e_tooltip(trigger = "axis") %>%
            e_datazoom(type = "slider") %>%
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_theme("westeros") 
        
    })
    
    output$pwr_history_table = renderDatagrid({
        
        req(react$list_inputs_field_pwr)
        
        list_inputs = react$list_inputs_field_pwr
        DT = copy(list_inputs$ENV_SPOT$history_pwr)
        DT[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        setorder(DT, datetime, RIC)
        datagrid(DT,
                  filters = TRUE)
    })
    
    
    
    ### GAS 
    
    output$gas_history_plot_mult = renderEcharts4r({
        req(react$list_gas_multi)
        react$list_gas_multi[[input$in_select_gasplot_mult]]$PLOT_X
    })
    
    output$gas_history_table_mult = renderDatagrid({
        req(react$list_gas_multi)
        DT = copy(react$list_gas_multi[[input$in_select_gasplot_mult]]$DT_X)
        setorder(DT, date, RIC)
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$forecast_params_table_recap_gas = renderDatagrid({
        req(react$forecast_params_table_gas)
        datagrid(react$forecast_params_table_gas,
                 pagination = 14)
    })
    
    output$gas_history_plot = renderEcharts4r({
        
        req(react$list_inputs_field_gas)
        
        list_inputs = react$list_inputs_field_gas
        DT = copy(list_inputs$ENV_SPOT$history_gas)
        rics = unique(DT$RIC) 
        setorder(DT, date, RIC)
        
        DT %>%
            e_charts(date) %>%
            e_line(value, name = rics, symbol = 'none') %>%
            e_title(text = paste("Daily Spot Prices for", rics)) %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price") %>%
            e_tooltip(trigger = "axis") %>%
            e_datazoom(type = "slider") %>%
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_theme("westeros") 
        
    })
    
    output$gas_history_table = renderDatagrid({
        
        req(react$list_inputs_field_gas)
        
        list_inputs = react$list_inputs_field_gas
        DT = copy(list_inputs$ENV_SPOT$history_gas)
        setorder(DT, date, RIC)
        datagrid(DT,
                 filters = TRUE)
    })
    
    
    
    ## FORECAST ------------------------------------
    
    ### POWER
    output$pwr_forecast_plot_mult = renderEcharts4r({
        
        req(react$list_pwr_for_mult)
        react$list_pwr_for_mult[[input$in_select_pwrplot_mult_for]]$PLOT_Y
        
    })
    
    output$pwr_forecast_table_mult = renderDatagrid({
        req(react$list_pwr_for_mult)
        DT = copy(react$list_pwr_for_mult[[input$in_select_pwrplot_mult_for]]$DT_Y)
        setorder(DT, date, RIC)
        datagrid(DT,
                 filters = TRUE)
    })
    
    # Forecast plots for Power and Gas using echarts4r
    output$pwr_forecast_plot = renderEcharts4r({
        
        req(react$object_with_forecast_data_pwr)
        
        dt_pwr_lg = melt(react$object_with_forecast_data_pwr, id.vars = c('date', 'hour', 'season', 'peak', 'RIC'), variable.name = 'type', value.name = 'value')
        dt_pwr_lg[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        rics = unique(dt_pwr_lg$RIC) 
        setorder(dt_pwr_lg, datetime, RIC)
        
        dt_pwr_lg %>% 
            group_by(type) %>% 
            e_charts(datetime) %>% 
            e_line(value, smooth = TRUE, symbol='none') %>% 
            e_title(text = paste("Hourly Forecast Prices for", rics)) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
        
    })
    
    output$pwr_forecast_table = renderDatagrid({
        
        req(react$object_with_forecast_data_pwr)
        
        dt_pwr_lg = melt(react$object_with_forecast_data_pwr, id.vars = c('date', 'hour', 'season', 'peak', 'RIC'), variable.name = 'type', value.name = 'value')
        dt_pwr_lg[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        rics = unique(dt_pwr_lg$RIC) 
        setorder(dt_pwr_lg, datetime, RIC)
        
        datagrid(dt_pwr_lg,
                 filters = TRUE)
    })
    
    
    ### GAS 
    output$gas_forecast_plot_mult = renderEcharts4r({
        
        req(react$list_gas_for_mult)
        react$list_gas_for_mult[[input$in_select_gasplot_mult_for]]$PLOT_Y
        
    })
    
    output$gas_forecast_table_mult = renderDatagrid({
        req(react$list_gas_for_mult)
        DT = copy(react$list_gas_for_mult[[input$in_select_gasplot_mult_for]]$DT_Y)
        setorder(DT, date, RIC)
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$gas_forecast_plot = renderEcharts4r({
        
        req(react$object_with_forecast_data_gas)
        
        dt_gas_lg = melt(react$object_with_forecast_data_gas, id.vars = c('date', 'RIC'), variable.name = 'type', value.name = 'value')
        rics = unique(dt_gas_lg$RIC) 
        setorder(dt_gas_lg, date, RIC)
        
        dt_gas_lg %>% 
            group_by(type) %>% 
            e_charts(date) %>% 
            e_line(value, smooth = TRUE, symbol='none') %>% 
            e_title(text = paste("Daily Forecast Prices for", rics)) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
    })
    
    output$gas_forecast_table = renderDatagrid({
        
        req(react$object_with_forecast_data_gas)
        
        dt_gas_lg = melt(react$object_with_forecast_data_gas, id.vars = c('date', 'RIC'), variable.name = 'type', value.name = 'value')
        rics = unique(dt_gas_lg$RIC) 
        setorder(dt_gas_lg, date, RIC)
        
        datagrid(dt_gas_lg,
                 filters = TRUE)
    })
    
    
    
    # K. LONG TERM CURVE ------------------------------------------------------------------
    observe({
        req(input$in_select_basket)
        
        basket_1_1 = input$in_select_basket[[1]]
        if(length(input$in_select_basket) > 1) {basket_1_2 = input$in_select_basket[[2]]} else {basket_1_2 = 'EMPTY'}
        if(length(input$in_select_basket) > 2) {basket_1_3 = input$in_select_basket[[3]]} else {basket_1_3 = 'EMPTY'}
        if(length(input$in_select_basket) > 3) {basket_1_4 = input$in_select_basket[[4]]} else {basket_1_4 = 'EMPTY'}
        
        updateNumericInput(session, inputId = 'wg_1', label = basket_1_1)
        updateNumericInput(session, inputId = 'wg_2', label = basket_1_2)
        updateNumericInput(session, inputId = 'wg_3', label = basket_1_3)
        updateNumericInput(session, inputId = 'wg_4', label = basket_1_4)
    })
    
    
    ## AUTO BASKET

    preparation_basket = reactiveVal(NULL)
    plot_cont_main = reactiveVal(NULL)
    proxy_basket = reactiveVal(NULL)
    in_commodity_main = reactiveVal(NULL)
    in_commodity_basket = reactiveVal(NULL)
    
    observeEvent(input$act_product_basket_lt, {
        
        commodity_main = input$in_select_main_product
        commodity_basket = input$in_select_basket
        start_horizon = input$in_select_lt_train[1]
        end_horizon = input$in_select_lt_train[2]
        
        ## Generate continuation codes ----------------------------------------
        list_cont_codes = eikondata::products_continuation[COMMODITY %in% c(commodity_main, commodity_basket)]
        
        list_cont = lapply(list_cont_codes$c1, function(x) {
            tryCatch({
                DT = eikondata::get_timeseries(
                    rics = x, 
                    fields = c("TIMESTAMP", "CLOSE", "VOLUME"),
                    start_date = paste0(start_horizon, 'T00:00:00'),
                    end_date = paste0(end_horizon, 'T00:00:00'),
                    interval = 'daily'
                )
                setDT(DT)
            }, error = function(e) {
                message(sprintf("Failed to retrieve data for RIC: %s - %s", x, e$message))
                return(NULL)
            })
        })
        
        dt_cont = rbindlist(list_cont)
        colnames(dt_cont) = c('DATE', 'VALUE', 'VOLUME', 'RIC')
        dt_cont[, DATE := sub("T.*", "", DATE)]
        dt_cont[, DATE := as.Date(DATE)]
        dt_cont[, VALUE := as.numeric(VALUE)]
        dt_cont[, VOLUME := as.numeric(VOLUME)]
        
        dt_cont = merge(melt(list_cont_codes, id.vars = 'COMMODITY', variable.name = 'TYPE', value.name = 'RIC'), dt_cont, by = 'RIC', all.y = TRUE)
        
        preparation_basket(dt_cont)
        
        list_autobasket = HPFC::basket_selection(DT = dt_cont, mk_comm_0 = commodity_main, basket = commodity_basket, start = input$in_select_lt_train[1], end = input$in_select_lt_train[2])
        
        coeff_table = as.data.table(list_autobasket$coef_glm)
        coeff_table[, RIC := NULL]
        # saveRDS(coeff_table, 'coeff_table.rds')
        proxy_basket(coeff_table)
        
        # PLOTS 
        DT = dt_cont[COMMODITY == commodity_main][DATE >= input$in_select_lt_train[1] & DATE <= input$in_select_lt_train[2]]
        DT = unique(DT)
        setorder(DT, COMMODITY, DATE)
        plot_main =
        DT |>
            e_charts(DATE) %>% 
            e_line(VALUE, smooth = TRUE, symbol='none', color = '#2392A2') %>% 
            e_title(text = paste("Main Power Cont.", commodity_main)) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_legend(bottom = 0) %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
        plot_cont_main(plot_main)
        in_commodity_main(commodity_main)
        in_commodity_basket(commodity_basket)
    })
    
    manual_weights = reactiveVal(NULL)
    observe({
        weights = c(input$wg_1, input$wg_2, input$wg_3, input$wg_4)
        commodities = input$in_select_basket[1:4]  # Extract up to 4 selected commodities
        
        # Fill missing slots with "EMPTY" to align length
        if (length(commodities) < 4) {
            commodities = c(commodities, rep("EMPTY", 4 - length(commodities)))
        }
        
        proxy_manual = data.table(
            COMMODITY = commodities,
            coeff = weights
        )
        
        proxy_manual = proxy_manual[COMMODITY != "EMPTY"]
        proxy_manual[, weight := coeff / sum(coeff)]
        proxy_manual[, coeff := coeff / 100]
        
        manual_weights(proxy_manual)
    })
    
    selected_weights = reactiveVal(NULL)
    observe({

        if(input$in_select_source_weights == 'Manual') {
            selected_weights(react$manual_weights)
        }
        if(input$in_select_source_weights == 'Auto')
            if(!is.null(react$proxy_basket)) {
                selected_weights(react$proxy_basket)
            } else {
                selected_weights(react$manual_weights)
            }
    })
    
    dt_proxy_basket = reactiveVal(NULL)
    observe({
        req(react$preparation_basket)
        req(react$selected_weights)
        dt_plot = merge(react$preparation_basket, react$selected_weights[, .(COMMODITY, coeff)], by = "COMMODITY")
        dt_plot[, weighted_value := VALUE * coeff]
        
        dt_plot_main = react$preparation_basket[COMMODITY == react$in_commodity_main, .(COMMODITY = react$in_commodity_main, VALUE = sum(VALUE)), by = 'DATE']
        dt_plot_proxy = dt_plot[, .(COMMODITY = 'BASKET', VALUE = sum(weighted_value)), by = 'DATE']
        dt_plot_basket = dt_plot[COMMODITY %in% react$in_commodity_basket, .(DATE, COMMODITY, VALUE)]
        
        # Combine data
        dt_all = rbindlist(list(dt_plot_main, dt_plot_proxy, dt_plot_basket), use.names = TRUE, fill = TRUE)
        dt_all = unique(dt_all)
        setorder(dt_all, COMMODITY, DATE)
        dt_proxy_basket(dt_all)
        
    })
    
    output$pwr_lt_coeff_table = renderDatagrid({
        req(react$selected_weights)
        datagrid(react$selected_weights[, .(COMMODITY, coeff = round(coeff, 2), weight = round(weight, 2))])
    })
    
    # output$pwr_lt_main_plot = renderEcharts4r({
    #     req(react$plot_cont_main)
    #     react$plot_cont_main
    # })
    
    output$pwr_lt_basketcorrelation_plot = renderEcharts4r({
        req(react$dt_proxy_basket)
        
        react$dt_proxy_basket[DATE >= input$in_select_lt_train[1] & DATE <= input$in_select_lt_train[2]] |>
            group_by(COMMODITY) |>
            e_charts(DATE) %>% 
            e_line(VALUE, smooth = TRUE, symbol='none', bind = COMMODITY) %>% 
            e_title(text = paste("Basket Power Cont.")) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_legend(top = 30) %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
    })
    
    fwd_main = reactiveVal(NULL)
    fwd_basket = reactiveVal(NULL)
    list_data = reactiveVal(NULL)
    
    observeEvent(input$act_download_fwd, {
        
        list_data = get_data_pipeline(commodity_main = input$in_select_main_product,
                                      commodity_basket = input$in_select_basket,
                                      start_train = input$in_select_lt_train[1],
                                      end_train = input$in_select_lt_train[2],
                                      start_horizon = input$in_select_lt_horizon[1],
                                      end_horizon = input$in_select_lt_horizon[2]
                                      )
        # saveRDS(list_data, 'list_data.rds')
        # list_data = readRDS('list_data.rds')
        
        list_data(list_data)
    })
    
    
    observeEvent(input$act_generate_fwd_curves, {
        req(react$selected_weights)
        req(react$list_data)
        
        dts =
            fwd_pipeline(
                commodity_main = input$in_select_main_product,
                coef_glm = react$selected_weights,
                list_data = react$list_data,
                start_train = input$in_select_lt_train[1],
                end_train = input$in_select_lt_train[2],
                start_horizon = input$in_select_lt_horizon[1],
                end_horizon = input$in_select_lt_horizon[2]
            )

        dts1 = as.data.table(dts[1])
        dts2 = as.data.table(dts[2])

        colnames(dts1) = c('date', 'hour', 'forecast', 'name')
        colnames(dts2) = c('date', 'hour', 'forecast', 'name')

        fwd_main(dts1)
        fwd_basket(dts2)
        
    })
    
    
    output$pwr_lt_basket_plot = renderEcharts4r({

        req(react$fwd_main)
        req(react$fwd_basket)

        dt1_avg = react$fwd_main[, .(value_main = mean(forecast)), by = .(date)]
        dt2_avg = react$fwd_basket[, .(value_proxy = mean(forecast)), by = .(date)]

        merged = merge(dt1_avg, dt2_avg, by = c("date"))

        merged[order(date)] %>%
            e_charts(date) %>%
            e_line(value_main, name = "Main", symbol = 'none') %>%
            e_line(value_proxy, name = "Proxy", symbol = 'none') %>%
            e_color(c("#2392A2", "#C05B8C")) %>%
            e_tooltip(trigger = "axis") %>%
            e_title("FWD Curves") %>%
            e_y_axis(name = "Price") %>%
            e_x_axis(name = "Date") %>%
            e_legend(top = 30) %>%
            e_datazoom(start = 0) %>%
            e_theme('westeros')
    })
    
    ## SCENARIOS
    dt_scenario = reactive({
        req(input$in_load_scenario)
        file_path = input$in_load_scenario$datapath
        df = openxlsx::read.xlsx(file_path)
        dt = data.table::as.data.table(df)
        setorder(dt, COMMODITY, DATE)
        print(dt)
    })
    
    dt_scenario = reactiveVal(NULL)
    observe({
        req(input$in_load_scenario)
        file_path = input$in_load_scenario$datapath
        df = openxlsx::read.xlsx(file_path, detectDates = TRUE)
        dt = data.table::as.data.table(df)
        setorder(dt, COMMODITY, DATE)
        dt_scenario(dt)
    })
    
    output$pwr_lt_scenario_plot = renderEcharts4r({
        req(react$dt_scenario)
        react$dt_scenario[, .(VALUE = mean(VALUE, na.rm = TRUE)), by = 'DATE'] |>
            e_charts(DATE) %>% 
            e_line(VALUE, smooth = TRUE, symbol='none', color = '#DE8969') %>% 
            e_title(text = paste("Scenario Data")) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_legend(top = 30) %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
    })
    
    ## CREATE FINAL CURVE
    
    dt_final_curve = reactiveVal(NULL)
    observeEvent(input$act_product_create_lt, {
        
        req(react$fwd_main)
        req(react$fwd_basket)
        req(react$dt_scenario)
        
        dt_main = react$fwd_main[, .(date, hour, value = forecast, source = 'MAIN')]
        dt_proxy = react$fwd_basket[, .(date, hour, value = forecast, source = 'PROXY')]
        dt_sce = react$dt_scenario[, .(date = DATE, hour = HOUR, value = VALUE, source = 'SCENARIO')]
        
        T1 = input$in_select_cutoff_mkt
        T2 = input$in_select_cutoff_sce
        
        dt_main_part = dt_main[date < T1]
        dt_proxy_part = dt_proxy[date >= T1 & date < T2]
        dt_sce_part = dt_sce[date >= T2]
        
        dt_final = rbindlist(list(dt_main_part, dt_proxy_part, dt_sce_part), use.names = TRUE)
        dt_final = dt_final[date >= input$in_select_total_horizon[1] & date < input$in_select_total_horizon[2]]
        
        dt_final_curve(dt_final)
        
        last_path = file.path('HPFC', 'last', 'output', input$in_select_main_product)
        if (!dir.exists(last_path)) {
            dir.create(last_path, recursive = TRUE)
        }
        
        saveRDS(dt_final, file.path(last_path, paste0('forecast_pwr_lt-', Sys.Date() ,'.rds')))
        fwrite(dt_final, file.path(last_path, paste0('forecast_pwr_lt', Sys.Date() ,'.csv')))
        
    })
    
    
    output$pwr_lt_final_plot = renderEcharts4r({
        
        req(react$dt_final_curve)
        
        dt_avg = react$dt_final_curve[, .(value = mean(value)), by = .(date, source)]
        
        dt_avg[order(date)] |>
            group_by(source)|>
            e_charts(date) |>
            e_line(serie = value, symbol = "none", bind = source) |>
            e_tooltip(trigger = "axis") |>
            e_color(c("#2392A2", "#C05B8C", "#DE8969")) |>
            e_title("Long Term Forecast Curve") |>
            e_y_axis(name = "Price") |>
            e_x_axis(name = "Datetime") |>
            e_legend(top = 30) %>% 
            e_theme('westeros')
    })
    
    
    output$act_lt_pwr_download <- downloadHandler(
        filename = function() {
            paste0("forecast_pwr_lt_", input$in_select_main_product, '-', Sys.Date(), ".csv")
        },
        content = function(file) {
            fwrite(react$dt_final_curve, file)  # for CSV
            # saveRDS(dt_final, file)  # for RDS
        }
    )
    
    
    
    # L. BACKTESTING ------------------------------------------------------------------
    
    dt_backtest = reactiveVal(NULL)
    observeEvent(input$act_load_backtest, {
        
        if(input$in_select_backtest_source == 'Sim') {shiny_sim = input$in_select_sim_name_backtest} else {shiny_sim = NULL}
        
        backtest = run_forecast_backtest_pwr(
            x = input$in_select_PWR_backtest,
            in_select_backtest_source = input$in_select_backtest_source,
            in_select_backtest_period_1 = input$in_select_backtest_period[1],
            in_select_backtest_period_2 = input$in_select_backtest_period[2],
            shiny_sim = shiny_sim,
            reuters_key = PLEASE_INSERT_REUTERS_KEY
        )
        backtest$dt_pwr_lg = backtest$dt_pwr_lg[date >= input$in_select_backtest_period[1] & date <= input$in_select_backtest_period[2]]
        dt_backtest(backtest)
    })
    
    output$backtest_overview_plot = renderEcharts4r({
        req(react$dt_backtest)
        
        react$dt_backtest$dt_pwr_lg %>% 
            group_by(type) %>% 
            e_charts(datetime) %>% 
            e_line(value, smooth = TRUE, symbol='none') %>% 
            e_title(text = paste("Hourly Forecast Prices")) %>%
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_toolbox_feature(feature = "restore") %>%
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
        
    })
    
    dt_error = reactiveVal(NULL) 
    
    observe({
        req(react$dt_backtest)
        dt_pwr = react$dt_backtest$dt_pwr_lg
        dt_pwr[, month := format(date, "%Y-%m-01")]
        dt_pwr[, wday := wday(date)]
        dt_pwr[, value := round(value, 2)]
        
        setorder(dt_pwr, -type, date)
        
        dt_pwr_error = dt_pwr[, .(date, hour, type, month, season, wday, value)] %>%
            dcast(date + hour + month + season + wday ~ type)
        
        dt_pwr_error[, peak := fifelse(hour >= 9 & hour < 21, 'ON', 'OFF')]
        
        setnames(dt_pwr_error, old = c('spot', 'forecast'), new = c('OBS', 'FOR'))
        dt_pwr_error[, value_bl := NULL]
        dt_pwr_error[, value_gas := NULL]
        
        dt_pwr_error[, ERROR := OBS - FOR]
        dt_pwr_error[, DIR := fifelse(ERROR >= 0, 'POS', 'NEG')]
        dt_pwr_error[, ERROR := round(ERROR, 2)]
        
        dt_error(dt_pwr_error)
    })
    
    
    output$backtest_error_distribution = renderEcharts4r({
        req(react$dt_error)
        dt_plot = react$dt_error[
            ERROR >= quantile(ERROR, 0.05, na.rm = TRUE) & 
                ERROR <= quantile(ERROR, 0.95, na.rm = TRUE)
        ]
        
        mean_err = mean(dt_plot$ERROR, na.rm = TRUE)
        sd_err = sd(dt_plot$ERROR, na.rm = TRUE)
        
        dt_plot %>% 
            e_charts() %>% 
            e_density(ERROR, areaStyle = list(opacity = .4), smooth = TRUE, y_index = 1, symbol = 'none', name = "Density") %>%  
            e_mark_line(data = list(xAxis = mean_err), title = "mean") %>%
            e_mark_line(data = list(xAxis = mean_err+sd_err), title = "+sd") %>%
            e_mark_line(data = list(xAxis = mean_err-sd_err), title = "-sd") %>%
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0)
        
    })  
    
    
    output$backtest_error_gauge_pv = renderEcharts4r({
        req(react$dt_error)
        pv_profile = readRDS('www/consumption_profiles.rds')
        error_pv = merge(react$dt_error[, .(date, hour, ERROR)], pv_profile, by = c('date', 'hour'), all.x = TRUE)
        error_pv = error_pv[, .(cp_delta = sum(ERROR * pv, na.rm = TRUE))]
        
        e_charts() |> 
            e_gauge(round(error_pv[[1]], 2), "ERROR") |> 
            e_title("PV Profile")
    })
    
    output$backtest_error_gauge_lv = renderEcharts4r({
        req(react$dt_error)
        lv_profile = readRDS('www/consumption_profiles.rds')
        error_pv = merge(react$dt_error[, .(date, hour, ERROR)], lv_profile, by = c('date', 'hour'), all.x = TRUE)
        error_pv = error_pv[, .(cp_delta = sum(ERROR * pv, na.rm = TRUE))]
        e_charts() |> 
            e_gauge(round(react$dt_error[, .(mean = mean(ERROR, na.rm = TRUE))][[1]], 2), "ERROR") |> 
            e_title("LV Profile")
    })    
    

    output$backtest_mean_day_plot = renderEcharts4r({
        req(react$dt_backtest)
        
        dt_pwr = react$dt_backtest$dt_pwr_lg[type %in% c('spot', 'forecast')]
        dts = dt_pwr[, .(value = round(mean(value, na.rm = TRUE))), by = .(hour, type)] 
        dts %>% 
            group_by(type) %>% 
            e_charts(hour) %>% 
            e_line(value, smooth = TRUE) %>% 
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0) 
    }) 
    

    output$backtest_mean_day_winter = renderEcharts4r({
        req(react$dt_backtest)
        
        dt_pwr = react$dt_backtest$dt_pwr_lg[type %in% c('spot', 'forecast')]
        dts = dt_pwr[season == 'winter', .(value = round(mean(value, na.rm = TRUE))), by = .(hour, type)] 
        dts %>% 
            group_by(type) %>% 
            e_charts(hour) %>% 
            e_line(value, smooth = TRUE) %>% 
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0)  
    }) 
    
    output$backtest_mean_day_spring = renderEcharts4r({
        req(react$dt_backtest)
        
        dt_pwr = react$dt_backtest$dt_pwr_lg[type %in% c('spot', 'forecast')]
        dts = dt_pwr[season == 'spring', .(value = round(mean(value, na.rm = TRUE))), by = .(hour, type)] 
        dts %>% 
            group_by(type) %>% 
            e_charts(hour) %>% 
            e_line(value, smooth = TRUE) %>% 
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0)  
    }) 
    
    output$backtest_mean_day_summer = renderEcharts4r({
        req(react$dt_backtest)
        
        dt_pwr = react$dt_backtest$dt_pwr_lg[type %in% c('spot', 'forecast')]
        dts = dt_pwr[season == 'summer', .(value = round(mean(value, na.rm = TRUE))), by = .(hour, type)] 
        dts %>% 
            group_by(type) %>% 
            e_charts(hour) %>% 
            e_line(value, smooth = TRUE) %>% 
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0)  
    }) 
    
    output$backtest_mean_day_fall = renderEcharts4r({
        req(react$dt_backtest)
        
        dt_pwr = react$dt_backtest$dt_pwr_lg[type %in% c('spot', 'forecast')]
        dts = dt_pwr[season == 'fall', .(value = round(mean(value, na.rm = TRUE))), by = .(hour, type)] 
        dts %>% 
            group_by(type) %>% 
            e_charts(hour) %>% 
            e_line(value, smooth = TRUE) %>% 
            e_legend(show = TRUE, orient = 'horizontal') %>% 
            e_tooltip(trigger = "axis") %>% 
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0)  
    })     
    
    
    output$backtest_error_metric_seasons = renderDatagrid({
        req(react$dt_error)
        DT =
        react$dt_error[, .(
            MEAN = round(mean(ERROR, na.rm = TRUE), 2),
            MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
            SD = round(sd(ERROR, na.rm = TRUE), 2),
            IQR = round(IQR(ERROR, na.rm = TRUE), 2),
            Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
            Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
        ), by = season] 
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$backtest_error_metric_weekday = renderDatagrid({
        req(react$dt_error)
        DT =
            react$dt_error[, .(
                MEAN = round(mean(ERROR, na.rm = TRUE), 2),
                MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
                SD = round(sd(ERROR, na.rm = TRUE), 2),
                IQR = round(IQR(ERROR, na.rm = TRUE), 2),
                Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
                Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
            ), by = wday] 
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$backtest_error_metric_peak = renderDatagrid({
        req(react$dt_error)
        DT =
            react$dt_error[, .(
                MEAN = round(mean(ERROR, na.rm = TRUE), 2),
                MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
                SD = round(sd(ERROR, na.rm = TRUE), 2),
                IQR = round(IQR(ERROR, na.rm = TRUE), 2),
                Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
                Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
            ), by = peak] 
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$backtest_error_bias_seasons_a = renderDatagrid({
        req(react$dt_error)
        DT =
            react$dt_error[, .(
                MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
            ), by = .(season)] 
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$backtest_error_bias_seasons_b = renderDatagrid({
        req(react$dt_error)
        DT =
            react$dt_error[, .(
                MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
            ), by = .(season, DIR)] %>% dcast(season ~ DIR)
        
        datagrid(DT,
                 filters = TRUE)
    })    
    
    output$backtest_error_bias_peak_a = renderDatagrid({
        req(react$dt_error)
        
        ## error metrics
        DT =
            react$dt_error[, .(
                MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
            ), by = .(peak)] 
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    output$backtest_error_bias_peak_b = renderDatagrid({
        req(react$dt_error)
        
        ## error metrics
        DT =
            react$dt_error[, .(
                MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
            ), by = .(peak, DIR)] %>% dcast(peak ~ DIR)
        
        datagrid(DT,
                 filters = TRUE)
    })
    
    
    output$in_backtest_pwr_download = downloadHandler(
        filename = function() {
            paste0("backtest_pwr-", input$in_select_PWR_backtest, '-', Sys.Date(), ".csv")
        },
        content = function(file) {
            fwrite(react$dt_backtest$dt_pwr_lg, file)
        }
    )
    
    
    ## END
    
}