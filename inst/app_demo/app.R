
# SETUP ------------------------------------------------------------------------------------------------- 

library(shiny)
library(shinyjs)
library(bslib)        # for bslib theme and components
library(echarts4r)     # for interactive charts
library(reactable)     # for interactive tables
library(shinycssloaders) # for loading spinner
library(magrittr)
library(react)
# library(HPFC)
devtools::load_all()
options(shiny.maxRequestSize = 50*1024^2)  # 50 MB

mbs_theme =
    bs_theme(
        version = 5,
        bootswatch = 'zephyr',
        primary = '#001437',
        secondary = '#dee2e6',
        success = '#2EB67D',
        info = '#0E9DD3',
        warning = '#f4bd61',
        danger = '#DE8969',
        base_font = font_google(family = "Inter"),
        heading_font = font_google(family = "Inter"),
        font_scale = 0.9,
        "navbar-nav-link-padding-x" = "0px",
        "navbar-nav-link-padding-y" = "0px"
    )

#bs_theme_preview(nordquant_theme)

bs_theme_update(mbs_theme, `enable-rounded` = FALSE)

# INPUTS ------------------------------------------------------------------------------------------------- 

## Training ===========================================================================================

vec_pwr_products =        HPFC::spot_PWR_products_full$countries
names(vec_pwr_products) = HPFC::spot_PWR_products_full$countries
vec_gas_products =        HPFC::spot_GAS_products_full$products_GAS
names(vec_gas_products) = HPFC::spot_GAS_products_full$products_GAS


### TRAINING PERIOD --------------------------------------------------------
select_history_period =
    dateRangeInput(
        inputId = "in_select_history",
        label = span("Select history Interval:", style = 'font-weight: bold;'),
        start  = "2016-01-01",
        end    = "2024-12-31",
        min    = "2016-01-01",
        max    = Sys.Date(),
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )


### SELECT PRODUCTS --------------------------------------------------------
select_PWR_product =
    selectInput(
        inputId = "in_select_PWR_indicator",
        label = span("Power:", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_GAS_product =
    selectInput(
        inputId = "in_select_GAS_indicator",
        label = span("Gas:", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )

#### BUTTON TO EXECUTE TRAINING --------------------------------
product_train_pwr =
    input_task_button(
        id = 'act_indicator_train_pwr',
        label = 'Train Power model',
        label_busy = "Training...",
        icon = shiny::icon('run'),
        width = '100%',
        type = "danger"
    )


product_train_gas =
    input_task_button(
        id = 'act_indicator_train_gas',
        label = 'Train Gas model',
        label_busy = "Training...",
        icon = shiny::icon('run'),
        width = '100%',
        type = "warning"
    )


### DATA SOURCE -------------------------------------
select_source_train =
    radioButtons(
        inputId = "in_source_train",
        label = "Select data source:",
        choices = c("Reuters",
                    "Excel"),
        selected = "Reuters",
        inline = FALSE
    )


### BUTTONS EXECUTE DOWNLOAD -----------------------
train_pwr_download =
    downloadButton(
        outputId = 'act_train_pwr_download',
        label = 'Power Download',
        icon = shiny::icon('download'),
        style = "width:50%;",
        class = "btn-secondary"
    )

train_gas_download =
    downloadButton(
        outputId = 'act_train_gas_download',
        label = 'Gas Download',
        icon = shiny::icon('download'),
        style = "width:50%;",
        class = "btn-secondary"
    )



## Forecast ====================================================================================

### FORECASTING PERIOD --------------------------------------------------------
select_horizon_period =
    dateRangeInput(
        inputId = "in_select_horizon",
        label = span("Select forecast horizon Interval:", style = 'font-weight: bold;'),
        start  = '2024-01-01',
        end    = '2024-12-31', 
        min    = '2017-01-01',
        max    = '2030-12-31',
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )

select_PWR_product_for =
    selectInput(
        inputId = "in_select_PWR_indicator_for",
        label = span("Power:", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_GAS_product_for =
    selectInput(
        inputId = "in_select_GAS_indicator_for",
        label = span("Gas:", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )

#### BUTTON TO EXECUTE FORECAST --------------------------------
product_forecast_pwr =
    input_task_button(
        id = 'act_indicator_forecast_pwr',
        label = 'Forecast Power',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "danger"
    )

product_forecast_gas =
    input_task_button(
        id = 'act_indicator_forecast_gas',
        label = 'Forecast Gas',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "warning"
    )

### DATA SOURCE -------------------------------------
select_source_forecast =
    radioButtons(
        inputId = "in_source_forecast",
        label = "Select data source:",
        choices = c("Reuters",
                    "Excel"),
        selected = "Reuters",
        inline = FALSE
    )



### BUTTONS EXECUTE DOWNLOAD -----------------------
fwd_pwr_download =
    downloadButton(
        outputId = 'act_forecast_pwr_download',
        label = ' Power',
        icon = shiny::icon('download'),
        style = "width:50%;",
        class = "btn-secondary"
    )

fwd_gas_download =
    downloadButton(
        outputId = 'act_forecast_gas_download',
        label = ' Gas',
        icon = shiny::icon('download'),
        style = "width:50%;",
        class = "btn-secondary"
    )



### PLOT SELECTORS -----------------------
plot_forecast_selector_gas =
    selectInput(
        inputId = "in_plot_forecast_selector_gas",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = c('TTF' = 'TFMB')
    )

plot_forecast_selector_pwr =
    selectInput(
        inputId = "in_plot_forecast_selector_pwr",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = c('TTF' = 'TFMB')
    )



# UI ------------------------------------------------------------------------------------------------- 

ui = page_navbar(
    
    # Use bslib for custom themes
    theme = mbs_theme,
    bg = '#001437',
    
    # Title Panel of the app
    title = "HPFC App",
    
    header = tagList(
        useShinyjs(),
        tags$style(".xxxx {margin-left:5px;}"),
        tags$style(".xxxx {margin-left:5px;}"),
        tags$style(HTML('
                        .navbar-nav > li > a {
                              padding: 10px !important;
                              font-color:#d8dee9;
                              letter-spacing: 1px;
                              display: flex;
                              align-items: center;
                            }'
        )
        ),
        tags$style(".topimg {
                            margin-left:5px;
                            margin-right: -15px;
                            margin-top: -19px;
                            margin-bottom: -60px;
                          }"
        )
    ),
    
    # Tabs in the NavbarPage (Train and Forecast)
    nav_panel(title = 'TRAIN',
              layout_sidebar(
                  sidebar = sidebar(bg = 'white',
                                    width = 400, padding = '40',
                                    title = 'Training',
                                    select_history_period,
                                    select_PWR_product,
                                    select_GAS_product,
                                    hr(),
                                    product_train_pwr,
                                    product_train_gas,
                                    hr(),
                                    select_source_train,
                                    uiOutput("select_source_file_train"),
                                    hr(),
                                    fluidRow(train_pwr_download, train_gas_download),
                                    br()
                  ),
                  
                  # Main Panel for the training
                  navset_card_pill(
                      full_screen = TRUE,
                      nav_panel('Power',
                                layout_sidebar(
                                    sidebar = sidebar(bg = 'white',
                                                      reactableOutput('forecast_params_table_recap_pwr'), position = 'right', open = FALSE, width = '450px'),
                                    fluidRow(
                                        echarts4rOutput(outputId = 'pwr_history_plot') %>% withSpinner(color = "#d08770"),
                                        hr(), br(),
                                        reactableOutput(outputId = 'pwr_history_table') %>% withSpinner(color = "#d08770")
                                    )
                                )
                      ),
                      
                      nav_panel('Gas',
                                layout_sidebar(
                                    sidebar = sidebar(bg = 'white',
                                                      reactableOutput('forecast_params_table_recap_gas'), position = 'right', open = FALSE, width = '450px'),
                                    fluidRow(
                                        echarts4rOutput(outputId = 'gas_history_plot') %>% withSpinner(color = "#d08770"),
                                        hr(), br(),
                                        reactableOutput(outputId = 'gas_history_table') %>% withSpinner(color = "#d08770")
                                    )
                                )
                      )
                  )
              )
    ),
    
    nav_panel(title = 'FORECAST',
              layout_sidebar(
                  sidebar = sidebar(bg = 'white',
                                    width = 400, padding = '40',
                                    title = 'Forecasting',
                                    select_horizon_period,
                                    select_PWR_product_for,
                                    select_GAS_product_for,
                                    hr(),
                                    product_forecast_pwr,
                                    product_forecast_gas,
                                    hr(),
                                    select_source_forecast,
                                    uiOutput("select_source_file_forecast"),
                                    hr(),
                                    fluidRow(fwd_pwr_download, fwd_gas_download),
                                    br()
                  ),
                  
                  # Main Panel for the forecast
                  navset_card_pill(
                      full_screen = TRUE,
                      nav_panel('Power',
                                fluidRow(
                                    echarts4rOutput(outputId = 'pwr_forecast_plot') %>% withSpinner(color = "#d08770")
                                )
                      ),
                      
                      nav_panel('Gas',
                                fluidRow(
                                    echarts4rOutput(outputId = 'gas_forecast_plot') %>% withSpinner(color = "#d08770")
                                )
                      )
                  )
              )
    )
)




# SERVER  ------------------------------------------------------------------------------------------------- 
server = function(input, output, session) {
    
    ## MANUAL DATA ------------------------------------------------------
    ### SPOTS
    observe({
        if (input$in_source_train == 'Excel') {
            output$select_source_file_train = renderUI({
                fileInput(
                    inputId = "in_train_excel",
                    label = "Excel file with spot data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })
            
        } else {
            output$select_source_file_train = renderUI(NULL)
        }
    })
    
    dt_spot_manual = reactiveVal(NULL)
    observe({
        req(input$in_train_excel)
        file_path = input$in_train_excel$datapath
        df = openxlsx::read.xlsx(file_path)
        dt = data.table::as.data.table(df)
        print(dt)
        dt_spot_manual(dt)
    })
    
    
    ### FWD
    observe({
        if (input$in_source_forecast == 'Excel') {
            output$select_source_file_forecast = renderUI({
                fileInput(
                    inputId = "in_forecast_excel",
                    label = "Excel file with forecast data",
                    accept = c(".xlsx", ".xls"),
                    multiple = FALSE
                )
            })
            
        } else {
            output$select_source_file_forecast = renderUI(NULL)
        }
    })
    
    dt_forecast_manual = reactiveVal(NULL)
    observe({
        req(input$in_forecast_excel)
        file_path = input$in_forecast_excel$datapath
        df = openxlsx::read.xlsx(file_path)
        dt = data.table::as.data.table(df)
        print(dt)
        dt_forecast_manual(dt)
    })
    
    
    # Reactive input recap data -----------------
    recap_data = reactive({
        data.frame(
            Input = c("History Period", "Forecast Horizon Period", "Power Products (Training)", "Gas Products (Training)"),
            Value = c(paste(input$in_select_history[1], "to", input$in_select_history[2]),
                      paste(input$in_select_horizon[1], "to", input$in_select_horizon[2]),
                      paste(input$in_select_PWR_indicator, collapse = ", "),
                      paste(input$in_select_GAS_indicator, collapse = ", "))
        )
    })
    
    # Render the recap table
    output$input_recap_table = renderReactable({
        reactable(recap_data(), columns = list(
            Value = colDef(width = 200)
        ))
    })
    
    
    # TRAIN - PWR ------------------------------------------
    
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
            data_source = '0df86b690b2c4ae2bf245680dbbfcc86bb041dc9',
            forecast_source = 'FWD',
            sim_name = 'NO',
            archive = 'NO'
        )
        
        params_input_pwr(params_list)
        
    })
    
    ### Exceute load_inputs
    observeEvent(input$act_indicator_train_pwr, {
        
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== START TRAINING PWR ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        print('------------- LOAD INPUTS START -------------')
        
        LST_PARAMS = react$params_input_pwr
        list_inputs = HPFC::load_inputs(params = LST_PARAMS)
        
        list_inputs_field_pwr(list_inputs)
        
        print('------------- LOAD INPUTS END ---------------')
        
    })
    
    
    
    
    ## Prepare Curves -----------------------
    prepare_gas_field_pwr = reactiveVal(NULL)
    prepare_pwr_field = reactiveVal(NULL)
    
    observe({
        
        req(react$list_inputs_field_pwr)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- PREPARE START -------------')
        
        list_inputs = react$list_inputs_field_pwr
        ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
        ENV_MODELS_PWR = prepare_pwr(list_inputs = list_inputs)
        
        prepare_gas_field_pwr(ENV_MODELS_GAS)
        prepare_pwr_field(ENV_MODELS_PWR)
        
        print('------------- PREPARE END ---------------')
        
    })
    
    
    ## Train Models -----------------------
    models_gas_field_pwr = reactiveVal(NULL)
    models_pwr_field = reactiveVal(NULL)
    
    observe({
        
        req(react$prepare_gas_field_pwr)
        req(react$prepare_pwr_field)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- TRAIN START -------------')
        
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
        
        models_gas_field_pwr(ENV_MODELS_GAS)
        models_pwr_field(ENV_MODELS_PWR)
        
        print('------------- TRAIN END ---------------')
        
    })
    
    
    # TRAIN - GAS ------------------------------------------
    
    ## Inputs -----------------------
    params_input_gas = reactiveVal(NULL)
    list_inputs_field_gas = reactiveVal(NULL)
    
    ### Prepare inputs params
    observe({
        params_list = list(
            model_type = 'PWR',
            selected_pwr_code = input$in_select_PWR_indicator,
            selected_gas_code = input$in_select_GAS_indicator,
            dependent_gas_code = input$in_select_PWR_indicator,
            history_start = input$in_select_history[1],
            history_end = input$in_select_history[2],
            forecast_start = input$in_select_horizon[1],
            forecast_end = input$in_select_horizon[2],
            model_source = 'TRAIN',
            data_source = '0df86b690b2c4ae2bf245680dbbfcc86bb041dc9',
            forecast_source = 'FWD',
            sim_name = 'NO',
            archive = 'NO'
        )
        
        params_input_gas(params_list)
        
    })
    
    ### Exceute load_inputs
    observeEvent(input$act_indicator_train_gas, {
        
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== START TRAINING GAS ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        print('------------- LOAD INPUTS START -------------')
        
        LST_PARAMS = react$params_input_gas
        list_inputs = HPFC::load_inputs(params = LST_PARAMS)
        
        list_inputs_field_gas(list_inputs)
        
        print('------------- LOAD INPUTS END ---------------')
        
    })
    
    
    
    
    ## Prepare Curves -----------------------
    prepare_gas_field_gas = reactiveVal(NULL)
    
    observe({
        
        req(react$list_inputs_field_gas)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- PREPARE START -------------')
        
        list_inputs = react$list_inputs_field_gas
        ENV_MODELS_GAS = prepare_gas(list_inputs = list_inputs)
        
        prepare_gas_field_gas(ENV_MODELS_GAS)
        
        print('------------- PREPARE END ---------------')
        
    })
    
    
    ## Train Models -----------------------
    models_gas_field_gas = reactiveVal(NULL)
    
    observe({
        
        req(react$prepare_gas_field_gas)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- TRAIN START -------------')
        
        ENV_MODELS_GAS = react$prepare_gas_field_gas
        
        ENV_MODELS_GAS$dt_lt_param_gasdep = 
            train_lt_gas(
                gas_data = ENV_MODELS_GAS$dt_lt_param_gasdep,
                ric_gas = unique(ENV_MODELS_GAS$dt_gas$RIC)
            )
        
        models_gas_field_gas(ENV_MODELS_GAS)
        
        print('------------- TRAIN END ---------------')
        
    })
    
    
    ## Download models -----------------------
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
    
    
    
    # FORECAST - PWR ------------------------------------------
    
    object_with_forecast_data_pwr = reactiveVal(NULL) 
    
    # PREPARE FWD ---------------------------------------------------
    
    fwd_pwr_field = reactiveVal(NULL)
    fwd_gas_field = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_pwr, {
        
        list_inputs_fwd = prepare_fwd(
            fwd_pwr_code = input$in_select_PWR_indicator_for,
            fwd_gas_code = 'TTF',
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            model_type = 'PWR',
            forecast_source = 'FWD',
            archive = 'NO',
            manual_pwr = NULL,
            manual_gas = NULL
        )
        
        fwd_pwr_field(list_inputs_fwd)
        
    })
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        list_inputs_fwd = prepare_fwd(
            fwd_gas_code = input$in_select_GAS_indicator_for,
            start_date = input$in_select_horizon[1],
            end_date = input$in_select_horizon[2],
            model_type = 'GAS',
            forecast_source = 'FWD',
            archive = 'NO',
            manual_pwr = NULL,
            manual_gas = NULL
        )
        
        fwd_gas_field(list_inputs_fwd)
        
    })    
    
    ## Forecast Parameters -----------------------
    forecast_params_field_pwr = reactiveVal(NULL)
    forecast_params_table_pwr = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_pwr, {
        
        req(react$models_gas_field_pwr)
        req(react$models_pwr_field)
        req(react$fwd_pwr_field)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- FORECAST PARAMS PREP START -------------')
        
        ENV_MODELS_GAS = react$models_gas_field_pwr
        ENV_MODELS_PWR = react$models_pwr_field
        list_inputs = react$list_inputs_field_pwr
        LST_PARAMS = react$params_input_pwr
        FWD = react$fwd_pwr_field$ENV_FWD
        
        LST_FOR = list(
            model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
            model_lt_pwr = copy(ENV_MODELS_PWR$dt_lt_param_pwr),
            model_st_pwr = copy(ENV_MODELS_PWR$lst_hr_param_pwr),
            dt_fwds = copy(FWD$dt_fwds),
            saved_history_gas = copy(list_inputs$ENV_SPOT$history_gas),
            saved_history_pwr = copy(list_inputs$ENV_SPOT$history_pwr),
            ric_spot_gas = list_inputs$ENV_SPOT$spot_gas_RIC,
            ric_fwd_gas = HPFC::spot_GAS_products_full[products_GAS %in% unique(LST_PARAMS$dependent_gas_code)]$products_GAS_code,
            ric_spot_pwr = list_inputs$ENV_SPOT$spot_pwr_RIC,
            ric_fwd_pwr = HPFC::spot_PWR_products_full[countries %in% unique(LST_PARAMS$selected_pwr_code)]$products_PWR_code,
            calendar_forecast = FWD$calendar_future,
            start_date = LST_PARAMS$forecast_start,
            end_date = LST_PARAMS$forecast_end,
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
        
        forecast_params_field_pwr(LST_FOR)
        forecast_params_table_pwr(dt_recap)
        
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== END TRAINING PWR  ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
    })
    
    ## Forecast Parameters -----------------------
    forecast_params_field_gas = reactiveVal(NULL)
    forecast_params_table_gas = reactiveVal(NULL)
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        req(react$models_gas_field_gas)
        req(react$fwd_gas_field)
        
        print('==================== ++++++++++++++ ====================')
        print('------------- FORECAST PARAMS PREP START -------------')
        
        ENV_MODELS_GAS = react$models_gas_field_gas
        list_inputs = react$list_inputs_field_gas
        LST_PARAMS = react$params_input_gas
        FWD = react$fwd_gas_field$ENV_FWD
        
        LST_FOR = list(
            model_lt_gas = copy(ENV_MODELS_GAS$dt_lt_param_gasdep),
            dt_fwds = copy(FWD$dt_fwds),
            saved_history_gas = copy(list_inputs$ENV_SPOT$history_gas),
            ric_spot_gas = list_inputs$ENV_SPOT$spot_gas_RIC,
            ric_fwd_gas = HPFC::spot_GAS_products_full[products_GAS %in% unique(LST_PARAMS$selected_gas_code)]$products_GAS_code,
            calendar_forecast = FWD$calendar_future,
            start_date = LST_PARAMS$forecast_start,
            end_date = LST_PARAMS$forecast_end,
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
        print('==================== ++++++++++++++ ====================')
        print('==================== END TRAINING GAS  ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
    })
    
    observe({
        
        req(react$forecast_params_field_pwr)
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== START FORECASTING PWR ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        print('------------- FORECAST START -------------')
        
        ENV_FOR_GAS = forecast_gas(input_forecast = react$forecast_params_field_pwr)
        ENV_FOR_PWR = forecast_pwr(input_forecast = react$forecast_params_field_pwr, gas_forecast = ENV_FOR_GAS)
        
        dt_pwr_for = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast, RIC, season, peak, value_gas, value_bl = spot_forward_month_BL)]
        dt_pwr_obs = react$forecast_params_field_pwr$saved_history_pwr[year(date) %in% unique(year(dt_pwr_for$date)) & RIC == unique(dt_pwr_for$RIC)][, .(date, hour, spot = value, RIC)]
        dt_pwr = merge(dt_pwr_for, dt_pwr_obs, by = c('date', 'hour', 'RIC'), all = TRUE)
        
        setcolorder(dt_pwr, c('date', 'hour', 'season', 'peak', 'RIC', 'spot', 'forecast', 'value_bl', 'value_gas'))
        setorder(dt_pwr, date, hour)
        
        object_with_forecast_data_pwr(dt_pwr)
        
        print('------------- FORECAST END -------------')        
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== END FORECASTING PWR ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        
    })
    
    
    # FORECAST - GAS ------------------------------------------
    
    object_with_forecast_data_gas = reactiveVal(NULL) 
    
    observeEvent(input$act_indicator_forecast_gas, {
        
        req(react$forecast_params_field_gas)
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== START FORECASTING GAS ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        print('------------- FORECAST START -------------')
        
        ENV_FOR_GAS = forecast_gas(input_forecast = react$forecast_params_field_gas)
        
        dt_gas_for = ENV_FOR_GAS[, .(date, forecast = L_e_u_adj, RIC, value_gas = spot_forward_month_BL)]
        dt_gas_obs = react$forecast_params_field_gas$saved_history_gas[year(date) %in% unique(year(dt_gas_for$date)) & RIC == unique(dt_gas_for$RIC)][, .(date, spot = value, RIC)]
        dt_gas = merge(dt_gas_for, dt_gas_obs, by = c('date', 'RIC'), all = TRUE)
        
        setcolorder(dt_gas, c('date', 'RIC', 'spot', 'forecast', 'value_gas'))
        setorder(dt_gas, date)
        
        object_with_forecast_data_gas(dt_gas)
        
        print('------------- FORECAST END -------------')        
        print('')
        print('==================== ++++++++++++++ ====================')
        print('==================== END FORECASTING GAS ====================')
        print('==================== ++++++++++++++ ====================')
        print('')
        
    })    
    
    
    ## Download forecasts -----------------------
    
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
    
    
    
    # VISUALIZE ------------------------------------------
    
    ## TRAIN ------------------------------------
    
    ## RECAP FORECAST PARAMS
    output$forecast_params_table_recap_pwr = renderReactable({
        req(react$forecast_params_table_pwr)
        reactable(react$forecast_params_table_pwr,
                  defaultPageSize = 14)
    })
    
    output$forecast_params_table_recap_gas = renderReactable({
        req(react$forecast_params_table_gas)
        reactable(react$forecast_params_table_gas,
                  defaultPageSize = 14)
    })
    
    
    # Outputs for the selected history period (for training)
    output$pwr_history_plot <- renderEcharts4r({
        
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
            e_theme("westeros") 
        
    })
    
    output$pwr_history_table <- renderReactable({
        
        req(react$list_inputs_field_pwr)
        
        list_inputs = react$list_inputs_field_pwr
        DT = copy(list_inputs$ENV_SPOT$history_pwr)
        DT[, datetime := as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), format = "%Y-%m-%d %H:%M:%S", tz = "CET")]
        setorder(DT, datetime, RIC)
        reactable(DT,
                  filterable = TRUE)
    })
    
    
    output$gas_history_plot <- renderEcharts4r({
        
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
            e_theme("westeros") 
        
    })
    
    output$gas_history_table <- renderReactable({
        
        req(react$list_inputs_field_gas)
        
        list_inputs = react$list_inputs_field_gas
        DT = copy(list_inputs$ENV_SPOT$history_gas)
        setorder(DT, date, RIC)
        reactable(DT,
                  filterable = TRUE)
    })
    
    
    
    ## FORECAST ------------------------------------
    
    # Forecast plots for Power and Gas using echarts4r
    output$pwr_forecast_plot <- renderEcharts4r({
        
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
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
        
    })
    
    output$gas_forecast_plot <- renderEcharts4r({
        
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
            e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
            e_datazoom(start = 0) %>% 
            e_theme('westeros')
    })
    
    
    
    ## END
}


# RUN APP ------------------------------------------------------------------------------------------------- 
shinyApp(ui = ui, server = server)

