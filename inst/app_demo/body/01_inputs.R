# GENERAL ======================================================================

### RICS NAMES -----------------------------------------------------------------
vec_pwr_products =        eikondata::pwr_products_full$countries
vec_gas_products =        eikondata::gas_products_full$products_GAS
vec_basket = c(vec_pwr_products, vec_gas_products, 'CO2')
names(vec_pwr_products) = eikondata::pwr_products_full$countries
names(vec_gas_products) = eikondata::gas_products_full$products_GAS
names(vec_basket) = vec_basket


## SIM NAME -------------------------------------------------------------------
select_sim_name = 
    textInput(
        inputId = 'in_new_sim_name',
        label = 'Archive: Simulation Name',
        placeholder = 'Archiving simulation name here...'
    )

### TRAINING PERIOD -----------------------------------------------------------
select_history_period =
    dateRangeInput(
        inputId = "in_select_history",
        label = "Select history Interval:",
        start  = "2016-01-01",
        end    = "2024-12-31",
        min    = "2016-01-01",
        max    = Sys.Date(),
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )


### DATA SOURCE -------------------------------------
select_source_train =
    radioButtons(
        inputId = "in_source_train",
        label = "Select data source:",
        choices = c("Reuters",
                    "Excel"),
        selected = "Reuters",
        inline = TRUE
    )


### FORECASTING HORIZON --------------------------------------------------------
select_horizon_horizon =
    dateRangeInput(
        inputId = "in_select_horizon",
        label = "Select forecast horizon Interval:",
        start  = '2024-01-01',
        end    = '2024-12-31', 
        min    = '2017-01-01',
        max    = '2030-12-31',
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )

select_horizon_total =
    dateRangeInput(
        inputId = "in_select_total_horizon",
        label = "Select curve total horizon:",
        start  = '2024-01-01',
        end    = '2024-12-31', 
        min    = '2017-01-01',
        max    = '2030-12-31',
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )

### for DATA SOURCE -------------------------------------
select_source_forecast =
    radioButtons(
        inputId = "in_source_forecast",
        label = "Select data source:",
        choices = c("Reuters",
                    "Excel"),
        selected = "Reuters",
        inline = TRUE
    )


### for RUN SOURCE -------------------------------------
select_source_run =
    radioButtons(
        inputId = "in_source_run",
        label = "Select data source:",
        choices = c("Last",
                    "Sim"),
        selected = "Last",
        inline = TRUE
    )


# INPUTS Mutiple ------------------------------------------------------------------------------------------------- 

## Training ===========================================================================================

### SELECT PRODUCTS --------------------------------------------------------
select_PWR_product_mult =
    selectInput(
        inputId = "in_select_PWR_indicator_mult",
        label = "Power:",
        multiple = TRUE,
        width = '100%',
        choices = vec_pwr_products,
        selected = vec_pwr_products
    )

select_GAS_product_mult =
    selectInput(
        inputId = "in_select_GAS_indicator_mult",
        label = "Gas:",
        multiple = TRUE,
        width = '100%',
        choices = vec_gas_products,
        selected = vec_gas_products
    )

select_pwrplot_mult =
    selectInput(
        inputId = "in_select_pwrplot_mult",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_gasplot_mult =
    selectInput(
        inputId = "in_select_gasplot_mult",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )


#### BUTTON TO EXECUTE TRAINING --------------------------------
product_train_pwr_mult =
    input_task_button(
        id = 'act_indicator_train_pwr_mult',
        label = 'Train Power model',
        label_busy = "Training...",
        icon = shiny::icon('person-running'),
        width = '100%',
        type = "danger"
    )


product_train_gas_mult =
    input_task_button(
        id = 'act_indicator_train_gas_mult',
        label = 'Train Gas model',
        label_busy = "Training...",
        icon = shiny::icon('person-running'),
        width = '100%',
        type = "warning"
    )

## Forecast ====================================================================================

select_PWR_product_for_mult =
    selectInput(
        inputId = "in_select_PWR_indicator_for_mult",
        label = "Power:",
        multiple = TRUE,
        width = '100%',
        choices = vec_pwr_products,
        selected = vec_pwr_products
    )

select_GAS_product_for_mult =
    selectInput(
        inputId = "in_select_GAS_indicator_for_mult",
        label = "Gas:",
        multiple = TRUE,
        width = '100%',
        choices = vec_gas_products,
        selected = vec_gas_products
    )

select_pwrplot_mult_for =
    selectInput(
        inputId = "in_select_pwrplot_mult_for",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_gasplot_mult_for =
    selectInput(
        inputId = "in_select_gasplot_mult_for",
        label = NULL,
        multiple = FALSE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )

#### BUTTON TO EXECUTE FORECAST --------------------------------
product_forecast_pwr_mult =
    input_task_button(
        id = 'act_indicator_forecast_pwr_mult',
        label = 'Forecast Power',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "danger"
    )

product_forecast_gas_mult =
    input_task_button(
        id = 'act_indicator_forecast_gas_mult',
        label = 'Forecast Gas',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "warning"
    )




# INPUTS Single ------------------------------------------------------------------------------------------------- 

## Training ===========================================================================================

### SELECT PRODUCTS --------------------------------------------------------
select_PWR_product =
    selectInput(
        inputId = "in_select_PWR_indicator",
        label = "Power:",
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_GAS_product =
    selectInput(
        inputId = "in_select_GAS_indicator",
        label = "Gas:",
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
        icon = shiny::icon('person-running'),
        width = '100%',
        type = "danger"
    )


product_train_gas =
    input_task_button(
        id = 'act_indicator_train_gas',
        label = 'Train Gas model',
        label_busy = "Training...",
        icon = shiny::icon('person-running'),
        width = '100%',
        type = "warning"
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

select_PWR_product_for =
    selectInput(
        inputId = "in_select_PWR_indicator_for",
        label = "Power:",
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_GAS_product_for =
    selectInput(
        inputId = "in_select_GAS_indicator_for",
        label = "Gas:",
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



## Autobasket =======================================
select_main_product =
    selectInput(
        inputId = "in_select_main_product",
        label = "Power:",
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_basket =
    selectizeInput(
        inputId = "in_select_basket",
        label = "Basket:",
        multiple = TRUE,
        width = '100%',
        options = list(maxItems = 4),
        choices = vec_basket,
        selected = c('Germany', 'TTF', 'CO2')
    )

product_basket_lt =
    input_task_button(
        id = 'act_product_basket_lt',
        label = 'Download & Estimate',
        label_busy = "Processing...",
        icon = shiny::icon('rain'),
        width = '100%',
        type = "info"
    )

generate_fwd_curves =
    input_task_button(
        id = 'act_generate_fwd_curves',
        label = 'Generate FWD Curve',
        label_busy = "Processing...",
        icon = shiny::icon('download'),
        width = '100%',
        type = "info"
    )

select_source_weights =
    radioButtons(
        inputId = "in_select_source_weights",
        label = "Select weights:",
        choices = c("Auto",
                    "Manual"),
        selected = "Auto",
        inline = TRUE
    )

select_lt_horizon =
    dateRangeInput(
        inputId = "in_select_lt_horizon",
        label = "Select LT Horizon Interval:",
        start  = Sys.Date(),
        end    = "2029-12-31",
        min    = "2016-01-01",
        max    = '2035-12-31',
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )

select_lt_train =
    dateRangeInput(
        inputId = "in_select_lt_train",
        label = "Basket Train period:",
        start  = "2016-01-01",
        end    = Sys.Date(),
        min    = "2016-01-01",
        max    = '2035-12-31',
        format = "yyyy/mm/dd",
        separator = " - ",
        width = '100%'
    )

upload_scenario =
    fileInput(
        inputId = "in_load_scenario",
        label = "Excel file with Scenario data",
        accept = c(".xlsx", ".xls"),
        multiple = FALSE
    )

select_cutoff_mkt =
    dateInput(
        inputId = 'in_select_cutoff_mkt',
        label = 'Select cut-off Main vs Basket',
        value = Sys.Date() + (365*2)
    )

select_cutoff_sce =
    dateInput(
        inputId = 'in_select_cutoff_sce',
        label = 'Select cut-off Market vs Scenario',
        value = Sys.Date() + (365*4)
    )

product_create_lt =
    input_task_button(
        id = 'act_product_create_lt',
        label = 'Forecast LT',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "warning"
    )


manual_wg_basket_1 = 
    numericInput(
        inputId = "wg_1", 
        label = "Com.1", 
        value = 100,
        step = 1
    )
manual_wg_basket_2 = 
    numericInput(
        inputId = "wg_2", 
        label = "Com.2", 
        value = 100,
        step = 1
    )
manual_wg_basket_3 = 
    numericInput(
        inputId = "wg_3", 
        label = "Com.3", 
        value = 100,
        step = 1
    )
manual_wg_basket_4 = 
    numericInput(
        inputId = "wg_4", 
        label = "Com.4", 
        value = 100,
        step = 1
    )

lt_pwr_download =
    downloadButton(
        outputId = 'act_lt_pwr_download',
        label = 'Curve Download',
        icon = shiny::icon('download'),
        style = "width:100%;",
        class = "btn-secondary"
    )