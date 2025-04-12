# GENERAL ======================================================================

### RICS NAMES -----------------------------------------------------------------
vec_pwr_products =        HPFC::spot_PWR_products_full$countries
names(vec_pwr_products) = HPFC::spot_PWR_products_full$countries
vec_gas_products =        HPFC::spot_GAS_products_full$products_GAS
names(vec_gas_products) = HPFC::spot_GAS_products_full$products_GAS

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
        selected = 'Greece'
    )

select_GAS_product_mult =
    selectInput(
        inputId = "in_select_GAS_indicator_mult",
        label = "Gas:",
        multiple = TRUE,
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

select_PWR_product_for_multi =
    selectInput(
        inputId = "in_select_PWR_indicator_for_multi",
        label = "Power:",
        multiple = TRUE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )

select_GAS_product_for_multi =
    selectInput(
        inputId = "in_select_GAS_indicator_for_multi",
        label = "Gas:",
        multiple = TRUE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )

#### BUTTON TO EXECUTE FORECAST --------------------------------
product_forecast_pwr_multi =
    input_task_button(
        id = 'act_indicator_forecast_pwr_multi',
        label = 'Forecast Power',
        label_busy = "Forecasting...",
        icon = shiny::icon('eye'),
        width = '100%',
        type = "danger"
    )

product_forecast_gas_multi =
    input_task_button(
        id = 'act_indicator_forecast_gas_multi',
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
