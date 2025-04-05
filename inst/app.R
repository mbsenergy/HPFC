
# SETUP ------------------------------------------------------------------------------------------------- 

library(shiny)
library(shinyjs)
library(bslib)        # for bslib theme and components
library(echarts4r)     # for interactive charts
library(reactable)     # for interactive tables
library(shinycssloaders) # for loading spinner
library(react)
library(HPFC)


hpfc_theme =
    bs_theme(
        version = 5,
        bootswatch = 'zephyr',
        primary = '#287bb5',
        secondary = '#dee2e6',
        success = '#2fb380',
        info = 'steelblue',
        warning = '#f4bd61',
        danger = '#d08770',
        base_font = font_google(family = "Inter"),
        heading_font = font_google(family = "Inter"),
        font_scale = 0.9
    )


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
product_train =
    actionButton(
        inputId = 'act_indicator_train',
        label = 'Train model',
        icon = shiny::icon('backward'),
        width = '100%',
        class = "btn-danger"
    )

#### DATA SOURCE --------------------------------
select_source =
    radioButtons(
        inputId = "in_source",
        label = span("Select data source:", style = 'font-weight: bold;'),
        choices = c("Reuters" = "0df86b690b2c4ae2bf245680dbbfcc86bb041dc9",
                    "Excel" = 'LOCAL'),
        inline = FALSE
    )

### BUTTONS EXECUTE DOWNLOAD -----------------------
spot_pwr_download =
    downloadButton(
        outputId = 'act_spot_pwr_download',
        label = 'Power Download',
        icon = shiny::icon('download'),
        style = "width:50%;",
        class = "btn-secondary"
    )

spot_gas_download =
    downloadButton(
        outputId = 'act_spot_gas_download',
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

### SELECT PRODUCTS --------------------------------------------------------

select_PWR_product_train =
    selectInput(
        inputId = "in_select_PWR_indicator_train",
        label = span("Power", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_pwr_products,
        selected = 'Greece'
    )


select_GAS_product_train =
    selectInput(
        inputId = "in_select_GAS_indicator_train",
        label = span("Gas:", style = 'font-weight: bold;'),
        multiple = FALSE,
        width = '100%',
        choices = vec_gas_products,
        selected = 'TTF'
    )


#### BUTTON TO EXECUTE FORECAST --------------------------------
product_forecast =
    actionButton(
        inputId = 'act_indicator_forecast',
        label = 'Forecast',
        icon = shiny::icon('eye'),
        width = '100%',
        class = "btn-warning"
    )


#### DATA SOURCE --------------------------------
select_source_forecast =
    radioButtons(
        inputId = "in_source_forecast",
        label = span("Select data source:", style = 'font-weight: bold;'),
        choices = c("Reuters" = "0df86b690b2c4ae2bf245680dbbfcc86bb041dc9",
                    "Excel" = 'LOCAL'),
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
    theme = hpfc_theme,
    
    # Title Panel of the app
    title = "HPFC App",
    
    bg = '#287bb5',
    
    # Tabs in the NavbarPage (Train and Forecast)
    nav_panel(title = 'TRAIN',
        layout_sidebar(
                   sidebar = sidebar(
                       width = 400, padding = '40',
                       title = 'Training',
                             select_history_period,
                             select_PWR_product_train,
                             select_GAS_product_train,
                             br(),
                             product_train,
                             br(),
                             select_source,
                             uiOutput("reactive_select_source_file"),
                             hr(),
                             fluidRow(spot_pwr_download, spot_gas_download),
                             br()
                        ),
                 
                 # Main Panel for the training
                        navset_card_pill(
                            full_screen = TRUE,
                            height = '845px',
                            nav_panel('Power', class = 'p-4',
                                fluidRow(
                                    echarts4rOutput(outputId = 'pwr_history_plot', height = '400px') %>% withSpinner(color = "#d08770"),
                                    hr(), br(),
                                    reactableOutput(outputId = 'pwr_history_table') %>% withSpinner(color = "#d08770")
                                )
                            ),
                            
                            nav_panel('Gas', class = 'p-4',
                                fluidRow(
                                    echarts4rOutput(outputId = 'gas_history_plot', height = '400px') %>% withSpinner(color = "#d08770"),
                                    hr(), br(),
                                    reactableOutput(outputId = 'gas_history_table') %>% withSpinner(color = "#d08770")
                                )
                            )
                        )
        )
    ),
    
    nav_panel(title = 'FORECAST',
        layout_sidebar(
                   sidebar = sidebar(
                       width = 400, padding = '40',
                       title = 'Forecasting',
                             select_horizon_period,
                             select_PWR_product,
                             select_GAS_product,
                             br(),
                             product_forecast,
                             br(),
                             select_source_forecast,
                             uiOutput("reactive_select_source_file_forecast"),
                             hr(),
                             fluidRow(fwd_gas_download, fwd_pwr_download),
                             br()
                        ),
                 
                 # Main Panel for the forecast
                        navset_card_pill(
                            full_screen = TRUE,
                            height = '845px',
                            nav_panel('Power', class = 'p-1',
                                fluidRow(
                                    h5('Historical Power prices'),
                                    echarts4rOutput(outputId = 'pwr_historysaved_plot', height = '300px') %>% withSpinner(color = "#d08770")
                                ),
                                hr(), br(),
                                fluidRow(
                                    column(width = 4, h5('Forecast Power prices')),
                                    column(width = 4, plot_forecast_selector_pwr),
                                    column(width = 4, p(''))
                                ),
                                fluidRow(
                                    echarts4rOutput(outputId = 'pwr_forecast_plot', height = '300px') %>% withSpinner(color = "#d08770")
                                )
                            ),
                            
                            nav_panel('Gas', class = 'p-1',
                                fluidRow(
                                    h5('Historical Gas prices'),
                                    echarts4rOutput(outputId = 'gas_historysaved_plot', height = '300px') %>% withSpinner(color = "#d08770")
                                ),
                                hr(), br(),
                                fluidRow(
                                    column(width = 4, h5('Forecast Gas prices')),
                                    column(width = 4, plot_forecast_selector_gas),
                                    column(width = 4, p(''))
                                ),
                                fluidRow(
                                    echarts4rOutput(outputId = 'gas_forecast_plot', height = '300px') %>% withSpinner(color = "#d08770")
                                )
                            )
                        )
        )
             ),
    
    nav_panel(title = "BACKTEST",
             fluidRow('PLACEHOLDER')
             ),
    
    nav_panel(title = "RECAP",
             fluidRow(
                 column(12,
                        h3("Input Recap"),
                        reactableOutput("input_recap_table")
                 )
             )
    )
)




# SERVER  ------------------------------------------------------------------------------------------------- 
server = function(input, output, session) {
    
    # Reactive input recap data -----------------
    recap_data = reactive({
        data.frame(
            Input = c("History Period", "Power Products (Training)", "Gas Products (Training)", "Data Source (Training)",
                      "Forecast Horizon Period",  "Power Products (Forecast)", "Gas Products (Forecast)", "Data Source (Forecast)"),
            Value = c(paste(input$in_select_history[1], "to", input$in_select_history[2]),
                      paste(input$in_select_PWR_indicator_train, collapse = ", "),
                      paste(input$in_select_GAS_indicator_train, collapse = ", "),
                      input$in_source,
                      paste(input$in_select_horizon[1], "to", input$in_select_horizon[2]),
                      paste(input$in_select_PWR_indicator, collapse = ", "),
                      paste(input$in_select_GAS_indicator, collapse = ", "),
                      input$in_source_forecast)
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
    observe({
        params_list = list(
            model_type = 'PWR',
            selected_pwr_code = input$in_select_PWR_indicator_train,
            selected_gas_code = 'TTF',
            dependent_gas_code = 'TTF',
            history_start = input$in_select_history[1],
            history_end = input$in_select_history[2],
            forecast_start = input$in_select_horizon[1],
            forecast_end = input$in_select_horizon[2],
            model_source = 'TRAIN',
            data_source = input$in_source, #0df86b690b2c4ae2bf245680dbbfcc86bb041dc9
            forecast_source = 'FWD',
            sim_name = 'NO',
            archive = 'NO'
        )
        
        print(params_list)
        params_input_pwr(params_list)
    })
    
    observeEvent(input$act_indicator_train, {
        print('-------------START TRAINING -------------------')
        LST_PARAMS = react$params_input_pwr
        list_inputs = HPFC::load_inputs(params = LST_PARAMS)
        print(LST_PARAMS)
    })
    
    
    # Reactive input for select source in training period
    output$reactive_select_source_file <- renderUI({
        req(input$in_source)
        if(input$in_source == "Excel") {
            fileInput("file", "Upload Excel File", accept = c(".xlsx"))
        }
    })
    
    # Reactive input for select source in forecast period
    output$reactive_select_source_file_forecast <- renderUI({
        req(input$in_source_forecast)
        if(input$in_source_forecast == "Excel") {
            fileInput("file_forecast", "Upload Excel File", accept = c(".xlsx"))
        }
    })
    
    # Outputs for the selected history period (for training)
    output$pwr_history_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date()-30, Sys.Date(), by="days")) %>%
            e_line(rnorm(30)) %>%
            e_title("Power Price History") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
    
    output$gas_history_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date()-30, Sys.Date(), by="days")) %>%
            e_line(rnorm(30)) %>%
            e_title("Gas Price History") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
    
    # Sample Reactable tables (for demonstration)
    output$pwr_history_table <- renderReactable({
        reactable(data.frame(Date = seq.Date(Sys.Date()-30, Sys.Date(), by="days"), Power_Price = rnorm(30)))
    })
    
    output$gas_history_table <- renderReactable({
        reactable(data.frame(Date = seq.Date(Sys.Date()-30, Sys.Date(), by="days"), Gas_Price = rnorm(30)))
    })
    
    # Forecast plots for Power and Gas using echarts4r
    output$pwr_forecast_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date(), Sys.Date()+30, by="days")) %>%
            e_line(rnorm(30)) %>%
            e_title("Power Price Forecast") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
    
    output$gas_forecast_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date(), Sys.Date()+30, by="days")) %>%
            e_line(rnorm(30)) %>%
            e_title("Gas Price Forecast") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
    
    # Saved History Plots using echarts4r
    output$pwr_historysaved_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date()-60, Sys.Date(), by="days")) %>%
            e_line(rnorm(60)) %>%
            e_title("Saved Power Price History") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
    
    output$gas_historysaved_plot <- renderEcharts4r({
        e_charts(seq.Date(Sys.Date()-60, Sys.Date(), by="days")) %>%
            e_line(rnorm(60)) %>%
            e_title("Saved Gas Price History") %>%
            e_x_axis(name = "Date") %>%
            e_y_axis(name = "Price")
    })
}


# RUN APP ------------------------------------------------------------------------------------------------- 
shinyApp(ui = ui, server = server)

