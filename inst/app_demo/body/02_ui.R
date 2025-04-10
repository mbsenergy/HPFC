# UI ------------------------------------------------------------------------------------------------- 

ui_app = page_navbar(
    
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
    
    nav_menu(title = 'HPFC', 
             
             nav_panel(title = 'TRAIN',
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             width = 400,
                                             accordion(class = 'padding: 20px',
                                                       accordion_panel(
                                                           title = 'Training All'
                                                       ),
                                                       accordion_panel(
                                                           title = 'Training Single',
                                                           select_sim_name,
                                                           hr(),
                                                           select_history_period,
                                                           select_PWR_product,
                                                           select_GAS_product,
                                                           select_source_train,
                                                           uiOutput("select_source_file_train"),
                                                           product_train_pwr,
                                                           product_train_gas,
                                                           hr(),
                                                           fluidRow(train_pwr_download, train_gas_download)
                                                       )
                                             )
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
             
             nav_spacer(),
             
             nav_panel(title = 'FORECAST',
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             width = 400, 
                                             accordion(class = 'padding: 20px',
                                                       accordion_panel(
                                                           title = 'Forecast All'
                                                       ),
                                                       accordion_panel(
                                                           title = 'Forecasting',
                                                           select_source_run,
                                                           uiOutput("select_source_run"),
                                                           hr(),
                                                           select_horizon_period,
                                                           select_PWR_product_for,
                                                           select_GAS_product_for,
                                                           select_source_forecast,
                                                           uiOutput("select_source_file_forecast_pwr"),
                                                           uiOutput("select_source_file_forecast_gas"),
                                                           product_forecast_pwr,
                                                           product_forecast_gas,
                                                           hr(),
                                                           fluidRow(fwd_pwr_download, fwd_gas_download)
                                                       )
                                             )
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
             ),
             
             nav_spacer(),
             
             nav_panel(title = 'BACKTESTING',
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             width = 400, padding = '40'
                           ),
                           fluidPage()
                       )
             )
             
    )
)


