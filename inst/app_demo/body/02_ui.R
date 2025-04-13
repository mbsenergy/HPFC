# UI ------------------------------------------------------------------------------------------------- 

ui_app = page_navbar(
    theme = mbs_theme,
    navbar_options = navbar_options(bg = "#001437", underline = FALSE, collapsible = TRUE, theme = 'dark'),
    
    # Title Panel of the app
    title = "HPFC",
    
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
             
             nav_panel(title = h2('TRAIN'),
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             title = 'Train',
                                             width = 400,
                                             select_sim_name,
                                             select_history_period,
                                             select_source_train,
                                             uiOutput("select_source_file_train_pwr"),
                                             uiOutput("select_source_file_train_gas"),
                                             accordion(
                                                       accordion_panel(
                                                           title = 'Training Multiple',
                                                           select_PWR_product_mult,
                                                           select_GAS_product_mult,
                                                           product_train_pwr_mult,
                                                           br(),
                                                           product_train_gas_mult
                                                       ),
                                                       accordion_panel(
                                                           title = 'Training Single',
                                                           select_PWR_product,
                                                           select_GAS_product,
                                                           product_train_pwr,
                                                           br(),
                                                           product_train_gas,
                                                           hr(),
                                                           fluidRow(train_pwr_download, train_gas_download)
                                                       )
                                             )
                           ),
                           
                           # Main Panel for the training
                           navset_card_pill(
                               nav_panel('Power Multiple',
                                         fluidRow(
                                             select_pwrplot_mult,
                                             card(full_screen = TRUE,
                                                  card_body(
                                                 echarts4rOutput(outputId = 'pwr_history_plot_mult') %>% withSpinner(color = "#F2606A")
                                                 )),
                                                 card(full_screen = TRUE,
                                                      card_body(
                                                 datagridOutput(outputId = 'pwr_history_table_mult') %>% withSpinner(color = "#F2606A")
                                             ))
                                         )
                               ),
                               nav_panel('Gas Multiple',
                                         fluidRow(
                                             select_gasplot_mult,
                                             fluidRow( 
                                                 echarts4rOutput(outputId = 'gas_history_plot_mult') %>% withSpinner(color = "#F2606A"),
                                                 hr(), br(),
                                                 datagridOutput(outputId = 'gas_history_table_mult') %>% withSpinner(color = "#F2606A")
                                             )
                                         )
                               ),
                               nav_panel('Power Single',
                                         layout_sidebar(
                                             sidebar = sidebar(bg = 'white',
                                                               datagridOutput('forecast_params_table_recap_pwr'), position = 'right', open = FALSE, width = '450px'),
                                             card(full_screen = TRUE,
                                                  card_body(
                                                 echarts4rOutput(outputId = 'pwr_history_plot') %>% withSpinner(color = "#F2606A")
                                                 )),
                                             card(full_screen = TRUE,
                                                  card_body(
                                                 datagridOutput(outputId = 'pwr_history_table') %>% withSpinner(color = "#F2606A")
                                                  ))
                                         )
                               ),
                               
                               nav_panel('Gas Single',
                                         layout_sidebar(
                                             sidebar = sidebar(bg = 'white',
                                                               datagridOutput('forecast_params_table_recap_gas'), position = 'right', open = FALSE, width = '450px'),
                                             card(full_screen = TRUE,
                                                  card_body(
                                                 echarts4rOutput(outputId = 'gas_history_plot') %>% withSpinner(color = "#F2606A")
                                                 )),
                                             card(full_screen = TRUE,
                                                  card_body(
                                                 datagridOutput(outputId = 'gas_history_table') %>% withSpinner(color = "#F2606A")
                                             ))
                                         )
                               )
                           )
                       )
             ),
             
             nav_panel(title = h2('FORECAST'),
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             title = 'Forecast',
                                             width = 400, 
                                             select_source_run,
                                             uiOutput("select_source_run"),
                                             select_horizon_horizon,
                                             select_source_forecast,
                                             uiOutput("select_source_file_forecast_pwr"),
                                             uiOutput("select_source_file_forecast_gas"),
                                             accordion(
                                                       accordion_panel(
                                                           title = 'Forecast Multiple',
                                                           select_PWR_product_for_mult,
                                                           select_GAS_product_for_mult,
                                                           product_forecast_pwr_mult,
                                                           br(),
                                                           product_forecast_gas_mult,                                                           
                                                       ),
                                                       accordion_panel(
                                                           title = 'Forecasting Single',
                                                           select_PWR_product_for,
                                                           select_GAS_product_for,
                                                           product_forecast_pwr,
                                                           br(),
                                                           product_forecast_gas,
                                                           hr(),
                                                           fluidRow(fwd_pwr_download, fwd_gas_download)
                                                       )
                                             )
                           ),
                           
                           # Main Panel for the forecast
                           navset_card_pill(
                               nav_panel('Power Multiple',
                                         fluidRow(
                                             echarts4rOutput(outputId = 'pwr_forecast_plot_mult') %>% withSpinner(color = "#C05B8C")
                                         )
                               ),
                               
                               nav_panel('Gas Multiple',
                                         fluidRow(
                                             echarts4rOutput(outputId = 'gas_forecast_plot_mult') %>% withSpinner(color = "#C05B8C")
                                         )
                               ),                               
                               nav_panel('Power Single',
                                         fluidRow(
                                             echarts4rOutput(outputId = 'pwr_forecast_plot') %>% withSpinner(color = "#C05B8C")
                                         )
                               ),
                               
                               nav_panel('Gas Single',
                                         fluidRow(
                                             echarts4rOutput(outputId = 'gas_forecast_plot') %>% withSpinner(color = "#C05B8C")
                                         )
                               )
                           )
                       )
             ),
             
             nav_panel(title = h2('BACKTESTING'),
                       layout_sidebar(
                           sidebar = sidebar(bg = 'white',
                                             width = 400, padding = '40'
                           ),
                           fluidPage()
                       )
             )
             
    ),
    
    nav_spacer(),
    
        nav_item(textOutput("reuters_status"))
)


