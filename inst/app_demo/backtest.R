# PREPARE -----------------------------------------
box::use(
    data.table[...],
    magrittr[...],
    echarts4r[...],
    reactable[...],
    HPFC[...],
    eikondata[...],
    openxlsx[...]
)
devtools::load_all()

pv_profile = readRDS('www/consumption_profiles.rds')

in_select_PWR_backtest = 'Ser'
in_select_backtest_source = 'Last'
x = in_select_PWR_backtest
in_select_backtest_period_1 = '2023-01-01'
in_select_backtest_period_2 = '2024-12-31'
shiny_run = in_select_backtest_source
shiny_sim = NULL

pwr_codes = eikondata::pwr_products_full[countries %in% in_select_PWR_backtest]$spot_PWR_code
history_pwr_full = eikondata::dt_spot_pwr[RIC == pwr_codes]

trial = run_forecast_backtest_pwr(
    x,
    in_select_backtest_source = 'Last',
    in_select_backtest_period_1,
    in_select_backtest_period_2,
    shiny_sim = NULL,
    reuters_key = Sys.getenv('REUTERS_KEY')
)
trial$dt_pwr

trial$dt_pwr_lg %>% 
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


dt_pwr = trial$dt_pwr_lg
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

error_pv = merge(dt_pwr_error[, .(date, hour, ERROR)], pv_profile, by = c('date', 'hour'), all.x = TRUE)
error_pv[, .(cp_obs = sum(ERROR * pv, na.rm = TRUE))]

e_charts() |> 
    e_gauge(round(, 2), "ERROR") |> 
    e_title("Forecast Mean Error")

## DATAPOINTS DISTRIBUTION
dt_plot = dt_pwr_error[
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



dt_pwr = dt_pwr[type %in% c('spot', 'forecast')]
## week plot
dts = dt_pwr[, .(value = round(mean(value))), by = .(wday, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(wday) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Weekly Curve") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 

## daily
dts = dt_pwr[, .(value = round(mean(value))), by = .(hour, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(hour) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Daily Curve (24h)") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 


## season winter
dts = dt_pwr[year(date) == 2024 & season == 'Winter', .(value = round(mean(value))), by = .(hour, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(hour) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Daily Curve (24h)") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 

## season spring
dts = dt_pwr[year(date) == 2024 & season == 'Spring', .(value = round(mean(value))), by = .(hour, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(hour) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Daily Curve (24h)") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 


## season summer
dts = dt_pwr[year(date) == 2024 & season == 'Summer', .(value = round(mean(value))), by = .(hour, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(hour) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Daily Curve (24h)") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 


## season fall
dts = dt_pwr[year(date) == 2024 & season == 'Fall', .(value = round(mean(value))), by = .(hour, type)] 
dts %>% 
    group_by(type) %>% 
    e_charts(hour) %>% 
    e_line(value, smooth = TRUE) %>% 
    e_legend(show = TRUE, orient = 'horizontal') %>% 
    e_title("Mean Daily Curve (24h)") %>% 
    e_tooltip(trigger = "axis") %>% 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0) 




## error metrics
dt_pwr_error[, .(
    MEAN = round(mean(ERROR, na.rm = TRUE), 2),
    MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
    SD = round(sd(ERROR, na.rm = TRUE), 2),
    IQR = round(IQR(ERROR, na.rm = TRUE), 2),
    Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
    Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
), by = season] %>% 
    knitr::kable()

dt_pwr_error[order(wday), .(
    MEAN = round(mean(ERROR, na.rm = TRUE), 2),
    MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
    SD = round(sd(ERROR, na.rm = TRUE), 2),
    IQR = round(IQR(ERROR, na.rm = TRUE), 2),
    Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
    Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
), by = wday] %>% 
    knitr::kable()

dt_pwr_error[, .(
    MEAN = round(mean(ERROR, na.rm = TRUE), 2),
    MEDIAN = round(median(ERROR, na.rm = TRUE), 2),
    SD = round(sd(ERROR, na.rm = TRUE), 2),
    IQR = round(IQR(ERROR, na.rm = TRUE), 2),
    Qn1 = round(quantile(ERROR, 0.25, na.rm = TRUE), 2),
    Qn3 = round(quantile(ERROR, 0.75, na.rm = TRUE), 2)
), by = peak] %>% 
    knitr::kable()



## error bias 
dt_pwr_error[, .(
    MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
), by = .(season)] %>% 
    knitr::kable()

dt_pwr_error[, .(
    MAE = round(mean(abs(ERROR), na.rm = TRUE), 2)
), by = .(season, DIR)] %>% dcast(season ~ DIR) %>% 
    knitr::kable()