
library(data.table)
library(echarts4r)

dt_main = readRDS(file = 'inst/app_demo/DT_FWD.rds')
dt_proxy = readRDS(file = 'inst/app_demo/DT_FWD2.rds')


# dt1_avg = dt1[, .(value_main = mean(forecast)), by = .(date)]
# dt2_avg = dt2[, .(value_proxy = mean(forecast)), by = .(date)]
# 
# merged = merge(dt1_avg, dt2_avg, by = c("date"))
# 
# # saveRDS(merged, 'merged.rds')
# 
# merged[order(date)] %>%
#     e_charts(date) %>%
#     e_line(value_main, name = "Main", symbol = 'none') %>%
#     e_line(value_proxy, name = "Proxy", symbol = 'none') %>%
#     e_tooltip(trigger = "axis") %>%
#     e_title("FWD Curves") %>%
#     e_y_axis(name = "Price") %>%
#     e_x_axis(name = "Date") %>%
#     e_legend(top = 30) %>% 
#     e_datazoom(start = 0) %>% 
#     e_theme('westeros')

dt_sce = openxlsx::read.xlsx('inst/app_demo/HPFC/longterm/scenario_sample.xlsx', detectDates = TRUE)
dt_sce = data.table::as.data.table(dt_sce)
setorder(dt_sce, COMMODITY, DATE)
dt_sce[, DATE := as.Date(DATE) - lubridate ::years(5)]


dt_main = dt_main[, .(date, hour, value = forecast, source = 'MAIN')]
dt_proxy = dt_proxy[, .(date, hour, value = forecast, source = 'PROXY')]
dt_sce = dt_sce[, .(date = DATE, hour = HOUR, value = VALUE, source = 'SCENARIO')]

T1 = '2021-06-01'
T2 = '2023-06-01'

dt_main_part = dt_main[date < T1]
dt_proxy_part = dt_proxy[date >= T1 & date < T2]
dt_sce_part = dt_sce[date >= T2]

dt_final = rbindlist(list(dt_main_part, dt_proxy_part, dt_sce_part), use.names = TRUE)

dt_final[order(date)] |>
    group_by(source)|>
    e_charts(date) |>
    e_line(serie = value, symbol = "none", bind = source) |>
    e_tooltip(trigger = "axis") |>
    e_color(c("blue", "orange", "green")) |>
    e_title("Unified Forecast Curve") |>
    e_y_axis(name = "Price") |>
    e_x_axis(name = "Datetime") |>
    e_legend(FALSE)
