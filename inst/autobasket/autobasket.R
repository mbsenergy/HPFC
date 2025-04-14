
## Library ----------------------------------------
library(data.table)
library(HPFC)
library(eikondata)

commodity_main = 'Italy'
commodity_basket = c('Germany', 'Spain', 'C02', 'TTF')
start_horizon = '2016-01-01'
end_horizon = '2025-03-31'

## Generate continuation codes ----------------------------------------
list_cont_codes = eikondata::products_continuation


## Get data  ----------------------------------------
eikondata::set_proxy_port(9000L)
eikondata::set_app_id(Sys.getenv('REUTERS_KEY'))

list_cont = lapply(list_cont_codes$c1, function(x) {
    tryCatch({
        print(x)
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


list_autobasket = basket_selection(DT = dt_cont, mk_comm_0 = commodity_main, basket = commodity_basket)

coef_glm = as.data.table(list_autobasket$coef_glm)

# Merge and weight time series
dt_plot = merge(dt_cont, coef_glm[, .(RIC, weight)], by = "RIC")
dt_plot[, weighted_value := VALUE * weight]

dt_plot_main = dt_cont[COMMODITY == commodity_main, .(COMMODITY = commodity_main, VALUE = sum(VALUE)), by = 'DATE']
dt_plot_proxy = dt_plot[, .(COMMODITY = 'BASKET', VALUE = sum(weighted_value)), by = 'DATE']
dt_plot_basket = dt_plot[COMMODITY %in% commodity_basket, .(DATE, COMMODITY, VALUE)]

# Combine data
dt_all = rbindlist(list(dt_plot_main, dt_plot_proxy, dt_plot_basket), use.names = TRUE, fill = TRUE)

# Plot
dt_all |>
    group_by(COMMODITY) |>
    e_charts(DATE) |>
    e_line(VALUE, bind = COMMODITY) |>
    e_color(c("blue", "#C05B8C", rep("lightgray", length(unique(dt_plot_basket$COMMODITY))))) |>
    e_tooltip(trigger = "axis")




library(data.table)
library(openxlsx)

# Create hourly datetime sequence for 1 year
dt = data.table(datetime = seq.POSIXt(from = as.POSIXct("2025-01-01 00:00:00"),
                                      to   = as.POSIXct("2030-12-31 23:00:00"),
                                      by   = "hour"))

# Extract DATE and HOUR
dt[, DATE := as.Date(datetime)]
dt[, HOUR := hour(datetime)]

# Assign random COMMODITY and generate VALUE
commodities = c("Greece")
dt[, COMMODITY := sample(commodities, .N, replace = TRUE)]
dt[, VALUE := round(rnorm(.N, mean = 100, sd = 20), 2)]

# Select and order columns
dt_final = dt[, .(DATE, HOUR, COMMODITY, VALUE)]

# Write to Excel
openxlsx::write.xlsx(dt_final, file = "scenario_sample.xlsx")