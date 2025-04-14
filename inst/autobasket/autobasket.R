
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

as.data.table(list_autobasket$coef_glm)
