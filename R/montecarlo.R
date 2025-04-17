#' Compute daily volatility (sigma) from power spot price data
#'
#' @param DT A `data.table` containing at least `date` and `value` columns.
#' @return A numeric scalar with the standard deviation of log-returns.
compute_sigma = function(DT) {
    dt_sigma = DT[, .(value = mean(value, na.rm=TRUE)), by=.(date)]
    setorder(dt_sigma, date)
    dt_sigma[, value_t1 := value]
    dt_sigma[, value_t0 := shift(value)]
    dt_sigma[, ret := log(value_t1 / value_t0)]
    dt_sigma = dt_sigma[!is.na(ret)]
    sigma = sd(dt_sigma$ret)
    return(sigma)
}

#' Simulate a single random walk price path
#'
#' @param S0 Initial price.
#' @param sigma Daily volatility.
#' @param Td Number of time steps (days).
#' @param N Optional. Simulation ID (used for naming the output column).
#' @return A `data.table` with `time_index` and simulated price path.
simulate_single_path = function(S0, sigma, Td, N = NULL) {
    shocks = rnorm(Td, mean=0, sd=sigma)
    path = c(S0, S0 + cumsum(shocks))
    DT = data.table(time_index=0:Td, price=path)
    if (!is.null(N)) {
        setnames(DT, names(DT), c('time_index', paste0('sim_', N)))
    }
    return(DT)
}

#' Simulate multiple random walk price paths
#'
#' @param S0 Initial price.
#' @param sigma Daily volatility.
#' @param Td Number of time steps (days).
#' @param N Number of simulations to run.
#' @importFrom  crayon green
#' @return A `data.table` with `time_index` and simulated price columns.
simulate_random_walk = function(S0, sigma, Td, N) {
    paths_list = lapply(1:N, function(i) simulate_single_path(S0, sigma, Td, N = i))
    merged_paths = Reduce(function(x, y) merge(x, y, by="time_index", all=TRUE), paths_list)
    cat(crayon::green$bold(paste0("\n✔ Monte Carlo simulation completed with ", N, " paths and σ = ", round(sigma, 4))), "\n")
    return(merged_paths)
}

#' Run Monte Carlo simulation for power prices
#'
#' @param S0 Initial price.
#' @param Td Time horizon in days.
#' @param N Number of simulation paths.
#' @param dt_spot_pwr `data.table` with historical spot power prices (must include `date`, `value`).
#' @param dt_fwd_pwr Optional. `data.table` with forward curve to append to the result.
#' @param seed Random seed (default is 42).
#' @param aux_sigma Optional. Override computed volatility with a user-supplied value.
#'
#' @return A `data.table` with monthly average simulated prices and optionally the forward curve.
#' @export
montecarlo_sim = function(S0, Td, N, dt_spot_pwr, dt_fwd_pwr = NULL, seed = 42, aux_sigma = NULL) {
    
    set.seed(seed)
    
    if(aux_sigma != 0) {
        sigma = aux_sigma
    } else {
        sigma = exp(compute_sigma(dt_spot_pwr))
    }
    
    simulated_prices = simulate_random_walk(S0, sigma, Td, N)
    
    dtw = melt(simulated_prices, id.vars="time_index", variable.name = "sim", value.name = "value") %>% as.data.table()
    dtw[, date := as.Date("2024-01-01") + time_index]
    dtw[, yymm := as.Date(paste0(format(date, "%Y-%m"), "-01"))]
    dts = dtw[, .(value = round(mean(value, na.rm=TRUE))), by=.(sim, yymm)]
    setorder(dts, sim, yymm)
    
    if (!is.null(dt_fwd_pwr)) {
        dts = rbind(dt_fwd_pwr, dts, fill=TRUE)
    }
    
    return(dts)
}


#' Apply Forecast Shaping to Power Prices
#'
#' Combines forward and spot market data for power and gas, along with pre-trained models,
#' to generate shaped hourly forecasts for power prices over a specified time window.
#'
#' @param country Country name (e.g., `"Italy"`). Used to extract the relevant PWR product code and holidays.
#' @param name Name identifier for the output scenario (default `"HPFC"`).
#' @param start_date Start date of the forecast period (e.g., `"2024-01-01"`).
#' @param end_date End date of the forecast period (e.g., `"2024-12-31"`).
#' @param dt_fwd_pwr `data.table`. Monthly forward power prices. Must include columns `yymm`, `value`.
#' @param dt_spot_pwr `data.table`. Historical hourly spot power prices. Must include `date`, `hour`, `value`, `RIC`.
#' @param dt_fwd_gas `data.table`. Monthly forward gas prices. Must include columns `yymm`, `value`.
#' @param dt_spot_gas `data.table`. Historical daily spot gas prices. Must include `date`, `value`.
#' @param model_gas List with model parameters for gas forecasting. Defaults to `ENV_MODELS_GAS`.
#' @param model_pwr List with model parameters for power forecasting. Defaults to `ENV_MODELS_PWR`.
#'
#' @return A `data.table` with shaped hourly power forecasts, including:
#' \describe{
#'   \item{date}{Date of forecasted value}
#'   \item{hour}{Hour of the day (1–24)}
#'   \item{forecast}{Forecasted power price}
#'   \item{name}{Scenario label}
#' }
#' @import data.table
#' @export
apply_shape = function(
        country = 'Italy',
        name = 'HPFC',
        start_date = '2024-01-01', 
        end_date = '2024-12-31',
        dt_fwd_pwr,
        dt_spot_pwr,
        dt_fwd_gas,
        dt_spot_gas,
        model_gas = ENV_MODELS_GAS,
        model_pwr = ENV_MODELS_PWR
) {
    
    spot_pwr_code = unique(dt_spot_pwr$RIC)
    spot_gas_code = 'TTFDA'
    fwd_pwr_code = unique(eikondata::pwr_products_full[countries == country]$products_PWR_code)
    fwd_gas_code = 'TFMB'
    
    last_date = as.Date(start_date) - 1
    calendar_holidays = as.data.table(eikondata::new_calendar_holidays)
    setnames(calendar_holidays, paste0("holiday_", country), 'holiday', skip_absent = TRUE)
    calendar_future = calendar_holidays[, .(date, holiday)]
    calendar_future[,`:=` (year = as.character(data.table::year(date)), 
                           quarter = as.character(data.table::quarter(date)),
                           month = as.character(data.table::month(date)))
    ]
    calendar_future = calendar_future[date >= start_date & date <= end_date]
    time_range = as.numeric(data.table::year(as.Date(start_date))):as.numeric(data.table::year(as.Date(end_date)))
    
    dt_fwd_prep_pwr = merge(dt_fwd_pwr, generate_monthrics_pwr(country, time_range = year(start_date):year(end_date)), by.x = 'yymm', by.y ='date', all.x = TRUE) 
    dt_fwd_prep_gas = merge(dt_fwd_gas, generate_monthrics_gas(fwd_gas_code, time_range = year(start_date):year(end_date)), by.x = 'yymm', by.y ='date', all.x = TRUE) 
    
    dt_fwds = rbind(dt_fwd_prep_pwr, dt_fwd_prep_gas)
    dt_fwds[, sim := NULL]
    colnames(dt_fwds) = c('date', 'value', 'RIC')
    
    LST_FOR = list(
        model_lt_gas = copy(model_gas$dt_lt_param_gasdep),
        model_lt_pwr = copy(model_pwr$dt_lt_param_pwr),
        model_st_pwr = copy(model_pwr$lst_hr_param_pwr),
        dt_fwds = copy(dt_fwds),
        saved_history_gas = dt_spot_gas,
        saved_history_pwr = dt_spot_pwr,
        ric_spot_gas = spot_gas_code,
        ric_fwd_gas = fwd_gas_code,
        ric_spot_pwr = spot_pwr_code,
        ric_fwd_pwr = fwd_pwr_code,
        calendar_forecast = calendar_future,
        start_date = start_date,
        end_date = end_date,
        last_date = last_date
    ) 
    
    ENV_FOR_GAS = forecast_gas(input_forecast = LST_FOR)
    ENV_FOR_PWR = forecast_pwr(input_forecast = LST_FOR, gas_forecast = ENV_FOR_GAS)
    
    dt_pwr = ENV_FOR_PWR[, .(date, hour, forecast = final_forecast)]
    setcolorder(dt_pwr, c('date', 'hour',  'forecast'))
    setorder(dt_pwr, date, hour)
    dt_pwr[, name := name]
    
    return(dt_pwr)
    
}