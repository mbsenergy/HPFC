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
#' @return A `data.table` with `time_index` and simulated price columns.
simulate_random_walk = function(S0, sigma, Td, N) {
    paths_list = lapply(1:N, function(i) simulate_single_path(S0, sigma, Td, N = i))
    merged_paths = Reduce(function(x, y) merge(x, y, by="time_index", all=TRUE), paths_list)
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
    
    if(!is.null(aux_sigma)) {
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
