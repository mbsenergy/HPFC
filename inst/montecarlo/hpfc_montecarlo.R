
# PREPARE -----------------------------------------
box::use(
    data.table[...],
    magrittr[...],
    echarts4r[...],
    reactable[...],
    # HPFC[...],
    eikondata[...],
    openxlsx[...]
)

devtools::load_all()

# INPUTS
STARTING_PRICE = 100
YEARS = 2
N_SIMS = 20
OVERRIDE_SIGMA = 'NO'
AUX_SIGMA = 2

START_DATE = '2024-01-01'
END_DATE = '2024-12-31'

in_aux_sigma = if (toupper(OVERRIDE_SIGMA) == "NO") 0 else AUX_SIGMA


MAIN_CURVE = 'Greece'


## sim path
sim_path = file.path('inst', 'montecarlo','mc_run')

## Spot curve
dt_spot_pwr = fread(file.path(sim_path, 'history', 'history_pwr.csv'))
dt_spot_pwr = dt_spot_pwr[date >= START_DATE]

## fwd curve
dt_fwd_pwr = fread(file.path(sim_path, 'fwds', 'base_fwd_curve.csv'))
dt_fwd_gas = fread(file.path(sim_path, 'fwds', 'base_fwd_curve_gas.csv'))

## Load models
ENV_MODELS_GAS = readRDS(file.path(sim_path, 'models', 'ENV_MODELS_GAS.rds'))
ENV_MODELS_PWR = readRDS(file.path(sim_path, 'models', 'ENV_MODELS_PWR.rds'))



# SIMULATE PATHS -----------------------------------
sim_result = montecarlo_sim(
    S0 = STARTING_PRICE,
    Td = 365 * YEARS,
    N = N_SIMS,
    dt_spot_pwr = dt_spot_pwr,
    dt_fwd_pwr = dt_fwd_pwr,
    aux_sigma = in_aux_sigma
)


### VISUALIZE PATHS 
sim_result |>
    group_by(sim) |>
    e_charts(yymm) |>
    e_line(value, symbol = 'none') |>
    e_title("Monte Carlo Simulation of a Random Walk") |>
    e_x_axis(name = "Time (Months)") |>
    e_y_axis(name = "Price") |>
    e_tooltip(trigger = "axis") |> 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0)  |>
    e_legend(orient = "vertical", right = 0) |>
    e_theme("westeros")  



# PREPARE FOR SHAPING ---------------------------------------------
dt_fwd_prep_pwr = merge(dt_fwd_pwr, generate_monthrics_pwr(MAIN_CURVE, time_range = 2024), by.x = 'yymm', by.y ='date', all.x = TRUE) 
dt_fwd_prep_gas = merge(dt_fwd_gas, generate_monthrics_gas('TFMB', time_range = 2024), by.x = 'yymm', by.y ='date', all.x = TRUE) 
dt_fwds = rbind(dt_fwd_prep_pwr, dt_fwd_prep_gas)
dt_fwds[, sim := NULL]
colnames(dt_fwds) = c('date', 'value', 'RIC')

list_inputs_fwd = prepare_fwd(
    fwd_pwr_code = MAIN_CURVE,
    fwd_gas_code = 'TTF',
    start_date = min(dt_fwd_pwr$yymm, na.rm = TRUE),
    end_date = max(dt_fwd_pwr$yymm, na.rm = TRUE),
    model_type = 'PWR',
    forecast_source = 'FWD',
    archive = 'NO',
    manual_pwr = dt_fwd_pwr,
    manual_gas = dt_fwd_gas
)


## APPLY TO REALIZED FWD -----------
DT_FWD = apply_shape(
    country = MAIN_CURVE,
    name = 'FWD',
    start_date = '2024-01-01',
    end_date = '2024-12-31',
    dt_fwd_pwr = dt_fwd_pwr,
    dt_spot_pwr = dt_spot_pwr,
    dt_fwd_gas = dt_fwd_gas,
    dt_spot_gas = dt_spot_gas,
    model_gas = ENV_MODELS_GAS,
    model_pwr = ENV_MODELS_PWR
)

## APPLY HPFC FWD PATHS ----------------------
# Get the unique simulations from the sim_result
vec_sims = unique(sim_result$sim)

# Apply the function to each unique simulation
results = lapply(vec_sims, function(sim_id) {
    # Filter the data for the current simulation
    dt_fwd_pwr_filtered = sim_result[sim == sim_id]
    
    # Call the apply_shape function with the necessary parameters
    DTS = apply_shape(
        country = MAIN_CURVE,
        name = sim_id,
        start_date = '2024-01-01',
        end_date = '2024-12-31',
        dt_fwd_pwr = dt_fwd_pwr_filtered,
        dt_spot_pwr = dt_spot_pwr,
        dt_fwd_gas = dt_fwd_gas,
        dt_spot_gas = dt_spot_gas,
        model_gas = ENV_MODELS_GAS,
        model_pwr = ENV_MODELS_PWR
    )
    
    cat(crayon::cyan$bold(paste0("✔ Simulation ", sim_id, " completed\n")))
    
    # Return the result for this simulation
    return(DTS)
    
})

DT_HPFC_MC = rbindlist(results)

### VISUALIZE HPFC's
DT_HPFC_MC[, .(value = mean(forecast, na.rm = TRUE)), by = .(yymm = date, sim = name)] |>
    group_by(sim) |>
    e_charts(yymm) |>
    e_line(value, symbol = 'none') |>
    e_title("HPFC - Monte Carlo Price paths") |>
    e_x_axis(name = "Time (Months)") |>
    e_y_axis(name = "Price") |>
    e_tooltip(trigger = "axis") |> 
    e_toolbox_feature(feature = "saveAsImage", title = "Save as image") %>% 
    e_datazoom(start = 0)  |>
    e_legend(orient = "vertical", right = 0) |>
    e_theme("westeros")



# EXPORT ------------------------------
openxlsx::write.xlsx(DT_HPFC_MC, file = file.path(sim_path, 'output', 'hpfc_mc_curves.xlsx'))
