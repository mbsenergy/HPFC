
## Library ----------------------------------------
library(data.table)
library(HPFC)
library(eikondata)

commodity_main = 'Italy'
commodity_basket = c('Germany', 'Spain', 'C02', 'TTF')

## Generate continuation codes ----------------------------------------
list_pwr_codes = eikondata::pwr_products_full
list_pwr_codes[, c1 := paste0(products_PWR_code, 'BYc1')]
list_pwr_codes[, c2 := paste0(products_PWR_code, 'BYc2')]
list_pwr_codes = list_pwr_codes[, .(COMMODITY = countries, c1, c2)]

list_gas_codes = eikondata::gas_products_full
list_gas_codes[, c1 := paste0(products_GAS_code, 'Yc1')]
list_gas_codes[, c2 := paste0(products_GAS_code, 'Yc2')]
list_gas_codes = list_gas_codes[, .(COMMODITY = products_GAS, c1, c2)]

list_co2_codes = data.table(COMMODITY = 'C02', 
                            c1 = 'CFI2Zc1',
                            c2 = 'CFI2Zc2')

list_cont_codes = rbind(list_pwr_codes, list_gas_codes, list_co2_codes)
list_codes_cont = c(list_pwr_codes$c1, list_pwr_codes$c2, list_gas_codes$c1, list_gas_codes$c2, list_co2_codes$c1, list_co2_codes$c2)
rm(list_pwr_codes, list_gas_codes, list_co2_codes)


## Get data  ----------------------------------------
eikondata::set_proxy_port(9000L)
eikondata::set_app_id(Sys.getenv('REUTERS_KEY'))

list_cont = lapply(list_codes_cont, function(x) {
    tryCatch({
        print(x)
        DT = eikondata::get_timeseries(
            rics = x, 
            fields = c("TIMESTAMP", "CLOSE", "VOLUME"),
            start_date = '2016-01-01T00:00:00',
            end_date = '2025-03-31T00:00:00',
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
dt_cont[, VALUE := as.numeric(VALUE)]
dt_cont[, VOLUME := as.numeric(VOLUME)]

dt_cont = merge(melt(list_cont_codes, id.vars = 'COMMODITY', variable.name = 'TYPE', value.name = 'RIC'), dt_cont, by = 'RIC', all.y = TRUE)

DT = dt_cont
mk_comm_0 = commodity_main
basket = commodity_basket

# xlsx_path = file.path('inst', 'autobasket', 'market_data_sample.xlsx')
# obj = basket_selection(DT = DATA_4, mk_comm_0 = commodity_main, full = TRUE)


basket_selection = function(DT, mk_comm_0, basket = NULL, preview = FALSE, full = FALSE) {
    
    dt_series_contcal = DT[COMMODITY %in% c(mk_comm_0, basket)]
    dt_series_contcal = unique(dt_series_contcal)
    
    dt_contcal_curves = 
        dt_series_contcal[TYPE == 'c1', .(DATE, VALUE, COMMODITY)] %>% 
        dcast(DATE ~ COMMODITY, value.var = c('VALUE')) 
    
    ### Prepare data for automatic-basis basket  
    
    dt_contcal_curves_model = copy(dt_contcal_curves)
    dt_contcal_curves_model[, (names(dt_contcal_curves_model)[-1]) := lapply(.SD, function(x) {nafill(x, type = 'nocb')}), .SDcols = names(dt_contcal_curves_model)[-1]]
    dt_contcal_curves_model = dt_contcal_curves_model[complete.cases(dt_contcal_curves_model)]
    
    
    ### Split into TRAIN-TEST sets
    dt_contcal_curves_model = dt_contcal_curves_model %>% as.data.frame()
    latest_date = max(dt_contcal_curves_model$DATE)
    
    split_date = as.Date(latest_date) - months(6)
    if (is.na(split_date)) {
        split_date = as.Date(latest_date) - months(7)
    }    
    dt_contcal_curves_train = dt_contcal_curves_model[dt_contcal_curves_model$DATE <= split_date, ]
    
    x = glmnet::makeX(dt_contcal_curves_train[,!(names(dt_contcal_curves_train) %in% c('DATE', mk_comm_0))],
                      na.impute = TRUE)
    
    # y = dt_contcal_curves_train$mk_comm_0 
    y = dt_contcal_curves_train[,(names(dt_contcal_curves_train) %in% c(mk_comm_0))]
    
    
    ### Model Parameters
    weights_vector = seq(1, nrow(x)) / max(seq(1, nrow(x)))
    lambda_seq = 10^seq(10, -2, length = 100)
    
    
    ## Model GLM - Weighted - Lasso
    model = glmnet::cv.glmnet(x = x,
                              y = y,
                              alpha = 0,
                              lambda = lambda_seq, 
                              lower.limits = 0,
                              weights = weights_vector,
                              intercept = FALSE)
    
    
    ### Model Perfomance (Train)
    best_model_index = which.min(model$cvm)
    coef_glm = coef(model, s = model$lambda[best_model_index])
    
    coef_glm = data.frame(COMMODITY = coef(model)@Dimnames[[1]][coef(model)@i + 1], coeff = coef(model)@x)
    coef_glm =  merge(coef_glm, unique(dt_series_contcal[TYPE == 'c1', .(RIC, COMMODITY)]), all.x = TRUE, by = 'COMMODITY')
    
    predictions = predict(model, newx = x, s = "lambda.min", bestmodel = TRUE)
    residuals = y - predictions
    mse = mean(residuals ^ 2)
    
    var_y = var(y)
    
    R2 = 1 - mse / var_y
    
    sd_residuals = sd(residuals)
    
    # Backtesting & Definitive Performance Metrics
    dt_contcal_curves_test = copy(dt_contcal_curves_model)
    x_test = as.matrix(dt_contcal_curves_test[, !(names(dt_contcal_curves_train) %in% c('DATE', mk_comm_0))])
    
    predictions_test = as.vector(predict(model, newx = x_test, s = "lambda.min", bestmodel = TRUE))
    residuals_test = dt_contcal_curves_train[,(names(dt_contcal_curves_train) %in% c(mk_comm_0))] - predictions_test
    
    dt_contcal_curves_test$residuals = as.vector(residuals_test)
    basis_residuals = as.vector(residuals_test)
    
    dt_contcal_curves_test = as.data.table(dt_contcal_curves_test)
    setnames(dt_contcal_curves_test, old = mk_comm_0, new = 'mk_comm_0')
    
    
    
    ## Model performance
    mse_test = mean(residuals_test ^ 2)
    mae_test = mean(abs(residuals_test))
    mse_test
    
    R2_test = 1 - mse_test / var_y
    R2_test
    
    
    # Standard deviation of residuals
    sd_residuals_test = sd(residuals_test)
    sd_residuals_test
    
    
    
    ## Plot for Model outcome control
    setDT(dt_contcal_curves_test)
    dt_contcal_curves_test[, basis := predictions_test]
    dt_contcal_curves_test[, DELTA := mk_comm_0 - basis]
    dt_contcal_curves_test[, SET := fifelse(DATE <= split_date, 'TRAINING', 'VALIDATION')]
    
    
    
    ## PRINT MODEL RESULTS COMPARED TO COMMODITY 0
    
    coef_glm$weight = coef_glm$coeff / sum(coef_glm$coeff, na.rm = TRUE)
    
    coef_glm
    R2_test
    mae_test
    sd_residuals_test
    
    dt_contcal_curves_test$SET = NULL
    dt_contcal_curves_test$residuals = NULL
    
    if(isTRUE(full)) {
        
        list_basis_basket = list(coef_glm, R2_test, mse_test, mae_test, sd_residuals_test, dt_contcal_curves_test)
        names(list_basis_basket) = c('coef_glm', 'R2_test', 'mse_test', 'mae_test', 'sd_residuals_test', 'dt_contcal_curves_test') 
        
    } else {
        
        list_basis_basket = list(coef_glm, R2_test, mse_test, mae_test, sd_residuals_test)
        names(list_basis_basket) = c('coef_glm', 'R2_test', 'mse_test', 'mae_test', 'sd_residuals_test')    
        
    }
    
    return(list_basis_basket)
    
}

list_autobasket = basket_selection(DT = dt_cont, mk_comm_0 = commodity_main, basket = commodity_basket)
